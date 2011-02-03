{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Handler.Warp
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- A fast, light-weight HTTP server handler for WAI.
--
---------------------------------------------------------
module Network.Wai.Handler.Warp
    ( run
    , sendResponse
    , parseRequest
    , takeLineMax
    , takeHeaders
    , takeUntilBlank
    , InvalidRequest (..)
    ) where

import Prelude hiding (catch)
import Network.Wai
import qualified System.IO

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network
    ( listenOn, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Network.Socket
    ( accept, SockAddr
    )
import qualified "network-bytestring" Network.Socket.ByteString as Sock
import Control.Exception (bracket, finally, Exception, SomeException, catch)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mappend, mconcat)
import Network.Socket.SendFile (sendFile)
import Network.Socket.Enumerator (iterSocket)

import Control.Monad.IO.Class (liftIO)
import System.Timeout (timeout)
import Data.Word (Word8)
import Data.List (foldl')

run :: Port -> Application -> IO ()
run port = withSocketsDo . -- FIXME should this be called by client user instead?
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> Application -> Socket -> IO ()
serveConnections port app socket = do
    (conn, sa) <- accept socket
    _ <- forkIO $ serveConnection port app conn sa
    serveConnections port app socket

serveConnection :: Port -> Application -> Socket -> SockAddr -> IO ()
serveConnection port app conn remoteHost' = do
    catch
        (finally
          (E.run_ $ fromClient $$ serveConnection')
          (sClose conn))
        ignoreAll
  where
    ignoreAll :: SomeException -> IO ()
    ignoreAll _ = return ()
    fromClient = enumSocket bytesPerRead conn
    serveConnection' = do
        (enumeratee, env) <- parseRequest port remoteHost'
        res <- E.joinI $ enumeratee $$ app env
        keepAlive <- liftIO $ sendResponse env (httpVersion env) conn res
        if keepAlive then serveConnection' else return ()

parseRequest :: Port -> SockAddr -> E.Iteratee S.ByteString IO (E.Enumeratee ByteString ByteString IO a, Request)
parseRequest port remoteHost' = do
    headers' <- takeHeaders
    parseRequest' port headers' remoteHost'

-- FIXME come up with good values here
maxHeaders, maxHeaderLength, bytesPerRead, readTimeout :: Int
maxHeaders = 30
maxHeaderLength = 1024
bytesPerRead = 4096
readTimeout = 3000000

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | TooManyHeaders
    | IncompleteHeaders
    | OverLargeHeader
    | SocketTimeout
    deriving (Show, Typeable, Eq)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> SockAddr
              -> E.Iteratee S.ByteString IO (E.Enumeratee S.ByteString S.ByteString IO a, Request)
parseRequest' _ [] _ = E.throwError $ NotEnoughLines []
parseRequest' port (firstLine:otherLines) remoteHost' = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let rpath =
            if S.null rpath'
                then "/"
                else rpath'
    let heads = map parseHeaderNoAttr otherLines
    let host = fromMaybe "" $ lookup "host" heads
    let len =
            case lookup "content-length" heads of
                Nothing -> 0
                Just bs ->
                    case reads $ B.unpack bs of -- FIXME could probably be optimized
                        (x, _):_ -> x
                        [] -> 0
    let serverName' = takeUntil 58 host -- ':'
    -- FIXME isolate takes an Integer instead of Int or Int64. If this is a
    -- performance penalty, we may need our own version.
    return (EB.isolate len, Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = remoteHost'
                })


takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> E.Iteratee S.ByteString IO (ByteString, ByteString, ByteString, HttpVersion)
parseFirst s = do
    let pieces = S.split 32 s  -- ' '
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> E.throwError $ BadFirstLine $ B.unpack s
    let (hfirst, hsecond) = B.splitAt 5 http'
    if (hfirst == "HTTP/")
        then
            let (rpath, qstring) = B.break (== '?') query
             in return (method, rpath, qstring, hsecond)
        else E.throwError NonHttp
{-# INLINE parseFirst #-} -- FIXME is this inline necessary? the function is only called from one place and not exported

httpBuilder, spaceBuilder, newlineBuilder, transferEncodingBuilder
           , colonSpaceBuilder :: Builder
httpBuilder = copyByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = copyByteString "\r\n"
transferEncodingBuilder = copyByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = copyByteString ": "

headers :: HttpVersion -> Status -> ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked' = {-# SCC "headers" #-}
    let !start = httpBuilder
                `mappend` copyByteString httpversion
                `mappend` spaceBuilder
                `mappend` fromShow (statusCode status)
                `mappend` spaceBuilder
                `mappend` copyByteString (statusMessage status)
                `mappend` newlineBuilder
        !start' = foldl' responseHeaderToBuilder start responseHeaders
        !end = if isChunked'
                 then transferEncodingBuilder
                 else newlineBuilder
    in start' `mappend` end

responseHeaderToBuilder :: Builder -> (CIByteString, ByteString) -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` (copyByteString $ ciOriginal x)
  `mappend` colonSpaceBuilder
  `mappend` copyByteString y
  `mappend` newlineBuilder

isChunked :: HttpVersion -> Bool
isChunked = (==) http11

hasBody :: Status -> Request -> Bool
hasBody s req = s /= (Status 204 "") && requestMethod req /= "HEAD"

sendResponse :: Request -> HttpVersion -> Socket -> Response -> IO Bool
sendResponse req hv socket (ResponseFile s hs fp) = do
    Sock.sendMany socket $ L.toChunks $ toLazyByteString $ headers hv s hs False
    if hasBody s req
        then do
            sendFile socket fp
            return $ lookup "content-length" hs /= Nothing
        else return True
sendResponse req hv socket (ResponseBuilder s hs b)
    | hasBody s req = do
          toByteStringIO (Sock.sendAll socket) b'
          return isKeepAlive
    | otherwise = do
        Sock.sendMany socket
            $ L.toChunks
            $ toLazyByteString
            $ headers hv s hs False
        return True
  where
    headers' = headers hv s hs isChunked'
    b' =
        if isChunked'
            then headers'
                 `mappend` chunkedTransferEncoding b
                 `mappend` chunkedTransferTerminator
            else headers hv s hs False `mappend` b
    hasLength = lookup "content-length" hs /= Nothing
    isChunked' = isChunked hv && not hasLength
    isKeepAlive = isChunked' || hasLength
sendResponse req hv socket (ResponseEnumerator res) =
    res go
  where
    go s hs
        | not (hasBody s req) = do
            liftIO $ Sock.sendMany socket
                   $ L.toChunks $ toLazyByteString
                   $ headers hv s hs False
            return True
    go s hs =
            chunk'
          $ E.enumList 1 [headers hv s hs isChunked']
         $$ E.joinI $ builderToByteString
         $$ (iterSocket socket >> return isKeepAlive)
      where
        hasLength = lookup "content-length" hs /= Nothing
        isChunked' = isChunked hv && not hasLength
        isKeepAlive = isChunked' || hasLength
        chunk' i =
            if isChunked'
                then E.joinI $ chunk $$ i
                else i
        chunk :: E.Enumeratee Builder Builder IO Bool
        chunk = E.checkDone $ E.continue . step
        step k E.EOF = k (E.Chunks [chunkedTransferTerminator]) >>== return
        step k (E.Chunks []) = E.continue $ step k
        step k (E.Chunks [x]) = k (E.Chunks [chunkedTransferEncoding x]) >>== chunk
        step k (E.Chunks xs) = k (E.Chunks [chunkedTransferEncoding $ mconcat xs]) >>== chunk

parseHeaderNoAttr :: ByteString -> (CIByteString, ByteString)
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                    then SU.unsafeDrop 2 rest
                    else rest
     in (mkCIByteString k, rest')

-- FIXME when we switch to Jeremy's timeout code, we can probably start using
-- network-enumerator's enumSocket
enumSocket :: Int -> Socket -> E.Enumerator ByteString IO a
enumSocket len socket (E.Continue k) = do
#if NO_TIMEOUT_PROTECTION
    bs <- liftIO $ Sock.recv socket len
    go bs
#else
    mbs <- liftIO $ timeout readTimeout $ Sock.recv socket len
    case mbs of
        Nothing -> E.throwError SocketTimeout
        Just bs -> go bs
#endif
  where
    go bs
        | S.length bs == 0 = E.continue k
        | otherwise = k (E.Chunks [bs]) >>== enumSocket len socket
enumSocket _ _ step = E.returnI step

------ The functions below are not warp-specific and could be split out into a
--separate package.

takeUntilBlank :: Int
               -> ([ByteString] -> [ByteString])
               -> E.Iteratee S.ByteString IO [ByteString]
takeUntilBlank count _
    | count > maxHeaders = E.throwError TooManyHeaders
takeUntilBlank count front = do
    l <- takeLineMax 0 id
    if B.null l
        then return $ front []
        else takeUntilBlank (count + 1) $ front . (:) l

takeLineMax :: Int
            -> ([ByteString] -> [ByteString])
            -> E.Iteratee ByteString IO ByteString
takeLineMax len front = do
    mbs <- EL.head
    case mbs of
        Nothing -> E.throwError IncompleteHeaders
        Just bs -> do
            let (x, y) = S.breakByte 10 bs
                x' = if S.length x > 0 && S.last x == 13
                        then S.init x
                        else x
            let len' = len + B.length x
            case () of
                ()
                    | len' > maxHeaderLength -> E.throwError OverLargeHeader
                    | B.null y -> takeLineMax len' $ front . (:) x
                    | otherwise -> do
                        E.yield () $ E.Chunks [B.drop 1 y]
                        return $ B.concat $ front [x']

maxTotalHeaderLength = 50 * 1024
takeHeaders :: E.Iteratee S.ByteString IO [ByteString]
#if TAKE_UNTIL_BLANK
takeHeaders = takeUntilBlank 0 id
#else
takeHeaders = do
  !x <- forceHead
  takeHeaders' 0 id id x
#endif
{-# INLINE takeHeaders #-}

takeHeaders' :: Int
             -> ([ByteString] -> [ByteString])
             -> ([ByteString] -> [ByteString])
             -> ByteString
             -> E.Iteratee S.ByteString IO [ByteString]
takeHeaders' !len _ _ _ | len > maxTotalHeaderLength = E.throwError OverLargeHeader
takeHeaders' !len !lines !prepend !bs = do
  let !bsLen = {-# SCC "takeHeaders'.bsLen" #-} S.length bs
      !mnl = {-# SCC "takeHeaders'.mnl" #-} S.elemIndex 10 bs
  case mnl of
       -- no newline.  prepend entire bs to next line
       !Nothing -> {-# SCC "takeHeaders'.noNewline" #-} do
         let !len' = len + bsLen
         !more <- forceHead 
         takeHeaders' len' lines (prepend . (:) bs) more
       Just !nl -> {-# SCC "takeHeaders'.newline" #-} do
         let !end = nl 
             !start = nl + 1
             !line = {-# SCC "takeHeaders'.line" #-}
                     if end > 0
                        -- line data included in this chunk
                        then S.concat $! prepend [SU.unsafeTake (checkCR bs end) bs]
                        --then S.concat $! prepend [SU.unsafeTake (end-1) bs]
                        -- no line data in this chunk (all in prepend, or empty line)
                        else S.concat $! prepend []
         if S.null line
            -- no more headers
            then {-# SCC "takeHeaders'.noMoreHeaders" #-} do
              let !lines' = {-# SCC "takeHeaders'.noMoreHeaders.lines'" #-} lines []
              if start < bsLen
                 then {-# SCC "takeHeaders'.noMoreHeaders.yield" #-} do
                   let !rest = {-# SCC "takeHeaders'.noMoreHeaders.yield.rest" #-} SU.unsafeDrop start bs
                   E.yield lines' $! E.Chunks [rest]
                 else return lines'

            -- more headers
            else {-# SCC "takeHeaders'.moreHeaders" #-} do
              let !len' = len + start 
                  !lines' = {-# SCC "takeHeaders.lines'" #-} lines . (:) line
              !more <- {-# SCC "takeHeaders'.more" #-} 
                       if start < bsLen
                          then return $! SU.unsafeDrop start bs
                          else forceHead
              {-# SCC "takeHeaders'.takeMore" #-} takeHeaders' len' lines' id more
{-# INLINE takeHeaders' #-}

forceHead = do
  !mx <- E.head
  case mx of
       !Nothing -> E.throwError IncompleteHeaders
       Just !x -> return x
{-# INLINE forceHead #-}

checkCR bs pos = 
  let !p = pos - 1
  in if '\r' == B.index bs p
        then p
        else pos
{-# INLINE checkCR #-}

