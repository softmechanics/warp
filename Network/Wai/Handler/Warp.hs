{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
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
    ) where

import Prelude hiding (catch)
import Network.Wai
import qualified System.IO

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network
    ( listenOn, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Network.Socket
    ( accept
    )
import qualified Network.Socket.ByteString as Sock
import Control.Exception (bracket, finally, Exception, SomeException, catch)
import System.IO (Handle, hClose, hFlush)
import System.IO.Error (isEOFError, ioeGetHandle)
import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Control.Arrow (first)

import Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import Data.Enumerator.IO (iterHandle, enumHandle)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder (fromByteString, Builder, toLazyByteString, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Data.Monoid (mconcat)
import Network.Socket.SendFile (sendFile)

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Timeout (timeout)

run :: Port -> Application -> IO ()
run port = withSocketsDo .
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> Application -> Socket -> IO ()
serveConnections port app socket = do
    (conn, sa) <- accept socket
    let remoteHost' = stripPort $ show sa -- FIXME
    _ <- forkIO $ serveConnection port app conn remoteHost'
    serveConnections port app socket
  where
    stripPort s =
        case break (== ':') $ reverse s of
            (_, ':' : rest) -> reverse rest
            _ -> s

serveConnection :: Port -> Application -> Socket -> String -> IO ()
serveConnection port app conn remoteHost' = do
    catch
        (finally
          (E.run_ $ fromClient $$ serveConnection')
          (sClose conn))
        ignoreAll
  where
    ignoreAll :: SomeException -> IO ()
    ignoreAll e = return ()
    fromClient = enumSocket bytesPerRead conn
    serveConnection' = do
        (enumeratee, env) <- parseRequest port remoteHost'
        res <- E.joinI $ enumeratee $$ app env
        keepAlive <- liftIO $ sendResponse env (httpVersion env) conn res
        when keepAlive serveConnection'

parseRequest :: Port -> String -> E.Iteratee S.ByteString IO (E.Enumeratee ByteString ByteString IO a, Request)
parseRequest port remoteHost' = do
    headers' <- takeUntilBlank 0 id
    parseRequest' port headers' remoteHost'

-- FIXME come up with good values here
maxHeaders, maxHeaderLength, bytesPerRead, readTimeout :: Int
maxHeaders = 30
maxHeaderLength = 1024
bytesPerRead = 4096
readTimeout = 30000000

takeUntilBlank :: Int
               -> ([ByteString] -> [ByteString])
               -> E.Iteratee S.ByteString IO [ByteString]
takeUntilBlank count _
    | count > maxHeaders = E.throwError TooManyHeaders
takeUntilBlank count front = do
    l <- takeLine 0 id
    if B.null l
        then return $ front []
        else takeUntilBlank (count + 1) $ front . (:) l

{--
breakLine :: ByteString -> Maybe (ByteString, ByteString)
breakLine bs = 
  case S.elemIndex 10 bs of
       Nothing -> Nothing
       Just n  -> 
        Just (
           if n == 0
              then S.empty
              else S.unsafeTake (n-1) bs
         , if n == S.length bs
              then S.empty
              else S.unsafeDrop (n+1) bs
         )
{-# INLINE breakLine #-}

takeLine :: Int
         -> ([ByteString] -> [ByteString])
         -> E.Iteratee ByteString IO ByteString
takeLine len front = do
    mbs <- {-# SCC "takeLine.mbs" #-} E.head
    case mbs of
        Nothing -> E.throwError IncompleteHeaders
        Just bs -> do
            case breakLine bs of
                 Nothing -> do  -- no \n
                   let len' = {-# SCC "takeLine.len'" #-} len + B.length bs
                   {-# SCC "takeLine.recurse" #-} takeLine len' $ front . (:) bs
                 Just (x,y) -> do
                   let xLen = B.length x
                   {-# SCC "takeLine.maxHeaderLength" #-} when (len + xLen > maxHeaderLength) $ E.throwError OverLargeHeader
                   {-# SCC "takeLine.yield" #-} when (not $ B.null y) $ E.yield () $ E.Chunks [y]
                   {-# SCC "takeLine.concat" #-} return $ B.concat $ front [x]
--}

{--}
takeLine :: Int
         -> ([ByteString] -> [ByteString])
         -> E.Iteratee ByteString IO ByteString
takeLine len front = do
    mbs <- {-# SCC "takeLine.mbs" #-} E.head
    case mbs of
        !Nothing -> E.throwError IncompleteHeaders
        Just !bs -> do
            case S.elemIndex 10 bs of
                 !Nothing -> do  -- no \n
                   let !len' = {-# SCC "takeLine.len'" #-} len + B.length bs
                   {-# SCC "takeLine.recurse" #-} takeLine len' $! front . (:) bs
                 Just !n -> do
                   let !xLen = n - 1       -- drop \r
                       !restStart = n + 1  -- drop \n
                       !bsLen = B.length bs
                       !x = {-# SCC "takeLine.x" #-} if xLen > 0 then S.unsafeTake xLen bs else S.empty
                   {--
                   {-# SCC "takeLine.maxHeaderLength" #-} when (len + xLen > maxHeaderLength) $ E.throwError OverLargeHeader
                   {-# SCC "takeLine.yield" #-} when (restStart < bsLen) $! E.yield () $! E.Chunks [{-# SCC "takeLine.y" #-} S.unsafeDrop restStart bs]
                   {-# SCC "takeLine.concat" #-} return $ B.concat $ front [x]
                   --}
                   {--}
                   if len + xLen > maxHeaderLength
                      then E.throwError OverLargeHeader
                      else 
                        let !result = {-# SCC "takeLine.concat" #-} B.concat $! front [x]
                        in if restStart < bsLen
                              then {-# SCC "takeLine.yield" #-} E.yield result $! E.Chunks [{-# SCC "takeLine.y" #-} S.unsafeDrop restStart bs]
                              else return result
                   --}
--}

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | TooManyHeaders
    | IncompleteHeaders
    | OverLargeHeader
    | SocketTimeout
    deriving (Show, Typeable)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Port
              -> [ByteString]
              -> String
              -> E.Iteratee S.ByteString IO (E.Enumeratee S.ByteString S.ByteString IO a, Request)
parseRequest' port lines' remoteHost' = do
    (firstLine, otherLines) <-
        case lines' of
            x:xs -> return (x, xs)
            [] -> E.throwError $ NotEnoughLines $ map B.unpack lines'
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let rpath = '/' : case B.unpack rpath' of
                        ('/':x) -> x
                        _ -> B.unpack rpath'
    let heads = map (first mkCIByteString . parseHeaderNoAttr) otherLines
    let host = fromMaybe "" $ lookup "host" heads
    let len = fromMaybe 0 $ do
                bs <- lookup "Content-Length" heads
                let str = B.unpack bs
                case reads str of
                    (x, _):_ -> Just x
                    _ -> Nothing
    let (serverName', _) = B.break (== ':') host
    return (requestBodyHandle len, Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = B.pack rpath
                , queryString = gets
                , serverName = serverName'
                , serverPort = port
                , requestHeaders = heads
                , isSecure = False
                , errorHandler = System.IO.hPutStr System.IO.stderr
                , remoteHost = B.pack remoteHost'
                })

parseFirst :: ByteString
           -> E.Iteratee S.ByteString IO (ByteString, ByteString, ByteString, HttpVersion)
parseFirst s = do
    let pieces = B.words s
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> E.throwError $ BadFirstLine $ B.unpack s
    let (hfirst, hsecond) = B.splitAt 5 http'
    unless (hfirst == "HTTP/") $ E.throwError NonHttp
    let (rpath, qstring) = B.break (== '?') query
    return (method, rpath, qstring, hsecond)

headers :: HttpVersion -> Status -> ResponseHeaders -> Bool -> Builder
headers httpversion status responseHeaders isChunked' = mconcat
    [ fromByteString "HTTP/"
    , fromByteString httpversion
    , fromChar ' '
    , fromString $ show $ statusCode status
    , fromChar ' '
    , fromByteString $ statusMessage status
    , fromByteString "\r\n"
    , mconcat $ map go responseHeaders
    , if isChunked'
        then fromByteString "Transfer-Encoding: chunked\r\n\r\n"
        else fromByteString "\r\n"
    ]
  where
    go (x, y) = mconcat
        [ fromByteString $ ciOriginal x
        , fromByteString ": "
        , fromByteString y
        , fromByteString "\r\n"
        ]

isChunked :: HttpVersion -> Bool
isChunked = (==) http11

hasBody :: Status -> Request -> Bool
hasBody s req = s /= (Status 204 "") && requestMethod req /= "HEAD"

sendResponse :: Request -> HttpVersion -> Socket -> Response -> IO Bool
sendResponse req hv socket (ResponseFile s hs fp) = {-# SCC "sendResponseFile" #-} do
    Sock.sendMany socket $ L.toChunks $ toLazyByteString $ headers hv s hs False
    if hasBody s req
        then do
            sendFile socket fp
            return $ lookup "content-length" hs /= Nothing
        else return True
sendResponse req hv socket (ResponseEnumerator res) = {-# SCC "sendResponseEnumerator" #-}
    res go
  where
    go s hs
        | not (hasBody s req) = {-# SCC "sendResponseEnumerator.noBody" #-} do
            liftIO $ Sock.sendMany socket
                   $ L.toChunks $ toLazyByteString
                   $ headers hv s hs False
            return True
    go s hs = {-# SCC "sendResponseEnumerator.hasBody" #-} 
            chunk' $ 
            ({-# SCC "sendResponseEnumerator.hasBody.headers" #-} E.enumList 1 [headers hv s hs isChunked'])
         $$ ({-# SCC "sendResponseEnumerator.hasBody.iterSocket" #-} iterSocket socket >> return isKeepAlive)
      where
        hasLength = {-# SCC "sendResponseEnumerator.hasBody.hasLength" #-} lookup "content-length" hs /= Nothing
        isChunked' = {-# SCC "sendResponseEnumerator.hasBody.isChunked'" #-} isChunked hv && not hasLength
        isKeepAlive = {-# SCC "sendResponseEnumerator.hasBody.isKeepAlive" #-} isChunked' || hasLength
        chunk' i = {-# SCC "sendResponseEnumerator.hasBody.chunk'" #-}
            if isChunked'
                then E.joinI $ chunk $$ i
                else i
        chunk :: E.Enumeratee Builder Builder IO Bool
        chunk = {-# SCC "sendResponseEnumerator.hasBody.chunk" #-} E.checkDone $ E.continue . step
        step k E.EOF = {-# SCC "sendResponseEnumerator.hasBody.stepEOF" #-} k (E.Chunks [chunkedTransferTerminator]) >>== return
        step k (E.Chunks []) = {-# SCC "sendResponseEnumerator.hasBody.stepEmptyChunks" #-} E.continue $ step k
        step k (E.Chunks builders) = {-# SCC "sendResponseEnumerator.hasBody.stepChunks" #-}
            k (E.Chunks [chunked]) >>== chunk
          where
            chunked = {-# SCC "sendResponseEnumerator.hasBody.stepChunks.chunked" #-} chunkedTransferEncoding $ mconcat builders

parseHeaderNoAttr :: ByteString -> (ByteString, ByteString)
parseHeaderNoAttr s =
    let (k, rest) = B.span (/= ':') s
        rest' = if not (B.null rest) &&
                   B.head rest == ':' &&
                   not (B.null $ B.tail rest) &&
                   B.head (B.tail rest) == ' '
                    then B.drop 2 rest
                    else rest
     in (k, rest')

requestBodyHandle :: Int
                  -> E.Enumeratee ByteString ByteString IO a
requestBodyHandle initLen =
    go initLen
  where
    go 0 step = return step
    go len (E.Continue k) = do
        x <- E.head
        case x of
            Nothing -> return $ E.Continue k
            Just bs -> do
                let newlen = max 0 $ len - B.length bs
                k (E.Chunks [bs]) >>== go newlen
    go len step = do
        drain len
        return step
    drain 0 = return ()
    drain len = do
        mbs <- E.head
        case mbs of
            Nothing -> return ()
            Just bs -> do
                let newlen = len - B.length bs
                if newlen <= 0
                    then return ()
                    else drain newlen

iterSocket :: MonadIO m => Socket -> E.Iteratee Builder m ()
iterSocket socket =
    E.continue go
  where
    go E.EOF = E.yield () E.EOF
    go (E.Chunks cs) = do
      liftIO $ mapM_ (toByteStringIO $ Sock.sendAll socket) cs
      E.continue go

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
