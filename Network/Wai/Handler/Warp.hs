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
import System.IO.Unsafe
import Foreign (unsafeForeignPtrToPtr, plusPtr, minusPtr, ForeignPtr)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Blaze.ByteString.Builder.Internal as BI
import qualified Blaze.ByteString.Builder.Internal.Types as BI
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import Data.List (foldl', foldl1')
import Network
    ( listenOn, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Network.Socket
    ( accept
    , SockAddr (..)
    , inet_ntoa
    )
import qualified Network.Socket.ByteString as Sock
import Control.Exception (bracket, finally, Exception, SomeException, catch)
import System.IO (Handle, hClose, hFlush)
import System.IO.Error (isEOFError, ioeGetHandle)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

import Control.Arrow (first)

import Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import Data.Enumerator.IO (iterHandle, enumHandle)
import Blaze.ByteString.Builder.Enumerator (builderToByteString, unsafeBuilderToByteString, allocBuffer)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder (fromByteString, Builder, toLazyByteString, toByteStringIO)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Data.Monoid (mappend, mempty)
import Network.Socket.SendFile (sendFile)

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Timeout (timeout)
import Numeric (showHex)

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
    remoteHost' <- socketHost sa
    _ <- forkIO $ serveConnection port app conn remoteHost'
    serveConnections port app socket
  where
    socketHost (SockAddrInet _ host) = do
      chost <- inet_ntoa host
      return $ B.pack chost
    socketHost sa = 
      let s = show sa
      in return $ B.pack $
         case break (== ':') $ reverse s of
              (_, ':' : rest) -> reverse rest
              _ -> s

serveConnection :: Port -> Application -> Socket -> ByteString -> IO ()
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
        if keepAlive 
           then serveConnection'
           else return ()

parseRequest :: Port -> ByteString -> E.Iteratee S.ByteString IO (E.Enumeratee ByteString ByteString IO a, Request)
parseRequest port remoteHost' = do
    headers' <- takeHeaders
    parseRequest' port headers' remoteHost'

-- FIXME come up with good values here
maxHeaders, maxHeaderLength, bytesPerRead, readTimeout :: Int
maxHeaders = 30
maxHeaderLength = 1024
bytesPerRead = 4096
readTimeout = 30000000

takeHeaders :: E.Iteratee S.ByteString IO [ByteString]
takeHeaders = do
  !x <- forceHead
  takeHeaders' 0 id 0 id x
{-# INLINE takeHeaders #-}

takeHeaders' :: Int
           -> ([ByteString] -> [ByteString])
           -> Int
           -> ([ByteString] -> [ByteString])
           -> ByteString
           -> E.Iteratee S.ByteString IO [ByteString]
takeHeaders' !n !lines !lineLen !prepend !bs = do
  let !bsLen = {-# SCC "takeHeaders'.bsLen" #-} S.length bs
      !mnl = {-# SCC "takeHeaders'.mnl" #-} S.elemIndex 10 bs
  case mnl of
       -- no newline.  prepend entire bs to next line
       !Nothing -> {-# SCC "takeHeaders'.noNewline" #-} do
         let !lineLen' = lineLen + bsLen
         if {-# SCC "takeHeaders'.checkMaxHeaderLength" #-} lineLen' > maxHeaderLength 
            then E.throwError OverLargeHeader
            else do 
              !more <- forceHead 
              takeHeaders' n lines lineLen' (prepend . (:) bs) more
       Just !nl -> {-# SCC "takeHeaders'.newline" #-} do
         let !end = nl - 1
             !start = nl + 1
             !line = {-# SCC "takeHeaders'.line" #-}
                     if end > 0
                        -- line data included in this chunk
                        then S.concat $! prepend [S.unsafeTake end bs]
                        -- no line data in this chunk (all in prepend, or empty line)
                        else S.concat $! prepend []
         if S.null line
            -- no more headers
            then {-# SCC "takeHeaders'.noMoreHeaders" #-} do
              let !lines' = {-# SCC "takeHeaders'.noMoreHeaders.lines'" #-} lines []
              if start < bsLen
                 then {-# SCC "takeHeaders'.noMoreHeaders.yield" #-} do
                   let !rest = {-# SCC "takeHeaders'.noMoreHeaders.yield.rest" #-} S.unsafeDrop start bs
                   E.yield lines' $! E.Chunks [rest]
                 else return lines'

            -- more headers
            else {-# SCC "takeHeaders'.moreHeaders" #-} do
              let !n' = n + 1
              if {-# SCC "takeHeaders'.checkMaxHeaders" #-} n' > maxHeaders 
                 then E.throwError TooManyHeaders
                 else do
                   let !lines' = {-# SCC "takeHeaders.lines'" #-} lines . (:) line
                   !more <- {-# SCC "takeHeaders'.more" #-} 
                            if start < bsLen
                               then return $! S.unsafeDrop start bs
                               else forceHead
                   {-# SCC "takeHeaders'.takeMore" #-} takeHeaders' n' lines' 0 id more

forceHead = do
  !mx <- E.head
  case mx of
       !Nothing -> E.throwError IncompleteHeaders
       Just !x -> return x
{-# INLINE forceHead #-}

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
              -> ByteString
              -> E.Iteratee S.ByteString IO (E.Enumeratee S.ByteString S.ByteString IO a, Request)
parseRequest' _ [] _ = E.throwError $ NotEnoughLines []
parseRequest' port (firstLine:otherLines) remoteHost' = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let rpath = {-# SCC "parseRequest'.rpath" #-} 
                if B.null rpath'
                   then "/"
                   else if '/' == B.head rpath'
                           then rpath'
                           else B.cons '/' rpath'
    let heads = {-# SCC "parseRequest'.heads" #-} map parseHeaderNoAttr otherLines
    let host = {-# SCC "parseRequest'.host" #-} fromMaybe "" $ lookup "host" heads
    let len = {-# SCC "parseRequest'.len" #-}
              case lookup "Content-Length" heads of
                   Nothing -> 0
                   Just bs ->
                     let str = B.unpack bs
                     in case reads str of
                             (x, _):_ -> x
                             _ -> 0
    let serverName' = {-# SCC "parseRequest'.serverName'" #-} takeUntil 58 host  -- ':'
    return (requestBodyHandle len, Request
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
       Just !idx -> S.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> E.Iteratee S.ByteString IO (ByteString, ByteString, ByteString, HttpVersion)
parseFirst s = do
    let !pieces = {-# SCC "parseFirst.pieces" #-} S.split 32 s  -- ' '
    case pieces of
         [method, query, http'] 
           | B.isPrefixOf "HTTP/" http' -> do
             let !httpVersion = {-# SCC "parseFirst.httpVersion" #-} S.unsafeDrop 5 http'
                 (!rpath, !qstring) = {-# SCC "parseFirst.(rpath,qstring)" #-} S.breakByte 63 query  -- '?'
             return (method, rpath, qstring, httpVersion)
           | otherwise -> E.throwError NonHttp
         _ -> E.throwError $ BadFirstLine $ B.unpack s
{-# INLINE parseFirst #-}

{--}
httpBuilder = fromByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = fromByteString "\r\n"
transferEncodingBuilder = fromByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = fromByteString ": "

headers :: HttpVersion -> Status -> ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked' = {-# SCC "headers" #-}
    let !start = httpBuilder
                `mappend` fromByteString httpversion
                `mappend` spaceBuilder
                `mappend` (fromString $ show $ statusCode status)
                `mappend` spaceBuilder
                `mappend` (fromByteString $ statusMessage status)
                `mappend` newlineBuilder
        !start' = foldl' responseHeaderToBuilder start responseHeaders
        !end = if isChunked'
                 then transferEncodingBuilder
                 else newlineBuilder
    in mappend start' end

responseHeaderToBuilder :: Builder -> (CIByteString, ByteString) -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` (fromByteString $ ciOriginal x)
  `mappend` colonSpaceBuilder
  `mappend` fromByteString y
  `mappend` newlineBuilder
--}

{--
headers :: HttpVersion -> Status -> ResponseHeaders -> Bool -> Builder
headers httpversion status responseHeaders isChunked' = {-# SCC "headers" #-}
    let !start = fromByteString "HTTP/"
                `mappend` fromByteString httpversion
                `mappend` fromChar ' '
                `mappend` (fromString $ show $ statusCode status)
                `mappend` fromChar ' '
                `mappend` (fromByteString $ statusMessage status)
                `mappend` fromByteString "\r\n"
        !start' = foldl' responseHeaderToBuilder start responseHeaders
        !end = if isChunked'
                 then fromByteString "Transfer-Encoding: chunked\r\n\r\n"
                 else fromByteString "\r\n"
    in mappend start' end
    where
      responseHeaderToBuilder :: Builder -> (CIByteString, ByteString) -> Builder
      responseHeaderToBuilder b (x, y) = b
        `mappend` (fromByteString $ ciOriginal x)
        `mappend` fromByteString ": "
        `mappend` fromByteString y
        `mappend` fromByteString "\r\n"
--}

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
            {-# SCC "sendResponseEnumerator.noBody.headers" #-} E.yield 0 $ E.Chunks [headers hv s hs False] 
            {-# SCC "sendResponseEnumerator.noBody.iterSocket" #-} iterSocket socket
            return True
    go s hs = {-# SCC "sendResponseEnumerator.hasBody" #-} 
#if ITER_SOCKET_ITER_BUILDER
-- iterBuilder has "inlined" chunking
            do
              iterBuilder (Sock.sendMany socket) iterBuilderChunked $ headers hv s hs isChunked'
#else
            chunk' $ do
              {-# SCC "sendResponseEnumerator.hasBody.headers" #-} E.yield 0 $ E.Chunks [headers hv s hs isChunked'] 
              {-# SCC "sendResponseEnumerator.hasBody.iterSocket" #-} iterSocket socket
#endif
              return isKeepAlive
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
            chunked = {-# SCC "sendResponseEnumerator.hasBody.stepChunks.chunked" #-} chunkedTransferEncoding $ foldl1' mappend builders

parseHeaderNoAttr :: ByteString -> (CIByteString, ByteString)
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s  -- ':'
        restLen = {-# SCC "parseHeaderNoAttr.restLen" #-} S.length rest
        rest' = {-# SCC "parseHeaderNoAttr.rest'" #-} 
                if restLen > 1 && S.unsafeTake 2 rest == ": "
                   then S.unsafeDrop 2 rest
                   else rest
     in (mkCIByteString k, rest')

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

iterBuilder :: MonadIO m => ([ByteString] -> IO ()) -> Bool -> Builder -> E.Iteratee Builder m ()
iterBuilder = iterBuilderWith BI.defaultBufferSize 
{-# INLINE iterBuilder #-}

finalStep :: BI.BufRange -> IO (BI.BuildSignal ())
finalStep !(BI.BufRange pf _) = return $ BI.Done pf ()
{-# INLINE finalStep #-}

{-# INLINE iterBuilderWith #-}
iterBuilderWith :: MonadIO m 
                => Int 
                -> ([ByteString] -> IO ()) 
                -> Bool
                -> Builder
                -> E.Iteratee Builder m ()
iterBuilderWith !bufSize' io chunked headersBuilder = E.continue (createBufferI True bufSize')
  -- FIXME: if not chunked, send add headers to the buf (instead of toLazyByteString)
  where
    {-# INLINE addHeaders #-}
    addHeaders True bufs = (L.toChunks $ toLazyByteString headersBuilder) ++ bufs
    addHeaders _ bufs = bufs

    {-# INLINE chunkBufs #-}
    chunkBufs | chunked = iterBuilderChunker 
              | otherwise = id

    {-# INLINE endEOF #-}
    endEOF | chunked = iterBuilderOnEOF
           | otherwise = []

    {-# INLINE createBufferI #-}
    createBufferI :: MonadIO m => Bool -> Int -> E.Stream Builder -> E.Iteratee Builder m ()
    createBufferI !first !bufSize !E.EOF = E.continue (createBufferI first bufSize)
    createBufferI !first !bufSize (E.Chunks [BI.Builder !b]) = 
      createBuffer first bufSize (b (BI.buildStep finalStep))

    {-# INLINE reuseBufferI #-}
    reuseBufferI :: MonadIO m => Bool -> Int -> ForeignPtr Word8 -> Int -> E.Stream Builder -> E.Iteratee Builder m ()
    reuseBufferI !first !bufSize !fpbuf !offset E.EOF = do
      let !bufs = addHeaders first $ 
                  if chunked
                     then iterBuilderChunker [S.PS fpbuf 0 offset] ++ endEOF
                     else [S.PS fpbuf 0 offset]
      liftIO $ io $ bufs
      E.yield () E.EOF
    reuseBufferI !first !bufSize !fpbuf !offset (E.Chunks [BI.Builder !b]) = 
      fillBuffer first bufSize fpbuf offset (b (BI.buildStep finalStep))

    {-# INLINE createBuffer #-}
    createBuffer :: MonadIO m => Bool -> Int -> BI.BuildStep () -> E.Iteratee Builder m ()
    createBuffer !first !bufSize !b = do
      let fpbuf = S.inlinePerformIO $ S.mallocByteString bufSize
      fillBuffer first bufSize fpbuf 0 b

    {-# INLINE fillBuffer #-}
    fillBuffer !first !bufSize !fpbuf !offset !step = do
      let !pf = unsafeForeignPtrToPtr fpbuf
          !br = BI.BufRange (pf `plusPtr` offset) (pf `plusPtr` bufSize)
          -- safe due to later reference of fpbuf
          -- BETTER than withForeignPtr, as we lose a tail call otherwise
          !signal = S.inlinePerformIO $ BI.runBuildStep step br
      case signal of
           BI.Done !pf' _ -> do
             --liftIO $ print "Done"
             E.continue $ reuseBufferI first bufSize fpbuf (pf' `minusPtr` pf)
           
           BI.BufferFull !minSize !pf' !nextStep  -> do
             let !bufs = addHeaders first $ chunkBufs [S.PS fpbuf 0 (pf' `minusPtr` pf)]
             liftIO $ io bufs
             if minSize > bufSize
                -- alloc larger buffer
                then createBuffer False minSize nextStep
                -- reuse current buffer
                else fillBuffer False bufSize fpbuf 0 nextStep
                  
           BI.InsertByteString !pf' !bs !nextStep  -> do
             let !end = if S.null bs
                          then []
                          else [bs]
                 !bufs = addHeaders first $ chunkBufs $ S.PS fpbuf 0 (pf' `minusPtr` pf) : end
             liftIO $ io bufs
             fillBuffer False bufSize fpbuf 0 nextStep
             

-- extra data to send on EOF (probably a better way to do this...)
iterBuilderOnEOF :: [ByteString]
iterBuilderOnEOF = ["\r\n0\r\n\r\n"]

iterBuilderChunked :: Bool
#if CHUNKED_RESPONSE
iterBuilderChunked = True
#else
iterBuilderChunked = False
#endif

iterBuilderChunker :: [ByteString] -> [ByteString]
iterBuilderChunker bufs = c:bufs
  where c = B.pack $ showHex len "\r\n"
        len = foldl' (\l s -> l + B.length s) 0 bufs
{-# INLINE iterBuilderChunker #-}



iterSocket :: MonadIO m => Socket -> E.Iteratee Builder m ()
#if ITER_SOCKET_ITER_BUILDER
--iterSocket socket = iterBuilder (Sock.sendMany socket) iterBuilderChunked
iterSocket = undefined
#elif ITER_SOCKET_LBS
iterSocket = iterSocketLBS
#elif ITER_SOCKET_BUILDER
iterSocket = iterSocketBuilder
#else
iterSocket = iterSocketByteString
#endif
{-# INLINE iterSocket #-}

minWrite=8*1024

iterSocketLBS :: MonadIO m => Socket -> E.Iteratee Builder m ()
iterSocketLBS socket =
    E.continue $ go 
  where
    {-# INLINE go #-}
    go E.EOF = E.yield () E.EOF
    go (E.Chunks cs) = goChunks cs
    {-# INLINE go' #-}
    go' !len !toChunks E.EOF = do
      sendChunks $ toChunks []
      E.yield () E.EOF
    go' !len !toChunks (E.Chunks cs) = goChunks' len toChunks cs
    
    {-# INLINE goChunks #-}
    goChunks [] = E.continue $ go
    goChunks (c:cs) = do
      let !lbs = toLazyByteString c
          !len = L.length lbs
          !chunks = L.toChunks lbs
      if len < minWrite
         then goChunks' len ((++) chunks) cs
         else do
           sendChunks chunks
           goChunks cs

    {-# INLINE goChunks' #-}
    goChunks' !len !toChunks [] = E.continue $ go' len toChunks
    goChunks' !len !toChunks (c:cs) = do
      let !lbs = toLazyByteString c
          !len' = len + L.length lbs
          !chunks = L.toChunks lbs 
      if len' < minWrite
         then goChunks' len' (toChunks . (++) chunks) cs
         else do
           sendChunks $ toChunks chunks
           goChunks cs

    {-# INLINE sendChunks #-}
    sendChunks chunks = {-# SCC "sendChunks" #-} liftIO $ do
--      print $ chunks
--      print $ map B.length chunks
      Sock.sendMany socket chunks
{-# INLINE iterSocketLBS #-}

iterSocketBuilder :: MonadIO m => Socket -> E.Iteratee Builder m ()
iterSocketBuilder socket =
    E.continue $ go mempty
  where    
    go !b E.EOF = do
      liftIO $ Sock.sendMany socket $ L.toChunks $ toLazyByteString b
      E.yield () E.EOF
    go !b (E.Chunks [cs]) = do
      E.continue $ go $ mappend b cs
{-# INLINE iterSocketBuilder #-}

iterSocketByteString :: MonadIO m => Socket -> E.Iteratee Builder m ()
iterSocketByteString socket = 
       E.joinI $ ({-# SCC "iterSocketByteString.unsafeBuilderToByteString" #-} unsafeBuilderToByteString (allocBuffer BI.defaultBufferSize))
       --E.joinI $ ({-# SCC "iterSocketByteString.builderToByteString" #-} builderToByteString)
    $$ E.continue go
  where
    go E.EOF = E.yield () E.EOF
    go (E.Chunks cs) = do
      liftIO $ Sock.sendMany socket cs
      E.continue go
{-# INLINE iterSocketByteString #-}
    

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
