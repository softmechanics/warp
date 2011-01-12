{-# LANGUAGE OverloadedStrings, CPP #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder (fromByteString)

main = run 3000 $ const $ return $ ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
#ifndef CHUNKED_RESPONSE
    , ("Content-Length", "4")
#endif
    ]
    $ fromByteString "PONG"
