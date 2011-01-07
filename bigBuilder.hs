{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder (fromByteString)
import qualified Data.ByteString.Char8 as B
import Data.Monoid

kilo = fromByteString $ B.pack $ take 1024 $ repeat '.'
kilos = kilos' mempty
kilos' !b !0 = b
kilos' !b !n = kilos' (b `mappend` kilo) (n-1)

response n = responseBuilder
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", B.pack $ show $ n * 1024)
    ]
    $ kilos n

main = run 3000 $ const $ return $ response 128
