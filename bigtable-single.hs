{-# LANGUAGE OverloadedStrings, CPP #-}
import Network.Wai
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromShow)
import Data.Monoid (mappend)
import Network.Wai.Handler.Warp (run)
import Data.List (foldl1')

bigtable :: Builder
bigtable =
    fromByteString "<table>"
    `mappend` foldl1' mappend (replicate 1000 row)
    `mappend` fromByteString "</table>"
  where
    row = fromByteString "<tr>"
          `mappend` foldr go (fromByteString "</tr>") [1..50]
    go i rest = fromByteString "<td>"
                `mappend` fromShow i
                `mappend` fromByteString "</td>"
                `mappend` rest

main = run 3000 app

app _ = return $ ResponseBuilder status200 [
      ("Content-Type", "text/html")
#ifndef CHUNKED_RESPONSE
    , ("Content-Length", B.pack "22015")
#endif
    ] bigtable
