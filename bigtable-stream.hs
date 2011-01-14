{-# LANGUAGE OverloadedStrings, CPP #-}
import Network.Wai
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromShow)
import Data.Monoid (mappend)
import Network.Wai.Handler.Warp (run)
import Data.Enumerator (enumList, ($$), run_)

bigtable :: [Builder]
bigtable =
    fromByteString "<table>"
    : foldr (:) [fromByteString "</table>"] (replicate 1000 row)
  where
    row = fromByteString "<tr>"
          `mappend` foldr go (fromByteString "</tr>") [1..50]
    go i rest = fromByteString "<td>"
                `mappend` fromShow i
                `mappend` fromByteString "</td>"
                `mappend` rest

main = run 3000 app

app _ = return $ ResponseEnumerator $ \f ->
    run_ $ enumList 1 bigtable $$ f status200 [
      ("Content-Type", "text/html")
#ifndef CHUNKED_RESPONSE
    , ("Content-Length", "22015")
#endif
    ]
