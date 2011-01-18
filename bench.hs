{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Data.Enumerator (run_, ($$), enumList, run)
import Network.Wai.Handler.Warp (takeUntilBlank, takeHeaders)
import qualified Data.ByteString as B

smallHeaders = ["f", "oo\n", "bar\r\nbaz\r\n\r\n"]

-- headers are usually all read into a single chunk
bigHeaders = [bs]
  where bs = B.concat [
                  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n"
                , "\r\n"
                ]

testHeaders v = [
      bench "Small Header" $ go smallHeaders
    , bench "Big Header" $ go bigHeaders
    ]
  where go headers = nfIO $ do
          bs <- run_ $ (enumList 1 headers) $$ v
          -- ByteString doesn't have NFData instance
          return $ map B.length bs

group = bgroup "Parse Headers" [
            bgroup "takeUntilBlank" $ testHeaders $ takeUntilBlank 0 id
          , bgroup "takeHeaders" $ testHeaders takeHeaders
          ]

main = defaultMain [group]

