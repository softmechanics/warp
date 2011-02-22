module PokeHex where

import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import Foreign

hexDigits :: Int -> Int
hexDigits = ceiling . logBase 16 . (+1) . fromIntegral

testPokeHex :: Int -> IO S.ByteString
testPokeHex i = do
  let len = hexDigits i
  fpbuf <- S.mallocByteString $ len 

  withForeignPtr fpbuf $ \buf ->
    pokeHex buf (len - 1) i

  return $ S.PS fpbuf 0 len

-- | pokeHex
--
pokeHex :: Ptr Word8 -> Int -> Int -> IO ()  
pokeHex fpbuf 0 dig | dig < 16 = 
  poke fpbuf $ toHex dig

pokeHex fpbuf cursor i = do
  let dig = mod i 16
      rest = div i 16
  pokeByteOff fpbuf cursor $ toHex dig
  pokeHex fpbuf (cursor - 1) rest

toHex :: Int -> Word8
toHex i = if i' < 10 then i' + 48 else i' + 55
  where i' = fromIntegral i


