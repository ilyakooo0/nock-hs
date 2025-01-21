module Nock.Mug () where

import Data.ByteString (ByteString)
import Data.Hash.Murmur (murmur3)
import Data.Word (Word32)
import Nock.Types

type Mug = Word32

mugBS :: ByteString -> Word32
mugBS = mum 0xcafe_babe 0x7fff

mugAtom :: Atom -> Word32
mugAtom = mugBS . atomBytes

mugBoth :: Word32 -> Word32 -> Word32
mugBoth m n =
  mum 0xdead_beef 0xfffe $
    toStrict $
      toLazyByteString (word32LE m <> word32LE n)

mum :: Word32 -> Word32 -> ByteString -> Word32
mum syd fal key = go syd 0
  where
    go syd 8 = fal
    go syd i =
      let haz = murmur3 syd key
          ham = shiftR haz 31 `xor` (haz .&. 0x7fff_ffff)
       in if ham /= 0
            then ham
            else go (syd + 1) (i + 1)
