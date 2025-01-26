module Nock.Jam (jam, cue, mat, rub) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Binary.Bits.BitOrder
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Base (Int (..), Word (..), Word#, int2Word#, minusWord#, or#, plusWord#, shiftL#, word2Int#)
import GHC.Num
import Nock
import Nock.Types

type BitParser = StateT (Int, Map Int Noun) BitGet

parseBit# :: BitParser Int
parseBit# = do
  (counter, m) <- get
  b <- lift getBool#
  put (counter + 1, m)
  pure b

parseBits# :: Word# -> BitParser Natural
parseBits# 0## = pure 0
parseBits# n = do
  I# b <- parseBit#
  rest <- parseBits# (n `minusWord#` 1##)
  pure $ (rest `shiftL` 1) `naturalOr` NS (int2Word# b)

getOffset :: BitParser Int
getOffset = fst <$> get

getMapping :: BitParser (Map Int Noun)
getMapping = snd <$> get

putMapping :: Map Int Noun -> BitParser ()
putMapping m = do
  (offset, _) <- get
  put (offset, m)

cue :: ByteString -> Noun
cue = runGet (runBitGet . withBitOrder LL . flip evalStateT (0, M.empty) $ cue')

cue' :: BitParser Noun
cue' = do
  offset <- getOffset
  I# isNotAtom <- parseBit#
  case isNotAtom of
    0# -> do
      a' <- rub'
      let a = atom a'
      m <- getMapping
      putMapping $ M.insert offset a m
      pure a
    _ -> do
      I# isRef <- parseBit#
      case isRef of
        0# -> do
          x <- cue'
          y <- cue'
          let c = cell x y
          m <- getMapping
          putMapping $ M.insert offset c m
          pure c
        _ -> do
          ref <- rub'
          let ref# = case ref of
                NS w -> I# (word2Int# w)
                _ -> undefined
          m <- getMapping
          case M.lookup ref# m of
            Nothing -> undefined
            Just a -> pure a

jam :: Noun -> ByteString
jam n = runPut . runBitPut . withBitOrder LL $ jam' n

jam' :: Noun -> BitPut ()
jam' (Atom n _) = do
  putBool# 0##
  mat' n
jam' (Cell lhs rhs _) = do
  putBool# 1##
  putBool# 0##
  jam' lhs
  jam' rhs

mat :: Natural -> ByteString
mat n = runPut . runBitPut . withBitOrder LL $ mat' n

mat' :: Natural -> BitPut ()
mat' 0 = putBool# 1##
mat' n = do
  let bitsLength = countBits# n
      lengthBits = init $ numToBits (W# bitsLength)
  putBool# 0##
  putBits $ replicate (length lengthBits) False
  putBool# 1##
  putBits lengthBits
  putNumToBits# n

putBits :: [Bool] -> BitPut ()
putBits [] = pure ()
putBits (x : xs) = putBool x >> putBits xs

rub :: ByteString -> Natural
rub = runGet (runBitGet . withBitOrder LL . flip evalStateT (0, M.empty) $ rub')

rub' :: BitParser Natural
rub' = do
  parseBit# >>= \case
    I# 0# -> do
      W# lengthOfLength <- countZeros 0
      lengthBits <- parseBits# lengthOfLength
      let lengthBits# = case lengthBits of
            NS w -> w
            _ -> undefined
      let lent = lengthBits# `or#` (1## `shiftL#` word2Int# lengthOfLength)
      parseBits# lent
    _ -> pure 0

countZeros :: Word -> BitParser Word
countZeros n = do
  I# b <- parseBit#
  case b of
    0# -> countZeros (n + 1)
    _ -> pure n

numToBits :: (Bits a, Integral a) => a -> [Bool]
numToBits n | n <= 0 = []
numToBits n = ((n .&. 1) == 1) : numToBits (shiftR n 1)

countBits# :: Natural -> Word#
countBits# (NS 0##) = 0##
countBits# n = 1## `plusWord#` countBits# (n `naturalShiftR#` 1##)

putNumToBits# :: Natural -> BitPut ()
putNumToBits# (NS 0##) = pure ()
putNumToBits# n = case n .&. 1 of
  NS w -> putBool# w >> putNumToBits# (n `naturalShiftR#` 1##)
  _ -> undefined

-- ((n .&. 1) == 1) : numToBits (shiftR n 1)

bitsToNum :: (Bits a, Integral a) => [Bool] -> a
bitsToNum = foldr (\b acc -> shiftL acc 1 .|. if b then 1 else 0) 0
