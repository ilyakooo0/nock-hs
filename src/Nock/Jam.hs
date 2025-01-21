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
import Nock
import Nock.Types
import Numeric.Natural

type BitParser = StateT (Int, Map Int Noun) BitGet

parseBit :: BitParser Bool
parseBit = do
  (counter, m) <- get
  b <- lift getBool
  put (counter + 1, m)
  pure b

parseBits :: Int -> BitParser [Bool]
parseBits 0 = pure []
parseBits n = do
  b <- parseBit
  (b :) <$> parseBits (n - 1)

getOffset :: BitParser Int
getOffset = fst <$> get

cue :: ByteString -> Noun
cue = runGet (runBitGet . withBitOrder LL . flip evalStateT (0, M.empty) $ cue')

cue' :: BitParser Noun
cue' = do
  offset <- getOffset
  isNotAtom <- parseBit
  case isNotAtom of
    False -> do
      a' <- rub'
      let a = atom a'
      (offset, m) <- get
      put (offset, M.insert offset a m)
      pure a
    True -> do
      isNotRef <- parseBit
      case isNotRef of
        False -> do
          x <- cue'
          y <- cue'
          let c = cell x y
          (offset, m) <- get
          put (offset, M.insert offset c m)
          pure c
        True -> do
          ref <- rub'
          (_, m) <- get
          case M.lookup (fromIntegral ref) m of
            Nothing -> undefined
            Just a -> pure a

jam :: Noun -> ByteString
jam n = runPut . runBitPut . withBitOrder LL $ jam' n

jam' :: Noun -> BitPut ()
jam' (Atom n _) = do
  putBool False
  mat' n
jam' (Cell lhs rhs _) = do
  putBool True
  putBool False
  jam' lhs
  jam' rhs

mat :: Natural -> ByteString
mat n = runPut . runBitPut . withBitOrder LL $ mat' n

mat' :: Natural -> BitPut ()
mat' 0 = putBool True
mat' n = do
  let bits = numToBits n
      bitsLength = length bits
      lengthBits = init $ numToBits bitsLength
  putBool False
  putBits $ replicate (length lengthBits) False
  putBool True
  putBits lengthBits
  putBits bits

putBits :: [Bool] -> BitPut ()
putBits [] = pure ()
putBits (x : xs) = putBool x >> putBits xs

rub :: ByteString -> Natural
rub = runGet (runBitGet . withBitOrder LL . flip evalStateT (0, M.empty) $ rub')

rub' :: BitParser Natural
rub' = do
  parseBit >>= \case
    True -> pure 0
    False -> do
      lengthOfLength <- countZeros 0
      lengthBits <- (++ [True]) <$> parseBits lengthOfLength
      let lent = bitsToNum lengthBits
      bitsToNum <$> parseBits lent

countZeros :: Int -> BitParser Int
countZeros n = do
  b <- parseBit
  case b of
    True -> pure n
    False -> countZeros (n + 1)

numToBits :: (Bits a, Integral a) => a -> [Bool]
numToBits n | n <= 0 = []
numToBits n = ((n .&. 1) == 1) : numToBits (shiftR n 1)

bitsToNum :: (Bits a, Integral a) => [Bool] -> a
bitsToNum = foldr (\b acc -> shiftL acc 1 .|. if b then 1 else 0) 0
