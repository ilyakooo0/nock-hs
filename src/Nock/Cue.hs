module Nock.Cue (cue, rub) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Binary.Bits.BitOrder
import Data.Binary.Bits.Get
import Data.Binary.Get (runGet)
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Base (Int (..), Word (..), Word#, int2Word#, minusWord#, or#, shiftL#, word2Int#)
import GHC.Num
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
