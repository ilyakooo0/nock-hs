{-# LANGUAGE NoMonomorphismRestriction #-}

module Nock.Jam (jam, mat) where

import Control.Monad.Identity
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State hiding (get, put)
import Data.Binary.Bits.BitOrder
import Data.Binary.Bits.Put qualified as Put
import Data.Binary.Put (runPut)
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC.Base (Word (..), Word#, int2Word#, isTrue#, leWord#, plusWord#, timesWord#)
import GHC.Int (Int (..))
import GHC.Num
import Nock.Types

type BitPrinterM = StateT (Natural, Map Noun Natural) Put.BitPut

class (MonadState (Natural, Map Noun Natural) m) => BitPrinter m where
  putBit# :: (BitPrinter m) => Word# -> m ()
  putBool :: Bool -> m ()

instance BitPrinter (StateT (Natural, Map Noun Natural) Identity) where
  putBit# _ = do
    (counter, m) <- get
    put (counter + 1, m)
  putBool _ = do
    (counter, m) <- get
    put (counter + 1, m)

instance BitPrinter BitPrinterM where
  putBit# w = do
    (counter, m) <- get
    lift $ Put.putBool# w
    put (counter + 1, m)
  putBool b = do
    (counter, m) <- get
    lift $ Put.putBool b
    put (counter + 1, m)

getOffset :: (BitPrinter m) => m Natural
getOffset = fst <$> get

getMapping :: (BitPrinter m) => m (Map Noun Natural)
getMapping = snd <$> get

putMapping :: (BitPrinter m) => Noun -> Natural -> m ()
putMapping idx n = do
  (offset, m) <- get
  put (offset, M.insert idx n m)

jam :: Noun -> ByteString
jam n = runPut . Put.runBitPut . withBitOrder LL . flip evalStateT (0, M.empty) $ jam' n

jam' :: (BitPrinter m) => Noun -> m ()
jam' n = do
  offset <- getOffset
  mapping <- getMapping
  case M.lookup n mapping of
    Nothing -> do
      putMapping n offset
      case n of
        (Atom a _) -> do
          putBit# 0##
          mat' a
        (Cell lhs rhs _) -> do
          putBit# 1##
          putBit# 0##
          jam' lhs
          jam' rhs
    Just ref -> do
      case n of
        Cell {} -> do
          putBit# 1##
          putBit# 1##
          mat' ref
        (Atom a _) -> do
          if isTrue# (matSize a `leWord#` matSize ref)
            then do
              putBit# 0##
              mat' a
            else do
              putBit# 1##
              putBit# 1##
              mat' ref

mat :: Natural -> ByteString
mat n = runPut . Put.runBitPut . withBitOrder LL . flip evalStateT (0, M.empty) $ mat' n

mat' :: (BitPrinter m) => Natural -> m ()
mat' 0 = putBit# 1##
mat' n = do
  let bitsLength = countBits# n
      lengthBits = init $ numToBits (W# bitsLength)
  putBit# 0##
  putBits $ replicate (length lengthBits) False
  putBit# 1##
  putBits lengthBits
  putNumToBits# n

matSize :: Natural -> Word#
matSize 0 = 1##
matSize n =
  let bitsLength = countBits# n
      !(I# lengthBits) = length $ init $ numToBits (W# bitsLength)
   in 2## `plusWord#` (int2Word# lengthBits `timesWord#` 2##) `plusWord#` bitsLength

putBits :: (BitPrinter m) => [Bool] -> m ()
putBits [] = pure ()
putBits (x : xs) = putBool x >> putBits xs

numToBits :: (Bits a, Integral a) => a -> [Bool]
numToBits n | n <= 0 = []
numToBits n = ((n .&. 1) == 1) : numToBits (shiftR n 1)

countBits# :: Natural -> Word#
countBits# (NS 0##) = 0##
countBits# n = 1## `plusWord#` countBits# (n `naturalShiftR#` 1##)

putNumToBits# :: (BitPrinter m) => Natural -> m ()
putNumToBits# (NS 0##) = pure ()
putNumToBits# n = case n .&. 1 of
  NS w -> putBit# w >> putNumToBits# (n `naturalShiftR#` 1##)
  _ -> undefined
