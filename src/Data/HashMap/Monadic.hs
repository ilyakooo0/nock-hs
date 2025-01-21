module Data.HashMap.Monadic (MEq (..), insert, lookup) where

import Control.Monad.ST (runST)
import Data.Bits
import Data.Functor ((<&>))
import Data.HashMap.Internal
  ( Hash,
    HashMap (..),
    Leaf (..),
    bitmapIndexedOrFull,
    collision,
    hash,
    index,
    mask,
    nextShift,
    ptrEq,
    sparseIndex,
    two,
    update32,
  )
import Data.HashMap.Internal.Array qualified as A
import Data.Hashable
import Prelude hiding (lookup)

class MEq x m where
  (===) :: x -> x -> m Bool

insert :: (MEq k m, Hashable k, Monad m) => k -> v -> HashMap k v -> m (HashMap k v)
insert k v m = insert' (Data.HashMap.Internal.hash k) k v m

insert' :: forall m k v. (MEq k m, Monad m) => Hash -> k -> v -> HashMap k v -> m (HashMap k v)
insert' h0 k0 v0 m0 = go h0 k0 v0 0 m0
  where
    -- go :: _ -> _ -> _ -> _ -> _ -> m _
    go !h !k x !_ Empty = pure @m $ Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
      | hy == h = do
          ky === k >>= \eq ->
            if eq
              then
                if x `ptrEq` y
                  then pure @m t
                  else pure @m $ Leaf h (L k x)
              else pure $ collision h l (L k x)
      | otherwise = pure $ runST (two s h k x hy t)
    go h k x s t@(BitmapIndexed b ary)
      | b .&. m == 0 =
          let !ary' = A.insert ary i $! Leaf h (L k x)
           in pure $ bitmapIndexedOrFull (b .|. m) ary'
      | otherwise = do
          let !st = A.index ary i
          !st' <- go h k x (nextShift s) st
          if st' `ptrEq` st
            then pure t
            else pure $ BitmapIndexed b (A.update ary i st')
      where
        m = mask h s
        i = sparseIndex b m
    go h k x s t@(Full ary) = do
      let !st = A.index ary i
      !st' <- go h k x (nextShift s) st
      if st' `ptrEq` st
        then pure t
        else pure $ Full (update32 ary i st')
      where
        i = index h s
    go h k x s t@(Collision hy v)
      | h == hy = Collision h <$> (updateOrSnocWith (\a _ -> (# a #)) k x v)
      | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)

updateOrSnocWith ::
  (MEq k m, Monad m) =>
  (v -> v -> (# v #)) ->
  k ->
  v ->
  A.Array (Leaf k v) ->
  m (A.Array (Leaf k v))
updateOrSnocWith f = updateOrSnocWithKey (const f)
{-# INLINEABLE updateOrSnocWith #-}

updateOrSnocWithKey ::
  (MEq k m, Monad m) =>
  (k -> v -> v -> (# v #)) ->
  k ->
  v ->
  A.Array (Leaf k v) ->
  m (A.Array (Leaf k v))
updateOrSnocWithKey f k0 v0 ary0 = go k0 v0 ary0 0 (A.length ary0)
  where
    go !k v !ary !i !n
      -- Not found, append to the end.
      | i >= n = pure $ A.snoc ary $ L k v
      | otherwise =
          let L kx y = A.index ary i
              (# v2 #) = f k v y
           in k === kx >>= \eq ->
                if eq
                  then pure $ A.update ary i (L k v2)
                  else go k v ary (i + 1) n

lookup :: (MEq k m, Hashable k, Monad m) => k -> HashMap k v -> m (Maybe v)
lookup k m = lookupCont Nothing (\v _i -> Just v) (Data.HashMap.Internal.hash k) k 0 m

lookupCont ::
  forall m r k v.
  (MEq k m, Monad m) =>
  r -> -- Absent continuation
  (v -> Int -> r) -> -- Present continuation
  Hash -> -- The hash of the key
  k ->
  Int -> -- The offset of the subkey in the hash.
  HashMap k v ->
  m r
lookupCont absent present !h0 !k0 !s0 !m0 = go h0 k0 s0 m0
  where
    go :: Hash -> k -> Int -> HashMap k v -> m r
    go !_ !_ !_ Empty = pure absent
    go h k _ (Leaf hx (L kx x))
      | h == hx =
          k === kx <&> \eq ->
            if eq
              then present x (-1)
              else absent
      | otherwise = pure absent
    go h k s (BitmapIndexed b v)
      | b .&. m == 0 = pure absent
      | otherwise =
          go h k (nextShift s) (A.index v (sparseIndex b m))
      where
        m = mask h s
    go h k s (Full v) =
      go h k (nextShift s) (A.index v (index h s))
    go h k _ (Collision hx v)
      | h == hx = lookupInArrayCont absent present k v
      | otherwise = pure absent

lookupInArrayCont ::
  forall m r k v.
  (MEq k m, Monad m) =>
  r ->
  (v -> Int -> r) ->
  k ->
  A.Array (Leaf k v) ->
  m r
lookupInArrayCont absent present k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go :: k -> A.Array (Leaf k v) -> Int -> Int -> m r
    go !k !ary !i !n
      | i >= n = pure absent
      | otherwise = case A.index ary i of
          (L kx v) ->
            k === kx >>= \eq ->
              if eq
                then pure $ present v i
                else go k ary (i + 1) n
