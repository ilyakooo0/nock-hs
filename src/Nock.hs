{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Nock (tar, hax, fas, cell, atom, Annotation (..), Noun) where

import Data.Bits
import Data.Functor ((<&>))
import Effectful
import Effectful.State.Static.Local qualified as SEL
import GHC.Base
import GHC.Num
import GHC.Num.BigNat (bigNatAddWord#, bigNatFromWord2#)
import Nock.Types

sig :: Noun
sig = atom 0

one :: Noun
one = atom 1

two :: Noun
two = atom 2

three :: Noun
three = atom 3

four :: Noun
four = atom 4

twoThree :: Noun
twoThree = cell two three

sigOne :: Noun
sigOne = cell sig one

wut :: Noun -> Noun
wut Cell {} = sig
wut Atom {} = one

lus :: Noun -> Noun
lus ~(Atom nat _) = atom $ case nat of
  (NS n) -> case addWordC# n 1## of
    (# l, 0# #) -> NS l
    (# l, c #) -> NB (bigNatFromWord2# (int2Word# c) l)
  (NB n) -> NB (bigNatAddWord# n 1##)

tis :: (SEL.State EqualityCache :> es, IOE :> es) => Noun -> Noun -> Eff es Noun
tis lhs rhs = nounEq lhs rhs <&> \eq -> if eq then sig else one

fas# :: Word# -> Noun -> Noun
fas# lhs rhs =
  case lhs of
    1## -> rhs
    2## -> case rhs of
      ~(Cell lhs' _ _) -> lhs'
    3## -> case rhs of
      ~(Cell _ rhs' _) -> rhs'
    n ->
      let rest = fas# (uncheckedShiftRL# n 1#) rhs
       in case and# 1## n of
            1## -> fas# 3## rest
            _ -> fas# 2## rest

fas :: Natural -> Noun -> Noun
fas (NS w) rhs = fas# w rhs
fas n rhs =
  let rest = fas (unsafeShiftR n 1) rhs
   in if testBit n 0
        then fas 3 rest
        else fas 2 rest

hax# :: Word# -> Noun -> Noun -> Noun
hax# 1## b _ = b
hax# n b c =
  let a = uncheckedShiftRL# n 1#
   in case and# n 1## of
        1## -> hax# a (cell (fas# (n `xor#` 1##) c) b) c
        _ -> hax# a (cell b (fas# (n `or#` 1##) c)) c

hax :: Natural -> Noun -> Noun -> Noun
hax (NS n) b c = hax# n b c
hax n b c =
  let a = unsafeShiftR n 1
   in if testBit n 0
        then hax a (cell (fas (n - 1) c) b) c
        else hax a (cell b (fas (n + 1) c)) c

tar :: (SEL.State EqualityCache :> es, IOE :> es) => Noun -> Eff es Noun
tar ~(Cell subject ~(Cell a b _) _) = tar' a b subject

tar' :: (SEL.State EqualityCache :> es, IOE :> es) => Noun -> Noun -> Noun -> Eff es Noun
tar' b c subject = case b of
  Cell x y _ -> case c of
    ~(Cell l k _) -> cell <$> tar' x y subject <*> tar' l k subject
  Atom (NB _) _ -> undefined
  Atom (NS n) _ -> case n of
    0## -> case c of
      ~(Atom nat _) -> pure $ fas nat subject
    1## -> pure c
    3## -> case c of
      ~(Cell l k _) -> wut <$> tar' l k subject
    4## -> case c of
      ~(Cell l k _) -> lus <$> tar' l k subject
    _ -> case c of
      ~(Cell x ~(Cell h j _) _) -> case n of
        2## -> case x of
          ~(Cell l k _) ->
            tar' l k subject >>= \case
              ~formula@(Cell battery ~(Cell sample _ _) _) ->
                -- if traceShowId battery == Nock.Jets.add
                --   then error "Found it!"
                tar' h j subject >>= \case
                  ~(Cell u v _) -> tar' u v formula
        5## -> case x of
          ~(Cell l k _) -> do
            p <- tar' h j subject
            q <- tar' l k subject
            tis p q
        7## -> case x of
          ~(Cell l k _) -> tar' l k subject >>= tar' h j
        8## -> case x of
          ~(Cell l k _) -> do
            p <- tar' l k subject
            tar' h j (cell p subject)
        9## -> tar' h j subject >>= tar' two (cell sigOne (cell sig x))
        6## -> do
          p <- tar' four (cell four x) subject
          q <- tar' sig p twoThree
          tar' sig q (cell h j) >>= \case
            ~(Cell u v _) -> tar' u v subject
        10## -> case x of
          ~(Cell ~(Atom b' _) ~(Cell u v _) _) -> hax b' <$> tar' u v subject <*> tar' h j subject
        11## -> case x of
          Cell _ ~(Cell u v _) _ -> tar' h j subject
          Atom _ _ -> tar' h j subject
        _ -> undefined
