module Nock.Types
  ( pretty,
    Noun (..),
    Annotation (..),
    atom,
    cell,
    nounEq,
    EqualityCache,
    annotation,
  )
where

import Control.DeepSeq
import Data.HashMap.Strict qualified as HM
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLB
import Effectful
import Effectful.State.Static.Local qualified as SEL
import GHC.Generics
import GHC.Natural
import System.Mem.StableName

newtype EqualityCache = EqualityCache (HM.HashMap (StableName Noun, StableName Noun) Bool)
  deriving newtype (Semigroup, Monoid)

nounEq :: (SEL.State EqualityCache :> es, IOE :> es) => Noun -> Noun -> Eff es Bool
nounEq x y = do
  xName <- liftIO $ makeStableName x
  yName <- liftIO $ makeStableName y
  EqualityCache cache <- SEL.get
  case HM.lookup (xName, yName) cache of
    Just res -> pure res
    Nothing -> do
      res <- case (x, y) of
        (Atom xa _, Atom ya _) -> pure $ xa == ya
        (Cell xLhs xRhs _, Cell yLhs yRhs _) -> do
          lhsRes <- nounEq xLhs yLhs
          if lhsRes then nounEq xRhs yRhs else pure False
        _ -> pure False
      SEL.put $ EqualityCache $ HM.insert (xName, yName) res $ HM.insert (yName, xName) res $ cache
      pure res

data Annotation = Annotation
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

atom :: Natural -> Noun
atom nat = Atom nat Annotation {}

cell :: Noun -> Noun -> Noun
cell lhs rhs =
  Cell
    lhs
    rhs
    Annotation
      {
      }

annotation :: Noun -> Annotation
annotation (Atom _ ann) = ann
annotation (Cell _ _ ann) = ann

data Noun
  = Atom !Natural Annotation
  | Cell !Noun !Noun Annotation
  deriving stock (Generic)
  deriving anyclass (NFData)

instance Show Noun where
  show = TL.unpack . pretty

pretty :: Noun -> TL.Text
pretty noun = TLB.toLazyText $ pretty' noun False

pretty' :: Noun -> Bool -> TLB.Builder
pretty' (Atom a _) _ = TLB.decimal a
pretty' (Cell lhs rhs _) True = pretty' lhs False <> " " <> pretty' rhs True
pretty' (Cell lhs rhs _) False = "[" <> pretty' lhs False <> " " <> pretty' rhs True <> "]"
