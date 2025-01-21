module Main (main) where

import Control.Monad
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.Text.Lazy.Encoding as TL
import Nock
import Nock.Parser qualified
import Nock.Printer
import Nock.Types
import System.FilePath.Posix
import System.Process.Typed
import Test.Tasty
import Test.Tasty.Golden
import Effectful
import Effectful.State.Static.Local qualified as SEL

main :: IO ()
main = do
  spec <- goldenTest
  defaultMain $ spec

goldenTest :: IO TestTree
goldenTest = do
  files <- findByExtension [".hoon"] "tests/golden"
  fmap (testGroup "nock") $ forM files $ \source -> do
    let target = replaceExtension source ".nock"
    pure $ goldenVsString (takeBaseName source) target $ do
      hoon <- BSL.readFile source
      nock <-
        fmap (Nock.Parser.noun . TL.decodeUtf8 . BSL.dropEnd 6 . BSL.drop 11 . snd)
          . readProcessStdout
          $ proc "urbit" ["eval"]
            & setStdin (byteStringInput hoon)
            & setStderr nullStream
      noun <- runEff . SEL.evalState (mempty :: EqualityCache) . tar $ cell (atom 0) nock
      let result = TL.encodeUtf8 . pretty $ noun
      pure result
