module Main (main) where

import Control.Monad
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.Text.Lazy.Encoding as TL
import Gauge
import Nock
import Nock.Parser (noun)
import System.FilePath.Posix
import System.Process.Typed
import Test.Tasty.Golden (findByExtension)
import Effectful

main :: IO ()
main = do
  files <- findByExtension [".hoon"] "tests/golden"
  benchmarks <- forM files $ \hoonFile -> do
    hoon <- BSL.readFile hoonFile
    nock <-
      fmap (Nock.Parser.noun . TL.decodeUtf8 . BSL.dropEnd 6 . BSL.drop 11 . snd)
        . readProcessStdout
        $ proc "urbit" ["eval"]
          & setStdin (byteStringInput hoon)
          & setStderr nullStream
    pure $ bench (takeBaseName hoonFile) $ nf (runPureEff . tar) $ cell (atom 0) nock
  defaultMain benchmarks
