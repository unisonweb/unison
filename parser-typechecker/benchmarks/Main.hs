module Main(main) where

import Criterion.Main

import qualified Benchmarks.Util.Bytes as Bytes
import qualified Benchmarks.Util.Text as Text


main :: IO ()
main = defaultMain
  [ Bytes.run
  , Text.run
  ]
