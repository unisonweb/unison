{-# LANGUAGE TypeApplications #-}
module Main where

import           Safe                  (headMay)
import           System.Environment    (getArgs)
-- import qualified Unison.Codebase       as Codebase
import qualified Unison.Codebase.Watch as W
import           Unison.Symbol         (Symbol)
import           Unison.Codebase.Runtime.JVM (javaRuntime)

main :: IO ()
main = do
  args <- getArgs
  runtime <- javaRuntime @Symbol 42441
  case args of
    [""] -> go runtime Nothing
    _    -> go runtime (headMay args)
  where go runtime x = W.watcher x "." runtime undefined
