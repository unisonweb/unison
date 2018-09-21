module Main where

import qualified Unison.Codebase.Watch as W

main :: IO ()
main = W.watcher "." 42441
