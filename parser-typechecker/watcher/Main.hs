module Main where

import qualified Unison.Codebase.Watch as W
import           System.Environment (getArgs)
import           Safe               (headMay)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [""] -> go Nothing
    _ -> go (headMay args)
  where go x = W.watcher x "." 42441
