module Main where

import           Safe                           ( headMay )
import           System.Environment             ( getArgs )
import qualified Unison.Codebase.FileCodebase  as FileCodebase
import qualified Unison.CommandLine.Main       as CommandLine
import qualified Unison.Runtime.Rt1IO          as Rt1
import qualified Unison.Codebase.Path          as Path
import qualified Version


main :: IO ()
main = do
  args <- getArgs
  let initialPath = Path.absoluteEmpty
      launch = do
        (dir, theCodebase) <- FileCodebase.ensureCodebaseInitialized
        CommandLine.main dir
                         initialPath
                         (headMay args)
                         (pure Rt1.runtime)
                         theCodebase
  case args of
    ["--version"] -> putStrLn $ "ucm version: " ++ Version.gitDescribe
    _ -> launch
