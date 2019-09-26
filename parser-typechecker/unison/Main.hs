module Main where

import           Data.List                      ( isPrefixOf )
import           Safe                           ( headMay )
import qualified System.Directory
import           System.Environment             ( getArgs )
import qualified System.Info
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
        dir <- getCurrentDirectory
        theCodebase <- FileCodebase.ensureCodebaseInitialized dir
        CommandLine.main dir
                         initialPath
                         (headMay args)
                         (pure Rt1.runtime)
                         theCodebase
  case args of
    ["--version"] -> putStrLn $ "ucm version: " ++ Version.gitDescribe
    _ -> launch

getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
  let widePrefix = "\\\\?\\"
  raw <- System.Directory.getCurrentDirectory
  pure $
    if System.Info.os `elem` ["mingw32", "win32", "cygwin32"] &&
       not (widePrefix `isPrefixOf` raw)
    then widePrefix ++ raw else raw
