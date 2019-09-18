module Main where

import Unison.Prelude
import           Safe                           ( headMay )
import           System.Environment             ( getArgs )
import qualified Unison.Codebase.FileCodebase  as FileCodebase
import qualified Unison.CommandLine.Main       as CommandLine
import qualified Unison.Runtime.Rt1IO          as Rt1
import qualified Unison.Codebase.Path          as Path
import qualified Version as Version
import qualified Unison.Codebase.TranscriptParser as TR
import qualified System.FilePath as FP


main :: IO ()
main = do
  args               <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool
  let usage = unlines [
        "ucm",
        "  Starts Unison and listens for commands and file changes.",
        "",
        "ucm mymain.u",
        "  Executes the definition called `main` in `mymain.u` and",
        "  exits after completion.",
        "",
        "ucm mytranscript.md",
        "  Executes the `mytranscript.md` transcript and creates",
        "  `mytranscript-output.md` if successful. Exits after completion.",
        "",
        "ucm version",
        "  Prints version of Unison then quits",
        "",
        "ucm help",
        "  Prints this help"
        ]

  -- so we can do `ucm --help`, `ucm -help` or `ucm help` (I hate
  -- having to remember which one is supported)
  let isFlag f arg = arg == f || arg == "-" ++ f || arg == "--" ++ f    

  case args of
    [version] | isFlag "version" version ->
      putStrLn $ "ucm version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> putStrLn usage
    args -> do
      (dir, theCodebase) <- FileCodebase.ensureCodebaseInitialized
      let initialPath = Path.absoluteEmpty
          launch      = CommandLine.main dir
                                         initialPath
                                         (headMay args)
                                         (pure Rt1.runtime)
                                         theCodebase
      case args of 
        args @ (_:_) -> for_ args $ \arg -> case FP.takeExtension arg of
          md | md == ".md" || md == ".markdown" -> do
            parsed <- TR.parseFile arg
            case parsed of
              Left err -> do
                putStrLn "Parse error: "
                putStrLn $ show err
              Right stanzas -> do
                -- todo: run on a copy of the codebase?
                mdOut <- TR.run dir stanzas theCodebase
                writeUtf8 (dir FP.</> (FP.addExtension (FP.dropExtension arg ++ "-output") md)) mdOut
          ".u" -> undefined
          _wat -> do
            putStrLn $ "That's not a command I recognize, sorry!"
            putStrLn $ "\n" ++ usage
        _ -> launch
