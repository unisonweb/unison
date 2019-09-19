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
import qualified System.IO.Temp as Temp
import qualified System.Directory as Directory
import qualified System.Exit as Exit


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
        "  `mytranscript.output.md` if successful. Exits after completion.",
        "",
        "ucm version",
        "  Prints version of Unison then quits.",
        "",
        "ucm help",
        "  Prints this help."
        ]

  -- so we can do `ucm --help`, `ucm -help` or `ucm help` (I hate
  -- having to remember which one is supported)
  let isFlag f arg = arg == f || arg == "-" ++ f || arg == "--" ++ f    
      initialPath = Path.absoluteEmpty
      launch dir code = CommandLine.main dir
                                     initialPath
                                     (headMay args)
                                     (pure Rt1.runtime)
                                     code
  let hasTranscript = any isMarkdown args 
      allOk = all isOk args
      isOk arg = isMarkdown arg || isDotU arg
      isDotU file = FP.takeExtension file == ".u"
      isMarkdown md = case FP.takeExtension md of
        ".md" -> True
        ".markdown" -> True
        _ -> False
  currentDir <- Directory.getCurrentDirectory
  transcriptDir <- if hasTranscript then Temp.createTempDirectory currentDir "transcript"
                   else pure currentDir
  when (not allOk) $ do
    putStrLn $ "\n" ++ usage
    Exit.exitWith (Exit.ExitFailure 1)
  case args of
    [version] | isFlag "version" version ->
      putStrLn $ "ucm version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> putStrLn usage
    args -> do
      (dir, theCodebase) <- FileCodebase.ensureCodebaseInitialized currentDir
      case args of 
        args@(_:_) -> do
          for_ args $ \arg -> case arg of
            md | isMarkdown md -> do
              parsed <- TR.parseFile arg
              case parsed of
                Left err -> putStrLn $ "Parse error: \n" <> show err
                Right stanzas -> do
                  (dir, theCodebase) <- FileCodebase.ensureCodebaseInitialized transcriptDir
                  mdOut <- TR.run dir stanzas theCodebase
                  writeUtf8 (currentDir FP.</> 
                             FP.addExtension (FP.dropExtension arg ++ ".output") 
                                             (FP.takeExtension md)) 
                            mdOut
            file | isDotU file -> undefined
            wat -> putStrLn $ "Unrecognized command, skipping: " <> wat
          when hasTranscript $ putStrLn $ unlines [ 
            "I've finished running the transcript(s). You can do:\n",
            "  pull " <> transcriptDir <> " .somepath", "",
            "from ucm to bring the resulting codebase into your local codebase."
            ]
        _ -> launch dir theCodebase
