{-# Language OverloadedStrings #-}

module Main where

import Unison.Prelude
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
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Util.Pretty as P
import qualified Unison.PrettyTerminal as PT
import qualified Data.Text as Text

main :: IO ()
main = do
  args               <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool
  let usage = P.callout "🌻" $ P.lines [
        P.bold "Usage instructions for the Unison Codebase Manager",
        "You are running version: " <> P.string Version.gitDescribe,
        "",
        P.bold "ucm",
        P.wrap "Starts Unison and listens for commands and file changes.", 
        "",
        P.bold "ucm mymain.u arg1 arg2",
        P.wrap $ "Executes the definition called `main` in `mymain.u`, passing"
              <> "any optional arguments arg1, arg2, etc, then exits after completion.",
        "",
        P.bold "ucm mytranscript.md",
        P.wrap $ "Executes the `mytranscript.md` transcript and creates"
              <> "`mytranscript.output.md` if successful. Exits after completion." 
              <> "Multiple transcript files may be provided; they are processed in sequence"
              <> "starting from the same codebase.",
        "",
        P.bold "ucm sandbox mytranscript.md",
        P.wrap $ "Executes the `mytranscript.md` transcript in a fresh codebase"
              <> "and creates `mytranscript.output.md` if successful. Exits after completion."
              <> "Multiple transcript files may be provided; they are processed in sequence"
              <> "starting from the same codebase.",
        "",
        P.bold "ucm version",
        "Prints version of Unison then quits.",
        "",
        P.bold "ucm help",
        "Prints this help."
        ]

  -- so we can do `ucm --help`, `ucm -help` or `ucm help` (I hate
  -- having to remember which one is supported)
  let isFlag f arg = arg == f || arg == "-" ++ f || arg == "--" ++ f    
      initialPath = Path.absoluteEmpty
      launch dir code inputs = CommandLine.main dir
                                     initialPath
                                     inputs 
                                     (pure Rt1.runtime)
                                     code
  let hasTranscript = any isMarkdown args 
      allOk = all isDotU (take 1 args) || all isOk args
      isOk arg = isMarkdown arg || isDotU arg
              || isFlag "version" arg || isFlag "help" arg || isFlag "sandbox" arg
      isDotU file = FP.takeExtension file == ".u"
      isMarkdown md = case FP.takeExtension md of
        ".md" -> True
        ".markdown" -> True
        _ -> False
  currentDir <- Directory.getCurrentDirectory
  transcriptDir <- if hasTranscript then Temp.createTempDirectory currentDir "transcript"
                   else pure currentDir
  when (not allOk) $ do
    PT.putPrettyLn usage
    Exit.exitWith (Exit.ExitFailure 1)
  case args of
    [version] | isFlag "version" version ->
      putStrLn $ "ucm version: " ++ Version.gitDescribe
    [help] | isFlag "help" help -> PT.putPrettyLn usage
    (file:args) | isDotU file -> do 
      e <- safeReadUtf8 file
      case e of
        Left _ -> PT.putPrettyLn $ P.callout "⚠️" "I couldn't find that file or it is for some reason unreadable."
        Right contents -> do
          (dir, theCodebase) <- FileCodebase.ensureCodebaseInitialized currentDir
          let fileEvent = Input.UnisonFileChanged (Text.pack file) contents
          launch dir theCodebase [Left fileEvent, Right $ Input.ExecuteI args, Right Input.QuitI]    
    args -> do
      (dir, theCodebase) <- FileCodebase.ensureCodebaseInitialized currentDir
      let sandboxed = take 1 args == ["sandbox"]
      case args of 
        args@(_:_) -> do
          for_ args $ \arg -> case arg of
            md | isMarkdown md -> do
              parsed <- TR.parseFile arg
              case parsed of
                Left err -> putStrLn $ "Parse error: \n" <> show err
                Right stanzas -> do
                  (dir, theCodebase) <- 
                    if sandboxed then FileCodebase.ensureCodebaseInitialized transcriptDir
                    else pure (dir, theCodebase)
                  mdOut <- TR.run dir stanzas theCodebase
                  let out = currentDir FP.</> 
                             FP.addExtension (FP.dropExtension arg ++ ".output") 
                                             (FP.takeExtension md)
                  writeUtf8 out mdOut
                  putStrLn $ "💾  Wrote " <> out
            "sandbox" -> pure ()
            wat -> putStrLn $ "Unrecognized command, skipping: " <> wat
          when hasTranscript . PT.putPrettyLn $
            P.callout "🌸" (
              P.lines [
                "I've finished running the transcript(s) in this codebase:", "",
                P.indentN 2 (P.string transcriptDir), "",
                "You can run `ucm` in this directory to do more work on it."])
        _ -> launch dir theCodebase []
