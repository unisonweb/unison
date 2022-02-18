{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}
{-# Language ViewPatterns #-}

{- Parse and execute markdown transcripts.
-}
module Unison.Codebase.TranscriptParser
  ( Stanza (..),
    FenceType,
    ExpectingError,
    Hidden,
    Err,
    UcmLine (..),
    run,
    parse,
    parseFile,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Control.Monad.State (runStateT)
import Data.List (isSubsequenceOf)
import Data.IORef
import Prelude hiding (readFile, writeFile)
import System.Directory ( doesFileExist )
import System.Exit (die)
import System.IO.Error (catchIOError)
import System.Environment (getProgName)
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.Command (LoadSourceResult (..))
import Unison.Codebase.Editor.Input (Input (..), Event(UnisonFileChanged))
import Unison.CommandLine
import Unison.CommandLine.InputPattern (InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
import Unison.CommandLine.OutputMessages (notifyUser, notifyNumbered)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Symbol (Symbol)
import Unison.CommandLine.Welcome (asciiartUnison)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.IO as IO
import qualified Crypto.Random as Random
import qualified Text.Megaparsec as P
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.HandleCommand as HandleCommand
import qualified Unison.Codebase.Editor.HandleInput.LoopState as LoopState
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as Q
import qualified Unison.Codebase.Editor.Output as Output
import Control.Lens (view)
import Control.Error (rightMay)
import qualified Unison.Codebase.Editor.HandleInput as HandleInput

-- | Render transcript errors at a width of 65 chars.
terminalWidth :: P.Width
terminalWidth = 65

type ExpectingError = Bool
type Err = String
type ScratchFileName = Text
type FenceType = Text

data Hidden = Shown | HideOutput | HideAll
            deriving (Eq, Show)
data UcmLine =
  UcmCommand Path.Absolute Text
  | UcmComment Text -- Text does not include the '--' prefix.

data Stanza
  = Ucm Hidden ExpectingError [UcmLine]
  | Unison Hidden ExpectingError (Maybe ScratchFileName) Text
  | UnprocessedFence FenceType Text
  | Unfenced Text

instance Show UcmLine where
  show (UcmCommand path txt) = show path <> ">" <> Text.unpack txt
  show (UcmComment txt) = "--" ++ Text.unpack txt

instance Show Stanza where
  show s = case s of
    Ucm _ _ cmds -> unlines [
      "```ucm",
      foldl (\x y -> x ++ show y) "" cmds,
      "```"
      ]
    Unison _hide _ fname txt -> unlines [
      "```unison",
      case fname of
        Nothing -> Text.unpack txt <> "```\n"
        Just fname -> unlines [
          "---",
          "title: " <> Text.unpack fname,
          "---",
          Text.unpack txt,
          "```",
          "" ]
      ]
    UnprocessedFence typ txt -> unlines [
      "```" <> Text.unpack typ,
      Text.unpack txt,
      "```", "" ]
    Unfenced txt -> Text.unpack txt

parseFile :: FilePath -> IO (Either Err [Stanza])
parseFile filePath = do
  exists <- doesFileExist filePath
  if exists then do
    txt <- readUtf8 filePath
    pure $ parse filePath txt
  else
    pure $ Left $ show filePath ++ " does not exist"

parse :: String -> Text -> Either Err [Stanza]
parse srcName txt = case P.parse (stanzas <* P.eof) srcName txt of
  Right a -> Right a
  Left e -> Left (show e)

run :: String -> FilePath -> FilePath -> [Stanza] -> Codebase IO Symbol Ann -> IO Text
run version dir configFile stanzas codebase = do
  let initialPath = Path.absoluteEmpty
  putPrettyLn $ P.lines [
    asciiartUnison, "",
    "Running the provided transcript file...",
    ""
    ]
  root <- fromMaybe Branch.empty . rightMay <$> Codebase.getRootBranch codebase
  do
    pathRef                  <- newIORef initialPath
    rootBranchRef            <- newIORef root
    numberedArgsRef          <- newIORef []
    inputQueue               <- Q.newIO
    cmdQueue                 <- Q.newIO
    unisonFiles              <- newIORef Map.empty
    out                      <- newIORef mempty
    hidden                   <- newIORef Shown
    allowErrors              <- newIORef False
    hasErrors                <- newIORef False
    mStanza                  <- newIORef Nothing
    (config, cancelConfig)   <-
      catchIOError (watchConfig configFile) $ \_ ->
        die "Your .unisonConfig could not be loaded. Check that it's correct!"
    runtime                  <- RTI.startRuntime RTI.Standalone version
    traverse_ (atomically . Q.enqueue inputQueue) (stanzas `zip` [1..])
    let patternMap =
          Map.fromList
            $   validInputs
            >>= (\p -> (patternName p, p) : ((, p) <$> aliases p))
    let
      output' :: Bool -> String -> IO ()
      output' inputEcho msg = do
        hide <- readIORef hidden
        unless (hideOutput inputEcho hide) $ modifyIORef' out (\acc -> acc <> pure msg)

      hideOutput :: Bool -> Hidden -> Bool
      hideOutput inputEcho = \case
        Shown      -> False
        HideOutput -> True && (not inputEcho)
        HideAll    -> True

      output, outputEcho :: (String -> IO ())
      output = output' False
      outputEcho = output' True

      awaitInput :: IO (Either Event Input)
      awaitInput = do
        cmd <- atomically (Q.tryDequeue cmdQueue)
        case cmd of
          -- end of ucm block
          Just Nothing -> do
            output "\n```\n"
            -- We clear the file cache after each `ucm` stanza, so
            -- that `load` command can read the file written by `edit`
            -- rather than hitting the cache.
            writeIORef unisonFiles Map.empty
            dieUnexpectedSuccess
            awaitInput
          -- ucm command to run
          Just (Just ucmLine) -> do
            case ucmLine of
              p@(UcmComment {}) -> do
                output ("\n" <> show p)
                awaitInput
              p@(UcmCommand path lineTxt) -> do
                curPath <- readIORef pathRef
                if curPath /= path then do
                  atomically $ Q.undequeue cmdQueue (Just p)
                  pure $ Right (SwitchBranchI $ Just (Path.absoluteToPath' path))
                else case words . Text.unpack $ lineTxt of
                  [] -> awaitInput
                  args -> do
                    output ("\n" <> show p <> "\n")
                    numberedArgs <- readIORef numberedArgsRef
                    currentRoot <- Branch.head <$> readIORef rootBranchRef
                    case parseInput currentRoot curPath numberedArgs patternMap args of
                      -- invalid command is treated as a failure
                      Left msg -> dieWithMsg $ P.toPlain terminalWidth msg
                      Right input -> pure $ Right input

          Nothing -> do
            dieUnexpectedSuccess
            writeIORef hidden Shown
            writeIORef allowErrors False
            maybeStanza <- atomically (Q.tryDequeue inputQueue)
            _ <- writeIORef mStanza maybeStanza
            case maybeStanza of
              Nothing -> do
                putStrLn ""
                pure $ Right QuitI
              Just (s,idx) -> do
                putStr $ "\r⚙️   Processing stanza " ++ show idx ++ " of "
                                              ++ show (length stanzas) ++ "."
                IO.hFlush IO.stdout
                case s of
                  Unfenced _ -> do
                    output $ show s
                    awaitInput
                  UnprocessedFence _ _ -> do
                    output $ show s
                    awaitInput
                  Unison hide errOk filename txt -> do
                    writeIORef hidden hide
                    outputEcho $ show s
                    writeIORef allowErrors errOk
                    output "```ucm\n"
                    atomically . Q.enqueue cmdQueue $ Nothing
                    modifyIORef' unisonFiles (Map.insert (fromMaybe "scratch.u" filename) txt)
                    pure $ Left (UnisonFileChanged (fromMaybe "scratch.u" filename) txt)
                  Ucm hide errOk cmds -> do
                    writeIORef hidden hide
                    writeIORef allowErrors errOk
                    writeIORef hasErrors False
                    output "```ucm"
                    traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
                    atomically . Q.enqueue cmdQueue $ Nothing
                    awaitInput

      loadPreviousUnisonBlock name = do
        ufs <- readIORef unisonFiles
        case Map.lookup name ufs of
          Just uf ->
            return (LoadSuccess uf)
          Nothing ->
            -- This lets transcripts use the `load` command, as in:
            --
            -- .> load someFile.u
            --
            -- Important for Unison syntax that can't be embedded in
            -- transcripts (like docs, which use ``` in their syntax).
            let f = LoadSuccess <$> readUtf8 (Text.unpack name)
            in f <|> pure InvalidSourceNameError

      cleanup = do Runtime.terminate runtime; cancelConfig
      print o = do
        msg <- notifyUser dir o
        errOk <- readIORef allowErrors
        let rendered = P.toPlain terminalWidth (P.border 2 msg)
        output rendered
        when (Output.isFailure o) $
          if errOk then writeIORef hasErrors True
          else dieWithMsg rendered

      printNumbered o = do
        let (msg, numberedArgs) = notifyNumbered o
        errOk <- readIORef allowErrors
        let rendered = P.toPlain terminalWidth (P.border 2 msg)
        output rendered
        when (Output.isNumberedFailure o) $
          if errOk then writeIORef hasErrors True
          else dieWithMsg rendered
        pure numberedArgs

      -- Looks at the current stanza and decides if it is contained in the
      -- output so far. Appends it if not.
      appendFailingStanza :: IO ()
      appendFailingStanza = do
        stanzaOpt <- readIORef mStanza
        currentOut <- readIORef out
        let stnz = maybe "" show (fmap fst stanzaOpt :: Maybe Stanza)
        unless (stnz `isSubsequenceOf` concat currentOut) $
          modifyIORef' out (\acc -> acc <> pure stnz)

      -- output ``` and new lines then call transcriptFailure
      dieWithMsg :: forall a. String -> IO a
      dieWithMsg msg = do
        executable <- getProgName
        output "\n```\n\n"
        appendFailingStanza
        transcriptFailure out $ Text.unlines [
          "\128721", "",
          "The transcript failed due to an error in the stanza above. The error is:", "",
          Text.pack msg, "",
          "Run `" <> Text.pack executable <> " --codebase " <> Text.pack dir <> "` " <> "to do more work with it."]

      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        executable <- getProgName
        errOk <- readIORef allowErrors
        hasErr <- readIORef hasErrors
        when (errOk && not hasErr) $ do
          output "\n```\n\n"
          appendFailingStanza
          transcriptFailure out $ Text.unlines [
            "\128721", "",
            "The transcript was expecting an error in the stanza above, but did not encounter one.", "",
            "Run `" <> Text.pack executable <> " --codebase " <> Text.pack dir <> "` " <> "to do more work with it."]

      loop state = do
        writeIORef pathRef (view LoopState.currentPath state)
        let free = runStateT (runMaybeT HandleInput.loop) state
            rng i = pure $ Random.drgNewSeed (Random.seedFromInteger (fromIntegral i))
        (o, state') <- HandleCommand.commandLine config awaitInput
                                     (const $ pure ())
                                     runtime
                                     print
                                     printNumbered
                                     loadPreviousUnisonBlock
                                     codebase
                                     Nothing
                                     rng
                                     free
        case o of
          Nothing -> do
            texts <- readIORef out
            pure $ Text.concat (Text.pack <$> toList (texts :: Seq String))
          Just () -> do
            writeIORef numberedArgsRef (LoopState._numberedArgs state')
            writeIORef rootBranchRef (LoopState._root state')
            loop state'
    (`finally` cleanup)
      $ loop (LoopState.loopState0 root initialPath)

transcriptFailure :: IORef (Seq String) -> Text -> IO b
transcriptFailure out msg = do
  texts <- readIORef out
  die
    .  Text.unpack
    $  Text.concat (Text.pack <$> toList (texts :: Seq String))
    <> "\n\n"
    <> msg

type P = P.Parsec () Text

stanzas :: P [Stanza]
stanzas = P.many (fenced <|> unfenced)

ucmLine :: P UcmLine
ucmLine = ucmCommand <|> ucmComment
  where
    ucmCommand :: P UcmLine
    ucmCommand = do
      P.lookAhead (word ".")
      path <- P.takeWhile1P Nothing (/= '>')
      void $ word ">"
      line <- P.takeWhileP Nothing (/= '\n') <* spaces
      path <- case Path.parsePath' (Text.unpack path) of
        Right (Path.unPath' -> Left abs) -> pure abs
        Right _ -> fail "expected absolute path"
        Left e -> fail e
      pure $ UcmCommand path line

    ucmComment :: P UcmLine
    ucmComment = do
      word "--"
      line <- P.takeWhileP Nothing (/= '\n') <* spaces
      pure $ UcmComment line

fenced :: P Stanza
fenced = do
  fence
  fenceType <- lineToken(word "ucm" <|> word "unison" <|> language)
  stanza <-
    if fenceType == "ucm" then do
      hide <- hidden
      err <- expectingError
      _ <- spaces
      cmds <- many ucmLine
      pure $ Ucm hide err cmds
    else if fenceType == "unison" then do
      -- todo: this has to be more interesting
      -- ```unison:hide
      -- ```unison
      -- ```unison:hide:all scratch.u
      hide <- lineToken hidden
      err <- lineToken expectingError
      fileName <- optional untilSpace1
      blob <- spaces *> untilFence
      pure $ Unison hide err fileName blob
    else UnprocessedFence fenceType <$> untilFence
  fence
  pure stanza

-- Three backticks, consumes trailing spaces too
-- ```
fence :: P ()
fence = P.try $ do void (word "```"); spaces

-- Parses up until next fence
unfenced :: P Stanza
unfenced = Unfenced <$> untilFence

untilFence :: P Text
untilFence = do
  _ <- P.lookAhead (P.takeP Nothing 1)
  go mempty
  where
  go :: Seq Text -> P Text
  go !acc = do
    f <- P.lookAhead (P.optional fence)
    case f of
      Nothing -> do
        oneOrTwoBackticks <- optional (word' "``" <|> word' "`")
        let start = fromMaybe "" oneOrTwoBackticks
        txt <- P.takeWhileP (Just "unfenced") (/= '`')
        eof <- P.lookAhead (P.optional P.eof)
        case eof of
          Just _ -> pure $ fold (acc <> pure txt)
          Nothing -> go (acc <> pure start <> pure txt)
      Just _ -> pure $ fold acc

word' :: Text -> P Text
word' txt = P.try $ do
  chs <- P.takeP (Just $ show txt) (Text.length txt)
  guard (chs == txt)
  pure txt

word :: Text -> P Text
word = word'

-- token :: P a -> P a
-- token p = p <* spaces

lineToken :: P a -> P a
lineToken p = p <* nonNewlineSpaces

nonNewlineSpaces :: P ()
nonNewlineSpaces = void $ P.takeWhileP Nothing (\ch -> ch `elem` (" \t" :: String))

hidden :: P Hidden
hidden = (\case Just x -> x; Nothing -> Shown) <$> optional go where
  go = ((\_ -> HideAll) <$> (word ":hide:all")) <|>
       ((\_ -> HideOutput) <$> (word ":hide"))

expectingError :: P ExpectingError
expectingError = isJust <$> optional (word ":error")

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

language :: P Text
language = P.takeWhileP Nothing (\ch -> Char.isDigit ch || Char.isLower ch || ch == '_' )

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace

-- single :: Char -> P Char
-- single t = P.satisfy (== t)
