{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}
{-# Language ViewPatterns #-}

module Unison.Codebase.TranscriptParser
  ( parse
  , parseFile
  , run
  )
where

import           Control.Concurrent.STM         ( atomically )
import           Control.Exception              ( finally )
import           Control.Lens                   ( view )
import           Control.Monad.State            ( runStateT )
import           Data.Bifunctor                 ( first )
import           Data.IORef
import           Data.List                      ( isSubsequenceOf )
import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( die )
import           System.IO.Error                ( catchIOError )
import           Unison.Codebase                ( Codebase )
import           Unison.Codebase.Editor.Command ( LoadSourceResult(..) )
import           Unison.Codebase.Editor.Input   ( Input(..)
                                                , Event(UnisonFileChanged)
                                                )
import           Unison.CommandLine
import           Unison.CommandLine.InputPattern
                                                ( InputPattern
                                                  ( aliases
                                                  , patternName
                                                  )
                                                )
import           Unison.CommandLine.Main        ( asciiartUnison
                                                , expandNumber
                                                )
import           Unison.CommandLine.InputPatterns
                                                ( validInputs )
import           Unison.CommandLine.OutputMessages
                                                ( notifyUser
                                                , notifyNumbered
                                                )
import           Unison.Parser                  ( Ann )
import           Unison.Prelude
import           Unison.PrettyTerminal
import           Unison.Symbol                  ( Symbol )
import qualified Crypto.Random                 as Random
import qualified Data.Char                     as Char
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified System.IO                     as IO
import qualified Text.Megaparsec               as P
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Editor.HandleCommand
                                               as HandleCommand
import qualified Unison.Codebase.Editor.HandleInput
                                               as HandleInput
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.Runtime       as Runtime
import qualified Unison.CommandLine.InputPattern
                                               as IP
import qualified Unison.Runtime.Rt1IO          as Rt1
import qualified Unison.Util.Pretty            as P
import qualified Unison.Util.TQueue            as Q

type ExpectingError = Bool
type Err = String
type ScratchFileName = Text
type FenceType = Text

data Hidden = Shown | HideOutput | HideAll
            deriving (Eq, Show)
data UcmCommand = UcmCommand Path.Absolute Text

data Stanza
  = Ucm Hidden ExpectingError [UcmCommand]
  | Unison Hidden ExpectingError (Maybe ScratchFileName) Text
  | UnprocessedFence FenceType Text
  | Unfenced Text

instance Show UcmCommand where
  show (UcmCommand path txt) = show path <> ">" <> Text.unpack txt

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
  if exists then
    parse filePath <$> readUtf8 filePath
  else
    pure $ Left $ show filePath ++ " does not exist"

parse :: String -> Text -> Either Err [Stanza]
parse srcName txt =
  first show $ P.parse (stanzas <* P.eof) srcName txt

run :: FilePath -> FilePath -> [Stanza] -> Codebase IO Symbol Ann -> IO Text
run dir configFile stanzas codebase = do
  let initialPath = Path.absoluteEmpty
  let startRuntime = pure Rt1.runtime
  putPrettyLn $ P.lines [
    asciiartUnison, "",
    "Running the provided transcript file...",
    ""
    ]
  root <- Codebase.getRootBranch codebase
  do
    runtime                  <- startRuntime
    pathRef                  <- newIORef initialPath
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
    traverse_ (atomically . Q.enqueue inputQueue) (stanzas `zip` [1 :: Int ..])
    let patternMap =
          Map.fromList
            $   validInputs
            >>= (\p -> (patternName p, p) : ((, p) <$> aliases p))
    let
      appendOutput :: Bool -> Hidden -> Bool
      appendOutput echoInput = \case
        Shown      -> True
        HideOutput -> echoInput -- True makes me Shown, False makes me HideAll
        HideAll    -> False

      withToggledHide :: Bool -> String -> IO ()
      withToggledHide echoInput msg = do
        hide <- readIORef hidden
        when (appendOutput echoInput hide) $ modifyIORef' out (\acc -> acc <> pure msg)

      -- Append this string to the output when the block is visible.
      -- The output will be discarded when 'hide' and 'hide:all' are specified.
      output :: String -> IO ()
      output = withToggledHide False

      -- Append this string to the output when the block is visible or hidden.
      -- The output will be discarded when 'hide:all' is specified.
      outputEcho :: String -> IO ()
      outputEcho = withToggledHide True

      awaitInput :: IO (Either Event Input)
      awaitInput = do

        cmd <- atomically (Q.tryDequeue cmdQueue)

        case cmd of

          Just Nothing -> do
            output "\n```\n"
            writeIORef hidden Shown
            awaitInput
          Just (Just p@(UcmCommand path lineTxt)) -> do
            curPath <- readIORef pathRef
            numberedArgs <- readIORef numberedArgsRef
            if curPath /= path then do
              atomically $ Q.undequeue cmdQueue (Just p)
              pure $ Right (SwitchBranchI (Path.absoluteToPath' path))
            else case (>>= expandNumber numberedArgs)
                       . words . Text.unpack $ lineTxt of
              [] -> awaitInput
              cmd:args -> do
                output ("\n" <> show p <> "\n")
                -- invalid command is treated as a failure
                case Map.lookup cmd patternMap of
                  Nothing ->
                    dieWithMsg
                  Just pat -> case IP.parse pat args of
                    Left msg -> do
                      output $ P.toPlain 65 (P.indentN 2 msg <> P.newline <> P.newline)
                      dieWithMsg
                    Right input -> do
                      dieUnexpectedSuccess 
                      pure $ Right input

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
                    output "```ucm"
                    traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
                    atomically . Q.enqueue cmdQueue $ Nothing
                    awaitInput

      loadPreviousUnisonBlock :: ScratchFileName -> IO LoadSourceResult
      loadPreviousUnisonBlock name = do
        ufs <- readIORef unisonFiles
        case Map.lookup name ufs of
          Just uf -> return $ LoadSuccess uf
          Nothing -> return InvalidSourceNameError

      cleanup :: IO ()
      cleanup = do Runtime.terminate runtime; cancelConfig

      print o = do
        msg <- notifyUser dir o
        errOk <- readIORef allowErrors
        let rendered = P.toPlain 65 (P.border 2 msg)
        output rendered
        when (Output.isFailure o) $
          if errOk then writeIORef hasErrors True
          else dieWithMsg

      printNumbered o = do
        let (msg, numberedArgs) = notifyNumbered o
        errOk <- readIORef allowErrors
        let rendered = P.toPlain 65 (P.border 2 msg)
        output rendered
        when (Output.isNumberedFailure o) $
          if errOk then writeIORef hasErrors True
          else dieWithMsg
        pure numberedArgs

      -- output ``` and new lines then call transcriptFailure
      dieWithMsg :: forall a. IO a
      dieWithMsg = do
        output "\n```\n\n"
        stanzaOpt <- readIORef mStanza
        currentOut <- readIORef out
        let stnz = maybe "" show (fmap fst stanzaOpt :: Maybe Stanza)
        unless (stnz `isSubsequenceOf` concat currentOut) $
          modifyIORef' out (\acc -> acc <> pure stnz)

        transcriptFailure out $ Text.unlines [
          "\128721", "",
          "The transcript failed due to an error while running the stanza above.", "",
          "Run `ucm -codebase " <> Text.pack dir <> "` " <> "to do more work with it."]
        
      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        errOk <- readIORef allowErrors
        hasErr <- readIORef hasErrors
        when (errOk && not hasErr) $ do
          output "\n```\n\n"
          transcriptFailure out $ Text.unlines [
            "\128721", "",
            "Transcript failed due to an unexpected success above.", "",
            "Run `ucm -codebase " <> Text.pack dir <> "` " <> "to do more work with it."]

      loop :: HandleInput.LoopState IO Symbol -> IO Text
      loop state = do
        writeIORef pathRef (view HandleInput.currentPath state)
        let free = runStateT (runMaybeT HandleInput.loop) state
            rng i = pure $ Random.drgNewSeed (Random.seedFromInteger (fromIntegral i))
        (o, state') <- HandleCommand.commandLine config awaitInput
                                     (const $ pure ())
                                     runtime
                                     print
                                     printNumbered
                                     loadPreviousUnisonBlock
                                     codebase
                                     rng
                                     free
        case o of
          Nothing -> do
            texts <- readIORef out
            pure $ Text.concat (Text.pack <$> toList (texts :: Seq String))
          Just () -> do
            writeIORef numberedArgsRef (HandleInput._numberedArgs state')
            loop state'

    (`finally` cleanup) $ loop (HandleInput.loopState0 root initialPath)

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

ucmCommand :: P UcmCommand
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

fenced :: P Stanza
fenced = do
  fence
  fenceType <- lineToken(word "ucm" <|> word "unison" <|> language)
  stanza <-
    if fenceType == "ucm" then do
      hide <- hidden
      err <- expectingError
      _ <- spaces
      cmds <- many ucmCommand
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
        oneOrTwoBackticks <- optional (word "``" <|> word "`")
        let start = fromMaybe "" oneOrTwoBackticks
        txt <- P.takeWhileP (Just "unfenced") (/= '`')
        eof <- P.lookAhead (P.optional P.eof)
        case eof of
          Just _ -> pure $ fold (acc <> pure txt)
          Nothing -> go (acc <> pure start <> pure txt)
      Just _ -> pure $ fold acc

word :: Text -> P Text
word txt = P.try $ do
  chs <- P.takeP (Just $ show txt) (Text.length txt)
  guard (chs == txt)
  pure txt

lineToken :: P a -> P a
lineToken p = p <* nonNewlineSpaces

nonNewlineSpaces :: P ()
nonNewlineSpaces = void $ P.takeWhileP Nothing (\ch -> ch `elem` (" \t" :: String))

hidden :: P Hidden
hidden = (const HideAll <$> word ":hide:all") <|> (const HideOutput <$> word ":hide") <|> pure Shown

expectingError :: P ExpectingError
expectingError = isJust <$> optional (word ":error")

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

language :: P Text
language = P.takeWhileP Nothing (\ch -> Char.isDigit ch || Char.isLower ch || ch == '_' )

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace
