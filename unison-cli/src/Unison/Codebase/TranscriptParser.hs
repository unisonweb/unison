{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- Parse and execute markdown transcripts.
-}
module Unison.Codebase.TranscriptParser
  ( Stanza (..),
    FenceType,
    ExpectingError,
    Hidden,
    TranscriptError (..),
    UcmLine (..),
    withTranscriptRunner,
    parse,
    parseFile,
  )
where

import Control.Lens (use, (?~), (^.))
import qualified Crypto.Random as Random
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Char as Char
import qualified Data.Configurator as Configurator
import Data.Configurator.Types (Config)
import Data.IORef
import Data.List (isSubsequenceOf)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.These (These (..))
import qualified Ki
import qualified Network.HTTP.Client as HTTP
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified System.IO as IO
import System.IO.Error (catchIOError)
import qualified Text.Megaparsec as P
import qualified U.Codebase.Sqlite.Operations as Operations
import qualified Unison.Auth.CredentialManager as AuthN
import qualified Unison.Auth.HTTPClient as AuthN
import qualified Unison.Auth.Tokens as AuthN
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch.Type as Branch
import qualified Unison.Codebase.Editor.HandleInput as HandleInput
import Unison.Codebase.Editor.Input (Event (UnisonFileChanged), Input (..))
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import qualified Unison.Codebase.Runtime as Runtime
import Unison.CommandLine
import Unison.CommandLine.InputPattern (InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import Unison.CommandLine.Welcome (asciiartUnison)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.CodebaseServer as Server
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Util.Pretty as Pretty
import qualified Unison.Util.TQueue as Q
import qualified UnliftIO
import UnliftIO.STM
import Prelude hiding (readFile, writeFile)

-- | Render transcript errors at a width of 65 chars.
terminalWidth :: Pretty.Width
terminalWidth = 65

-- | If provided, this access token will be used on all
-- requests which use the Authenticated HTTP Client; i.e. all codeserver interactions.
--
-- It's useful in scripted contexts or when running transcripts against a codeserver.
accessTokenEnvVarKey :: String
accessTokenEnvVarKey = "UNISON_SHARE_ACCESS_TOKEN"

type ExpectingError = Bool

type ScratchFileName = Text

type FenceType = Text

data Hidden = Shown | HideOutput | HideAll
  deriving (Eq, Show)

data UcmLine
  = UcmCommand UcmContext Text
  | UcmComment Text -- Text does not include the '--' prefix.

-- | Where a command is run: either loose code (.foo.bar.baz>) or a project branch (myproject/mybranch>).
data UcmContext
  = UcmContextLooseCode Path.Absolute
  | UcmContextProject (ProjectAndBranch ProjectName ProjectBranchName)

data APIRequest
  = GetRequest Text
  | APIComment Text

instance Show APIRequest where
  show (GetRequest txt) = "GET " <> Text.unpack txt
  show (APIComment txt) = "-- " <> Text.unpack txt

data Stanza
  = Ucm Hidden ExpectingError [UcmLine]
  | Unison Hidden ExpectingError (Maybe ScratchFileName) Text
  | API [APIRequest]
  | UnprocessedFence FenceType Text
  | Unfenced Text

instance Show UcmLine where
  show = \case
    UcmCommand context txt -> showContext context <> "> " <> Text.unpack txt
    UcmComment txt -> "--" ++ Text.unpack txt
    where
      showContext = \case
        UcmContextLooseCode path -> show path
        UcmContextProject (ProjectAndBranch project branch) -> Text.unpack (into @Text (These project branch))

instance Show Stanza where
  show s = case s of
    Ucm _ _ cmds ->
      unlines
        [ "```ucm",
          foldl (\x y -> x ++ show y) "" cmds,
          "```"
        ]
    Unison _hide _ fname txt ->
      unlines
        [ "```unison",
          case fname of
            Nothing -> Text.unpack txt <> "```\n"
            Just fname ->
              unlines
                [ "---",
                  "title: " <> Text.unpack fname,
                  "---",
                  Text.unpack txt,
                  "```",
                  ""
                ]
        ]
    API apiRequests ->
      "```api\n"
        <> ( apiRequests
               & fmap show
               & unlines
           )
        <> "```\n"
    UnprocessedFence typ txt ->
      unlines
        [ "```" <> Text.unpack typ,
          Text.unpack txt,
          "```",
          ""
        ]
    Unfenced txt -> Text.unpack txt

parseFile :: FilePath -> IO (Either TranscriptError [Stanza])
parseFile filePath = do
  exists <- doesFileExist filePath
  if exists
    then do
      txt <- readUtf8 filePath
      pure $ parse filePath txt
    else pure . Left . TranscriptParseError . Text.pack $ filePath <> " does not exist"

parse :: String -> Text -> Either TranscriptError [Stanza]
parse srcName txt = case P.parse (stanzas <* P.eof) srcName txt of
  Right a -> Right a
  Left e -> Left . TranscriptParseError . Text.pack . P.errorBundlePretty $ e

type TranscriptRunner =
  ( String ->
    Text ->
    (FilePath, Codebase IO Symbol Ann) ->
    IO (Either TranscriptError Text)
  )

withTranscriptRunner ::
  forall m r.
  (UnliftIO.MonadUnliftIO m) =>
  UCMVersion ->
  Maybe FilePath ->
  (TranscriptRunner -> m r) ->
  m r
withTranscriptRunner ucmVersion configFile action = do
  withRuntimes \runtime sbRuntime -> withConfig $ \config -> do
    action \transcriptName transcriptSrc (codebaseDir, codebase) -> do
      Server.startServer (Backend.BackendEnv {Backend.useNamesIndex = False}) Server.defaultCodebaseServerOpts runtime codebase $ \baseUrl -> do
        let parsed = parse transcriptName transcriptSrc
        result <- for parsed \stanzas -> do
          liftIO $ run codebaseDir stanzas codebase runtime sbRuntime config ucmVersion (tShow baseUrl)
        pure $ join @(Either TranscriptError) result
  where
    withRuntimes :: ((Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> m a) -> m a)
    withRuntimes action =
      RTI.withRuntime False RTI.Persistent ucmVersion \runtime -> do
        RTI.withRuntime True RTI.Persistent ucmVersion \sbRuntime -> do
          action runtime sbRuntime
    withConfig :: forall a. ((Maybe Config -> m a) -> m a)
    withConfig action = do
      case configFile of
        Nothing -> action Nothing
        Just configFilePath -> do
          let loadConfig = liftIO do
                catchIOError
                  (watchConfig configFilePath)
                  \_ -> die "Your .unisonConfig could not be loaded. Check that it's correct!"
          UnliftIO.bracket
            loadConfig
            (\(_config, cancelConfig) -> liftIO cancelConfig)
            (\(config, _cancelConfig) -> action (Just config))

run ::
  FilePath ->
  [Stanza] ->
  Codebase IO Symbol Ann ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Maybe Config ->
  UCMVersion ->
  Text ->
  IO (Either TranscriptError Text)
run dir stanzas codebase runtime sbRuntime config ucmVersion baseURL = UnliftIO.try $ Ki.scoped \scope -> do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  let initialPath = Path.absoluteEmpty
  putPrettyLn $
    Pretty.lines
      [ asciiartUnison,
        "",
        "Running the provided transcript file...",
        ""
      ]
  initialRootCausalHash <- Codebase.runTransaction codebase Operations.expectRootCausalHash
  rootVar <- newEmptyTMVarIO
  void $ Ki.fork scope do
    root <- Codebase.getRootBranch codebase
    atomically $ putTMVar rootVar root
  mayShareAccessToken <- fmap Text.pack <$> lookupEnv accessTokenEnvVarKey
  credMan <- AuthN.newCredentialManager
  let tokenProvider :: AuthN.TokenProvider
      tokenProvider =
        case mayShareAccessToken of
          Nothing -> do
            AuthN.newTokenProvider credMan
          Just accessToken ->
            \_codeserverID -> pure $ Right accessToken
  seedRef <- newIORef (0 :: Int)
  inputQueue <- Q.newIO
  cmdQueue <- Q.newIO
  unisonFiles <- newIORef Map.empty
  out <- newIORef mempty
  hidden <- newIORef Shown
  allowErrors <- newIORef False
  hasErrors <- newIORef False
  mStanza <- newIORef Nothing
  traverse_ (atomically . Q.enqueue inputQueue) (stanzas `zip` [1 :: Int ..])
  let patternMap =
        Map.fromList $
          validInputs
            >>= (\p -> (patternName p, p) : ((,p) <$> aliases p))
  let output' :: Bool -> String -> IO ()
      output' inputEcho msg = do
        hide <- readIORef hidden
        unless (hideOutput inputEcho hide) $ modifyIORef' out (\acc -> acc <> pure msg)

      hideOutput :: Bool -> Hidden -> Bool
      hideOutput inputEcho = \case
        Shown -> False
        HideOutput -> True && (not inputEcho)
        HideAll -> True

      output, outputEcho :: String -> IO ()
      output = output' False
      outputEcho = output' True

      apiRequest :: APIRequest -> IO ()
      apiRequest req = do
        output (show req <> "\n")
        case req of
          APIComment {} -> pure ()
          GetRequest path -> do
            req <- case HTTP.parseRequest (Text.unpack $ baseURL <> path) of
              Left err -> dieWithMsg (show err)
              Right req -> pure req
            respBytes <- HTTP.httpLbs req httpManager
            case Aeson.eitherDecode (HTTP.responseBody respBytes) of
              Right (v :: Aeson.Value) -> do
                let prettyBytes = Aeson.encodePretty' (Aeson.defConfig {Aeson.confCompare = compare}) v
                output . (<> "\n") . BL.unpack $ prettyBytes
              Left err -> dieWithMsg ("Error decoding response from " <> Text.unpack path <> ": " <> err)

      awaitInput :: Cli (Either Event Input)
      awaitInput = do
        cmd <- atomically (Q.tryDequeue cmdQueue)
        case cmd of
          -- end of ucm block
          Just Nothing -> do
            liftIO (output "\n```\n")
            -- We clear the file cache after each `ucm` stanza, so
            -- that `load` command can read the file written by `edit`
            -- rather than hitting the cache.
            liftIO (writeIORef unisonFiles Map.empty)
            liftIO dieUnexpectedSuccess
            awaitInput
          -- ucm command to run
          Just (Just ucmLine) -> do
            case ucmLine of
              p@(UcmComment {}) -> do
                liftIO (output ("\n" <> show p))
                awaitInput
              p@(UcmCommand context lineTxt) -> do
                curPath <- Cli.getCurrentPath
                -- We're either going to run the command now (because we're in the right context), else we'll switch to
                -- the right context first, then run the command next.
                maybeSwitchCommand <-
                  case context of
                    UcmContextLooseCode path ->
                      if curPath == path
                        then pure Nothing
                        else pure $ Just (SwitchBranchI $ Just (Path.absoluteToPath' path))
                    UcmContextProject (ProjectAndBranch projectName branchName) -> do
                      ProjectAndBranch project branch <-
                        ProjectUtils.expectProjectAndBranchByTheseNames (These projectName branchName)
                      let projectAndBranchIds = ProjectAndBranch (project ^. #projectId) (branch ^. #branchId)
                      if curPath == ProjectUtils.projectBranchPath projectAndBranchIds
                        then pure Nothing
                        else pure (Just (ProjectSwitchI (These projectName branchName)))
                case maybeSwitchCommand of
                  Just switchCommand -> do
                    atomically $ Q.undequeue cmdQueue (Just p)
                    pure (Right switchCommand)
                  Nothing -> do
                    case words . Text.unpack $ lineTxt of
                      [] -> awaitInput
                      args -> do
                        liftIO (output ("\n" <> show p <> "\n"))
                        rootVar <- use #root
                        numberedArgs <- use #numberedArgs
                        let getRoot = fmap Branch.head . atomically $ readTMVar rootVar
                        liftIO (parseInput getRoot curPath numberedArgs patternMap args) >>= \case
                          -- invalid command is treated as a failure
                          Left msg -> liftIO (dieWithMsg $ Pretty.toPlain terminalWidth msg)
                          Right input -> pure $ Right input
          Nothing -> do
            liftIO (dieUnexpectedSuccess)
            liftIO (writeIORef hidden Shown)
            liftIO (writeIORef allowErrors False)
            maybeStanza <- atomically (Q.tryDequeue inputQueue)
            _ <- liftIO (writeIORef mStanza maybeStanza)
            case maybeStanza of
              Nothing -> do
                liftIO (putStrLn "")
                pure $ Right QuitI
              Just (s, idx) -> do
                liftIO . putStr $
                  "\r⚙️   Processing stanza "
                    ++ show idx
                    ++ " of "
                    ++ show (length stanzas)
                    ++ "."
                liftIO (IO.hFlush IO.stdout)
                case s of
                  Unfenced _ -> do
                    liftIO (output $ show s)
                    awaitInput
                  UnprocessedFence _ _ -> do
                    liftIO (output $ show s)
                    awaitInput
                  Unison hide errOk filename txt -> do
                    liftIO (writeIORef hidden hide)
                    liftIO (outputEcho $ show s)
                    liftIO (writeIORef allowErrors errOk)
                    liftIO (output "```ucm\n")
                    atomically . Q.enqueue cmdQueue $ Nothing
                    liftIO (modifyIORef' unisonFiles (Map.insert (fromMaybe "scratch.u" filename) txt))
                    pure $ Left (UnisonFileChanged (fromMaybe "scratch.u" filename) txt)
                  API apiRequests -> do
                    liftIO (output "```api\n")
                    liftIO (for_ apiRequests apiRequest)
                    liftIO (output "```")
                    awaitInput
                  Ucm hide errOk cmds -> do
                    liftIO (writeIORef hidden hide)
                    liftIO (writeIORef allowErrors errOk)
                    liftIO (writeIORef hasErrors False)
                    liftIO (output "```ucm")
                    traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
                    atomically . Q.enqueue cmdQueue $ Nothing
                    awaitInput

      loadPreviousUnisonBlock name = do
        ufs <- readIORef unisonFiles
        case Map.lookup name ufs of
          Just uf ->
            return (Cli.LoadSuccess uf)
          Nothing ->
            -- This lets transcripts use the `load` command, as in:
            --
            -- .> load someFile.u
            --
            -- Important for Unison syntax that can't be embedded in
            -- transcripts (like docs, which use ``` in their syntax).
            let f = Cli.LoadSuccess <$> readUtf8 (Text.unpack name)
             in f <|> pure Cli.InvalidSourceNameError

      print :: Output.Output -> IO ()
      print o = do
        msg <- notifyUser dir o
        errOk <- readIORef allowErrors
        let rendered = Pretty.toPlain terminalWidth (Pretty.border 2 msg)
        output rendered
        when (Output.isFailure o) $
          if errOk
            then writeIORef hasErrors True
            else dieWithMsg rendered

      printNumbered :: Output.NumberedOutput -> IO Output.NumberedArgs
      printNumbered o = do
        let (msg, numberedArgs) = notifyNumbered o
        errOk <- readIORef allowErrors
        let rendered = Pretty.toPlain terminalWidth (Pretty.border 2 msg)
        output rendered
        when (Output.isNumberedFailure o) $
          if errOk
            then writeIORef hasErrors True
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
        output "\n```\n\n"
        appendFailingStanza
        transcriptFailure out $
          Text.unlines
            [ "\128721",
              "",
              "The transcript failed due to an error in the stanza above. The error is:",
              "",
              Text.pack msg
            ]

      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        errOk <- readIORef allowErrors
        hasErr <- readIORef hasErrors
        when (errOk && not hasErr) $ do
          output "\n```\n\n"
          appendFailingStanza
          transcriptFailure out $
            Text.unlines
              [ "\128721",
                "",
                "The transcript was expecting an error in the stanza above, but did not encounter one."
              ]

  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion

  let env =
        Cli.Env
          { authHTTPClient = authenticatedHTTPClient,
            codebase,
            config = fromMaybe Configurator.empty config,
            credentialManager = credMan,
            generateUniqueName = do
              i <- atomicModifyIORef' seedRef \i -> let !i' = i + 1 in (i', i)
              pure (Parser.uniqueBase32Namegen (Random.drgNewSeed (Random.seedFromInteger (fromIntegral i)))),
            loadSource = loadPreviousUnisonBlock,
            notify = print,
            notifyNumbered = printNumbered,
            runtime,
            sandboxedRuntime = sbRuntime,
            serverBaseUrl = Nothing,
            ucmVersion
          }

  let loop :: Cli.LoopState -> IO Text
      loop s0 = do
        Cli.runCli env s0 awaitInput >>= \case
          (Cli.Success input, s1) -> do
            let next s =
                  loop case input of
                    Left _ -> s
                    Right inp -> s & #lastInput ?~ inp
            Cli.runCli env s1 (HandleInput.loop input) >>= \case
              (Cli.Success (), s2) -> next s2
              (Cli.Continue, s2) -> next s2
              (Cli.HaltRepl, _) -> onHalt
          (Cli.Continue, s1) -> loop s1
          (Cli.HaltRepl, _) -> onHalt
        where
          onHalt = do
            texts <- readIORef out
            pure $ Text.concat (Text.pack <$> toList (texts :: Seq String))

  loop (Cli.loopState0 initialRootCausalHash rootVar initialPath)

transcriptFailure :: IORef (Seq String) -> Text -> IO b
transcriptFailure out msg = do
  texts <- readIORef out
  UnliftIO.throwIO
    . TranscriptRunFailure
    $ Text.concat (Text.pack <$> toList texts)
      <> "\n\n"
      <> msg

type P = P.Parsec Void Text

stanzas :: P [Stanza]
stanzas = P.many (fenced <|> unfenced)

ucmLine :: P UcmLine
ucmLine = ucmCommand <|> ucmComment
  where
    ucmCommand :: P UcmLine
    ucmCommand = do
      context <-
        P.try do
          contextString <- P.takeWhile1P Nothing (/= '>')
          context <-
            case (tryFrom @Text contextString, Path.parsePath' (Text.unpack contextString)) of
              (Right (These project branch), _) -> pure (UcmContextProject (ProjectAndBranch project branch))
              (Left _, Right (Path.unPath' -> Left abs)) -> pure (UcmContextLooseCode abs)
              _ -> fail "expected project/branch or absolute path"
          void $ lineToken $ word ">"
          pure context
      line <- P.takeWhileP Nothing (/= '\n') <* spaces
      pure $ UcmCommand context line

    ucmComment :: P UcmLine
    ucmComment = do
      word "--"
      line <- P.takeWhileP Nothing (/= '\n') <* spaces
      pure $ UcmComment line

apiRequest :: P APIRequest
apiRequest = do
  apiComment <|> getRequest
  where
    getRequest = do
      word "GET"
      spaces
      path <- P.takeWhile1P Nothing (/= '\n')
      spaces
      pure (GetRequest path)
    apiComment = do
      word "--"
      comment <- P.takeWhileP Nothing (/= '\n')
      spaces
      pure (APIComment comment)

fenced :: P Stanza
fenced = do
  fence
  fenceType <- lineToken (word "ucm" <|> word "unison" <|> word "api" <|> language)
  stanza <-
    case fenceType of
      "ucm" -> do
        hide <- hidden
        err <- expectingError
        _ <- spaces
        cmds <- many ucmLine
        pure $ Ucm hide err cmds
      "unison" ->
        do
          -- todo: this has to be more interesting
          -- ```unison:hide
          -- ```unison
          -- ```unison:hide:all scratch.u
          hide <- lineToken hidden
          err <- lineToken expectingError
          fileName <- optional untilSpace1
          blob <- spaces *> untilFence
          pure $ Unison hide err fileName blob
      "api" -> do
        _ <- spaces
        apiRequests <- many apiRequest
        pure $ API apiRequests
      _ -> UnprocessedFence fenceType <$> untilFence
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
nonNewlineSpaces = void $ P.takeWhileP Nothing (\ch -> ch == ' ' || ch == '\t')

hidden :: P Hidden
hidden = (\case Just x -> x; Nothing -> Shown) <$> optional go
  where
    go =
      ((\_ -> HideAll) <$> (word ":hide:all"))
        <|> ((\_ -> HideOutput) <$> (word ":hide"))

expectingError :: P ExpectingError
expectingError = isJust <$> optional (word ":error")

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

language :: P Text
language = P.takeWhileP Nothing (\ch -> Char.isDigit ch || Char.isLower ch || ch == '_')

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace

-- single :: Char -> P Char
-- single t = P.satisfy (== t)

data TranscriptError
  = TranscriptRunFailure Text
  | TranscriptParseError Text
  deriving stock (Show)
  deriving anyclass (Exception)
