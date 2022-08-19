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

import Control.Lens ((^.))
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
import qualified Ki
import qualified Network.HTTP.Client as HTTP
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import qualified System.IO as IO
import System.IO.Error (catchIOError)
import qualified Text.Megaparsec as P
import qualified Unison.Auth.CredentialManager as AuthN
import qualified Unison.Auth.HTTPClient as AuthN
import qualified Unison.Auth.Tokens as AuthN
import qualified Unison.Cli.Monad as Cli
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
  = UcmCommand Path.Absolute Text
  | UcmComment Text -- Text does not include the '--' prefix.

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
  show (UcmCommand path txt) = show path <> ">" <> Text.unpack txt
  show (UcmComment txt) = "--" ++ Text.unpack txt

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
  Left e -> Left . TranscriptParseError $ tShow e

type TranscriptRunner =
  ( String ->
    Text ->
    (FilePath, Codebase IO Symbol Ann) ->
    IO (Either TranscriptError Text)
  )

withTranscriptRunner ::
  forall m r.
  UnliftIO.MonadUnliftIO m =>
  UCMVersion ->
  Maybe FilePath ->
  (TranscriptRunner -> m r) ->
  m r
withTranscriptRunner ucmVersion configFile action = do
  withRuntime $ \runtime sbRuntime -> withConfig $ \config -> do
    action $ \transcriptName transcriptSrc (codebaseDir, codebase) -> do
      Server.startServer (Backend.BackendEnv {Backend.useNamesIndex = False}) Server.defaultCodebaseServerOpts runtime codebase $ \baseUrl -> do
        let parsed = parse transcriptName transcriptSrc
        result <- for parsed $ \stanzas -> do
          liftIO $ run codebaseDir stanzas codebase runtime sbRuntime config ucmVersion (tShow baseUrl)
        pure $ join @(Either TranscriptError) result
  where
    withRuntime :: ((Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> m a) -> m a)
    withRuntime action =
      UnliftIO.bracket
        (liftIO $ RTI.startRuntime False RTI.Persistent ucmVersion)
        (liftIO . Runtime.terminate)
        $ \runtime ->
          UnliftIO.bracket
            (liftIO $ RTI.startRuntime True RTI.Persistent ucmVersion)
            (liftIO . Runtime.terminate)
            (action runtime)
    withConfig :: forall a. ((Maybe Config -> m a) -> m a)
    withConfig action = do
      case configFile of
        Nothing -> action Nothing
        Just configFilePath -> do
          let loadConfig = liftIO $ do
                catchIOError (watchConfig configFilePath) $
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

      output, outputEcho :: (String -> IO ())
      output = output' False
      outputEcho = output' True

      apiRequest :: APIRequest -> IO ()
      apiRequest req = do
        output (show req <> "\n")
        case req of
          (APIComment {}) -> pure ()
          (GetRequest path) -> do
            req <- case HTTP.parseRequest (Text.unpack $ baseURL <> path) of
              Left err -> dieWithMsg (show err)
              Right req -> pure req
            respBytes <- HTTP.httpLbs req httpManager
            case Aeson.eitherDecode (HTTP.responseBody respBytes) of
              Right (v :: Aeson.Value) -> do
                let prettyBytes = Aeson.encodePretty' (Aeson.defConfig {Aeson.confCompare = compare}) v
                output . (<> "\n") . BL.unpack $ prettyBytes
              Left err -> dieWithMsg ("Error decoding response from " <> Text.unpack path <> ": " <> err)

      awaitInput :: Cli.LoopState -> IO (Either Event Input)
      awaitInput loopState = do
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
            awaitInput loopState
          -- ucm command to run
          Just (Just ucmLine) -> do
            case ucmLine of
              p@(UcmComment {}) -> do
                output ("\n" <> show p)
                awaitInput loopState
              p@(UcmCommand path lineTxt) -> do
                let curPath = loopState ^. #currentPath
                if curPath /= path
                  then do
                    atomically $ Q.undequeue cmdQueue (Just p)
                    pure $ Right (SwitchBranchI $ Just (Path.absoluteToPath' path))
                  else case words . Text.unpack $ lineTxt of
                    [] -> awaitInput loopState
                    args -> do
                      output ("\n" <> show p <> "\n")
                      let getRoot = fmap Branch.head . atomically $ readTMVar (loopState ^. #root)
                      parseInput getRoot curPath (loopState ^. #numberedArgs) patternMap args >>= \case
                        -- invalid command is treated as a failure
                        Left msg -> dieWithMsg $ Pretty.toPlain terminalWidth msg
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
              Just (s, idx) -> do
                putStr $
                  "\r⚙️   Processing stanza " ++ show idx ++ " of "
                    ++ show (length stanzas)
                    ++ "."
                IO.hFlush IO.stdout
                case s of
                  Unfenced _ -> do
                    output $ show s
                    awaitInput loopState
                  UnprocessedFence _ _ -> do
                    output $ show s
                    awaitInput loopState
                  Unison hide errOk filename txt -> do
                    writeIORef hidden hide
                    outputEcho $ show s
                    writeIORef allowErrors errOk
                    output "```ucm\n"
                    atomically . Q.enqueue cmdQueue $ Nothing
                    modifyIORef' unisonFiles (Map.insert (fromMaybe "scratch.u" filename) txt)
                    pure $ Left (UnisonFileChanged (fromMaybe "scratch.u" filename) txt)
                  API apiRequests -> do
                    output "```api\n"
                    for_ apiRequests apiRequest
                    output "```"
                    awaitInput loopState
                  Ucm hide errOk cmds -> do
                    writeIORef hidden hide
                    writeIORef allowErrors errOk
                    writeIORef hasErrors False
                    output "```ucm"
                    traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
                    atomically . Q.enqueue cmdQueue $ Nothing
                    awaitInput loopState

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
        input <- awaitInput s0
        Cli.runCli env s0 (HandleInput.loop input) >>= \case
          (Cli.Success (), s1) -> loop s1
          (Cli.Continue, s1) -> loop s1
          (Cli.HaltRepl, _) -> do
            texts <- readIORef out
            pure $ Text.concat (Text.pack <$> toList (texts :: Seq String))

  loop (Cli.loopState0 rootVar initialPath)

transcriptFailure :: IORef (Seq String) -> Text -> IO b
transcriptFailure out msg = do
  texts <- readIORef out
  UnliftIO.throwIO
    . TranscriptRunFailure
    $ Text.concat (Text.pack <$> toList (texts :: Seq String))
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
nonNewlineSpaces = void $ P.takeWhileP Nothing (\ch -> ch `elem` (" \t" :: String))

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
