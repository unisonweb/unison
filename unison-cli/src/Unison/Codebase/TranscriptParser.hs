{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Parse and execute CommonMark (like Github-flavored Markdown) transcripts.
module Unison.Codebase.TranscriptParser
  ( Error (..),
    Runner,
    withRunner,
  )
where

import CMark qualified
import Control.Lens (use, (?~))
import Crypto.Random qualified as Random
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Char qualified as Char
import Data.Configurator qualified as Configurator
import Data.Configurator.Types (Config)
import Data.IORef
import Data.List (isSubsequenceOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.These (These (..))
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Client qualified as HTTP
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO qualified as IO
import System.IO.Error (catchIOError)
import Text.Megaparsec qualified as P
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event (UnisonFileChanged), Input (..))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.Verbosity (Verbosity, isSilent)
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine
import Unison.CommandLine.InputPattern (InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import Unison.CommandLine.Welcome (asciiartUnison)
import Unison.Core.Project (ProjectBranchName, ProjectName (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (ProjectAndBranchNames'Unambiguous))
import Unison.Runtime.Interface qualified as RTI
import Unison.Server.Backend qualified as Backend
import Unison.Server.CodebaseServer qualified as Server
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.TQueue qualified as Q
import UnliftIO qualified
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

data Hidden = Shown | HideOutput | HideAll
  deriving (Eq, Show)

data UcmLine
  = UcmCommand UcmContext Text
  | UcmComment Text -- Text does not include the '--' prefix.

-- | Where a command is run: either loose code (.foo.bar.baz>) or a project branch (myproject/mybranch>).
data UcmContext
  = UcmContextProject (ProjectAndBranch ProjectName ProjectBranchName)

data APIRequest
  = GetRequest Text
  | APIComment Text

formatAPIRequest :: APIRequest -> Text
formatAPIRequest = \case
  GetRequest txt -> "GET " <> txt
  APIComment txt -> "-- " <> txt

pattern CMarkCodeBlock :: (Maybe CMark.PosInfo) -> Text -> Text -> CMark.Node
pattern CMarkCodeBlock pos info body = CMark.Node pos (CMark.CODE_BLOCK info body) []

type Stanza = Either CMark.Node ProcessedBlock

data ProcessedBlock
  = Ucm Hidden ExpectingError [UcmLine]
  | Unison Hidden ExpectingError (Maybe ScratchFileName) Text
  | API [APIRequest]

formatUcmLine :: UcmLine -> Text
formatUcmLine = \case
  UcmCommand context txt -> formatContext context <> "> " <> txt
  UcmComment txt -> "--" <> txt
  where
    formatContext (UcmContextProject projectAndBranch) = into @Text projectAndBranch

formatStanza :: Stanza -> Text
formatStanza = either formatNode formatProcessedBlock

formatNode :: CMark.Node -> Text
formatNode = (<> "\n") . CMark.nodeToCommonmark [] Nothing

formatProcessedBlock :: ProcessedBlock -> Text
formatProcessedBlock = formatNode . processedBlockToNode

processedBlockToNode :: ProcessedBlock -> CMark.Node
processedBlockToNode = \case
  Ucm _ _ cmds -> CMarkCodeBlock Nothing "ucm" $ foldr ((<>) . formatUcmLine) "" cmds
  Unison _hide _ fname txt ->
    CMarkCodeBlock Nothing "unison" $ maybe txt (\fname -> Text.unlines ["---", "title: " <> fname, "---", txt]) fname
  API apiRequests -> CMarkCodeBlock Nothing "api" $ Text.unlines $ formatAPIRequest <$> apiRequests

parse :: FilePath -> Text -> Either Error [Stanza]
parse srcName = first ParseError . stanzas srcName

type Runner =
  String ->
  Text ->
  (FilePath, Codebase IO Symbol Ann) ->
  IO (Either Error Text)

withRunner ::
  forall m r.
  (UnliftIO.MonadUnliftIO m) =>
  Bool {- Whether to treat this transcript run as a transcript test, which will try to make output deterministic -} ->
  Verbosity ->
  UCMVersion ->
  FilePath ->
  Maybe FilePath ->
  (Runner -> m r) ->
  m r
withRunner isTest verbosity ucmVersion nrtp configFile action = do
  withRuntimes nrtp \runtime sbRuntime nRuntime -> withConfig \config -> do
    action \transcriptName transcriptSrc (codebaseDir, codebase) -> do
      Server.startServer (Backend.BackendEnv {Backend.useNamesIndex = False}) Server.defaultCodebaseServerOpts runtime codebase \baseUrl -> do
        let parsed = parse transcriptName transcriptSrc
        result <- for parsed \stanzas -> do
          liftIO $ run isTest verbosity codebaseDir stanzas codebase runtime sbRuntime nRuntime config ucmVersion (tShow baseUrl)
        pure $ join @(Either Error) result
  where
    withRuntimes ::
      FilePath -> (Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> m a) -> m a
    withRuntimes nrtp action =
      RTI.withRuntime False RTI.Persistent ucmVersion \runtime -> do
        RTI.withRuntime True RTI.Persistent ucmVersion \sbRuntime -> do
          action runtime sbRuntime
            =<< liftIO (RTI.startNativeRuntime ucmVersion nrtp)
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
  Bool {- Whether to treat this transcript run as a transcript test, which will try to make output deterministic -} ->
  Verbosity ->
  FilePath ->
  [Stanza] ->
  Codebase IO Symbol Ann ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Maybe Config ->
  UCMVersion ->
  Text ->
  IO (Either Error Text)
run isTest verbosity dir stanzas codebase runtime sbRuntime nRuntime config ucmVersion baseURL = UnliftIO.try do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  (initialPP, emptyCausalHashId) <- Codebase.runTransaction codebase do
    (_, emptyCausalHashId) <- Codebase.emptyCausalHash
    initialPP <- Codebase.expectCurrentProjectPath
    pure (initialPP, emptyCausalHashId)

  unless (isSilent verbosity) . putPrettyLn $
    Pretty.lines
      [ asciiartUnison,
        "",
        "Running the provided transcript file...",
        ""
      ]
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
  -- Queue of Stanzas and Just index, or Nothing if the stanza was programmatically generated
  -- e.g. a unison-file update by a command like 'edit'
  inputQueue <- Q.newIO @(Stanza, Maybe Int)
  -- Queue of UCM commands to run.
  -- Nothing indicates the end of a ucm block.
  cmdQueue <- Q.newIO @(Maybe UcmLine)
  -- Queue of scratch file updates triggered by UCM itself, e.g. via `edit`, `update`, etc.
  ucmScratchFileUpdatesQueue <- Q.newIO @(ScratchFileName, Text)
  unisonFiles <- newIORef Map.empty
  out <- newIORef mempty
  hidden <- newIORef Shown
  allowErrors <- newIORef False
  hasErrors <- newIORef False
  mStanza <- newIORef Nothing
  traverse_ (atomically . Q.enqueue inputQueue) (stanzas `zip` (Just <$> [1 :: Int ..]))
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
        output . Text.unpack $ formatAPIRequest req <> "\n"
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
            liftIO dieUnexpectedSuccess
            atomically $ void $ do
              scratchFileUpdates <- Q.flush ucmScratchFileUpdatesQueue
              -- Push them onto the front stanza queue in the correct order.
              for (reverse scratchFileUpdates) \(fp, contents) -> do
                let fenceDescription = "unison:added-by-ucm " <> fp
                -- Output blocks for any scratch file updates the ucm block triggered.
                Q.undequeue inputQueue (Left $ CMarkCodeBlock Nothing fenceDescription contents, Nothing)
            awaitInput
          -- ucm command to run
          Just (Just ucmLine) -> do
            case ucmLine of
              p@(UcmComment {}) -> do
                liftIO . output . Text.unpack $ "\n" <> formatUcmLine p
                awaitInput
              p@(UcmCommand context lineTxt) -> do
                curPath <- Cli.getCurrentProjectPath
                -- We're either going to run the command now (because we're in the right context), else we'll switch to
                -- the right context first, then run the command next.
                maybeSwitchCommand <-
                  case context of
                    UcmContextProject (ProjectAndBranch projectName branchName) -> Cli.runTransaction do
                      Project {projectId, name = projectName} <-
                        Q.loadProjectByName projectName
                          >>= \case
                            Nothing -> do
                              projectId <- Sqlite.unsafeIO (Db.ProjectId <$> UUID.nextRandom)
                              Q.insertProject projectId projectName
                              pure $ Project {projectId, name = projectName}
                            Just project -> pure project
                      projectBranch <-
                        Q.loadProjectBranchByName projectId branchName >>= \case
                          Nothing -> do
                            branchId <- Sqlite.unsafeIO (Db.ProjectBranchId <$> UUID.nextRandom)
                            let projectBranch = ProjectBranch {projectId, parentBranchId = Nothing, branchId, name = branchName}
                            Q.insertProjectBranch "Branch Created" emptyCausalHashId projectBranch
                            pure projectBranch
                          Just projBranch -> pure projBranch
                      let projectAndBranchIds = ProjectAndBranch projectBranch.projectId projectBranch.branchId
                      pure
                        if (PP.toProjectAndBranch . PP.toIds $ curPath) == projectAndBranchIds
                          then Nothing
                          else Just (ProjectSwitchI (ProjectAndBranchNames'Unambiguous (These projectName branchName)))
                case maybeSwitchCommand of
                  Just switchCommand -> do
                    atomically $ Q.undequeue cmdQueue (Just p)
                    pure (Right switchCommand)
                  Nothing -> do
                    case words . Text.unpack $ lineTxt of
                      [] -> awaitInput
                      args -> do
                        liftIO . output . Text.unpack $ "\n" <> formatUcmLine p <> "\n"
                        numberedArgs <- use #numberedArgs
                        PP.ProjectAndBranch projId branchId <- PP.toProjectAndBranch . NonEmpty.head <$> use #projectPathStack
                        let getProjectRoot = liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
                        liftIO (parseInput codebase curPath getProjectRoot numberedArgs patternMap args) >>= \case
                          -- invalid command is treated as a failure
                          Left msg -> do
                            liftIO $ writeIORef hasErrors True
                            liftIO (readIORef allowErrors) >>= \case
                              True -> do
                                liftIO (output . Pretty.toPlain terminalWidth $ ("\n" <> msg <> "\n"))
                                awaitInput
                              False -> do
                                liftIO (dieWithMsg $ Pretty.toPlain terminalWidth msg)
                          -- No input received from this line, try again.
                          Right Nothing -> awaitInput
                          Right (Just (_expandedArgs, input)) -> pure $ Right input
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
                unless (Verbosity.isSilent verbosity) . liftIO $ do
                  putStr $
                    "\r⚙️   Processing stanza "
                      ++ show idx
                      ++ " of "
                      ++ show (length stanzas)
                      ++ "."
                  IO.hFlush IO.stdout
                either
                  ( \node -> do
                      liftIO . output . Text.unpack $ formatNode node
                      awaitInput
                  )
                  ( \block -> case block of
                      Unison hide errOk filename txt -> do
                        liftIO (writeIORef hidden hide)
                        liftIO . outputEcho . Text.unpack $ formatProcessedBlock block
                        liftIO (writeIORef allowErrors errOk)
                        -- Open a ucm block which will contain the output from UCM
                        -- after processing the UnisonFileChanged event.
                        liftIO (output "``` ucm\n")
                        -- Close the ucm block after processing the UnisonFileChanged event.
                        atomically . Q.enqueue cmdQueue $ Nothing
                        let sourceName = fromMaybe "scratch.u" filename
                        liftIO $ updateVirtualFile sourceName txt
                        pure $ Left (UnisonFileChanged sourceName txt)
                      API apiRequests -> do
                        liftIO (output "``` api\n")
                        liftIO (for_ apiRequests apiRequest)
                        liftIO (output "```\n\n")
                        awaitInput
                      Ucm hide errOk cmds -> do
                        liftIO (writeIORef hidden hide)
                        liftIO (writeIORef allowErrors errOk)
                        liftIO (writeIORef hasErrors False)
                        liftIO (output "``` ucm")
                        traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
                        atomically . Q.enqueue cmdQueue $ Nothing
                        awaitInput
                  )
                  s

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

      writeSourceFile :: ScratchFileName -> Text -> IO ()
      writeSourceFile fp contents = do
        shouldShowSourceChanges <- (== Shown) <$> readIORef hidden
        when shouldShowSourceChanges $ do
          atomically (Q.enqueue ucmScratchFileUpdatesQueue (fp, contents))
        updateVirtualFile fp contents

      updateVirtualFile :: ScratchFileName -> Text -> IO ()
      updateVirtualFile fp contents = do
        liftIO (modifyIORef' unisonFiles (Map.insert fp contents))

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
        let stnz = maybe "" (Text.unpack . formatStanza . fst) stanzaOpt
        unless (stnz `isSubsequenceOf` concat currentOut) $
          modifyIORef' out (\acc -> acc <> pure stnz)

      -- output ``` and new lines then call transcriptFailure
      dieWithMsg :: forall a. String -> IO a
      dieWithMsg msg = do
        output "\n```\n\n"
        appendFailingStanza
        transcriptFailure out $
          "The transcript failed due to an error in the stanza above. The error is:\n\n" <> Text.pack msg

      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        errOk <- readIORef allowErrors
        hasErr <- readIORef hasErrors
        when (errOk && not hasErr) $ do
          output "\n```\n\n"
          appendFailingStanza
          transcriptFailure out "The transcript was expecting an error in the stanza above, but did not encounter one."

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
            writeSource = writeSourceFile,
            notify = print,
            notifyNumbered = printNumbered,
            runtime,
            sandboxedRuntime = sbRuntime,
            nativeRuntime = nRuntime,
            serverBaseUrl = Nothing,
            ucmVersion,
            isTranscriptTest = isTest
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

  loop (Cli.loopState0 (PP.toIds initialPP))

transcriptFailure :: IORef (Seq String) -> Text -> IO b
transcriptFailure out msg = do
  texts <- readIORef out
  UnliftIO.throwIO . RunFailure $ mconcat (Text.pack <$> toList texts) <> "\n\n\128721\n\n" <> msg <> "\n"

type P = P.Parsec Void Text

stanzas :: FilePath -> Text -> Either (P.ParseErrorBundle Text Void) [Stanza]
stanzas srcName = (\(CMark.Node _ _DOCUMENT blocks) -> traverse stanzaFromNode blocks) . CMark.commonmarkToNode []
  where
    stanzaFromNode :: CMark.Node -> Either (P.ParseErrorBundle Text Void) Stanza
    stanzaFromNode node = case node of
      CMarkCodeBlock _ info body -> maybe (Left node) pure <$> P.parse (fenced info) srcName body
      _ -> pure $ Left node

ucmLine :: P UcmLine
ucmLine = ucmCommand <|> ucmComment
  where
    ucmCommand :: P UcmLine
    ucmCommand = do
      context <-
        P.try do
          contextString <- P.takeWhile1P Nothing (/= '>')
          context <-
            case (tryFrom @Text contextString) of
              (Right (These project branch)) -> pure (UcmContextProject (ProjectAndBranch project branch))
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

-- | Produce the correct parser for the code block based on the provided info string.
fenced :: Text -> P (Maybe ProcessedBlock)
fenced info = do
  body <- P.getInput
  P.setInput info
  fenceType <- lineToken (word "ucm" <|> word "unison" <|> word "api" <|> language)
  case fenceType of
    "ucm" -> do
      hide <- hidden
      err <- expectingError
      P.setInput body
      pure . Ucm hide err <$> (spaces *> many ucmLine)
    "unison" ->
      do
        -- todo: this has to be more interesting
        -- ```unison:hide
        -- ```unison
        -- ```unison:hide:all scratch.u
        hide <- lineToken hidden
        err <- lineToken expectingError
        fileName <- optional untilSpace1
        P.setInput body
        pure . Unison hide err fileName <$> (spaces *> P.getInput)
    "api" -> do
      P.setInput body
      pure . API <$> (spaces *> many apiRequest)
    _ -> pure Nothing

word' :: Text -> P Text
word' txt = P.try $ do
  chs <- P.takeP (Just $ show txt) (Text.length txt)
  guard (chs == txt)
  pure txt

word :: Text -> P Text
word = word'

lineToken :: P a -> P a
lineToken p = p <* nonNewlineSpaces

nonNewlineSpaces :: P ()
nonNewlineSpaces = void $ P.takeWhileP Nothing (\ch -> ch == ' ' || ch == '\t')

hidden :: P Hidden
hidden =
  (HideAll <$ word ":hide:all")
    <|> (HideOutput <$ word ":hide")
    <|> pure Shown

expectingError :: P ExpectingError
expectingError = isJust <$> optional (word ":error")

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

language :: P Text
language = P.takeWhileP Nothing (\ch -> Char.isDigit ch || Char.isLower ch || ch == '_')

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace

data Error
  = ParseError (P.ParseErrorBundle Text Void)
  | RunFailure Text
  deriving stock (Show)
  deriving anyclass (Exception)
