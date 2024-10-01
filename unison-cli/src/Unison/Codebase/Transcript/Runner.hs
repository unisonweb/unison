{-# LANGUAGE DeriveAnyClass #-}

-- | Execute transcripts.
module Unison.Codebase.Transcript.Runner
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
import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.These (These (..))
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Client qualified as HTTP
import System.Environment (lookupEnv)
import System.IO qualified as IO
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
import Unison.Codebase.Transcript
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.Codebase.Verbosity (Verbosity, isSilent)
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine
import Unison.CommandLine.InputPattern (InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import Unison.CommandLine.Welcome (asciiartUnison)
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

type Runner =
  String ->
  Text ->
  (FilePath, Codebase IO Symbol Ann) ->
  IO (Either Error (Seq Stanza))

withRunner ::
  forall m r.
  (UnliftIO.MonadUnliftIO m) =>
  -- | Whether to treat this transcript run as a transcript test, which will try to make output deterministic
  Bool ->
  Verbosity ->
  UCMVersion ->
  FilePath ->
  (Runner -> m r) ->
  m r
withRunner isTest verbosity ucmVersion nrtp action =
  withRuntimes nrtp \runtime sbRuntime nRuntime ->
    action \transcriptName transcriptSrc (codebaseDir, codebase) ->
      Server.startServer
        Backend.BackendEnv {Backend.useNamesIndex = False}
        Server.defaultCodebaseServerOpts
        runtime
        codebase
        \baseUrl ->
          either
            (pure . Left . ParseError)
            (run isTest verbosity codebaseDir codebase runtime sbRuntime nRuntime ucmVersion $ tShow baseUrl)
            $ Transcript.stanzas transcriptName transcriptSrc
  where
    withRuntimes ::
      FilePath -> (Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> m a) -> m a
    withRuntimes nrtp action =
      RTI.withRuntime False RTI.Persistent ucmVersion \runtime ->
        RTI.withRuntime True RTI.Persistent ucmVersion \sbRuntime ->
          action runtime sbRuntime =<< liftIO (RTI.startNativeRuntime ucmVersion nrtp)

run ::
  -- | Whether to treat this transcript run as a transcript test, which will try to make output deterministic
  Bool ->
  Verbosity ->
  FilePath ->
  Codebase IO Symbol Ann ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  UCMVersion ->
  Text ->
  [Stanza] ->
  IO (Either Error (Seq Stanza))
run isTest verbosity dir codebase runtime sbRuntime nRuntime ucmVersion baseURL stanzas = UnliftIO.try do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  (initialPP, emptyCausalHashId) <-
    Codebase.runTransaction codebase . liftA2 (,) Codebase.expectCurrentProjectPath $ snd <$> Codebase.emptyCausalHash

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
        maybe
          (AuthN.newTokenProvider credMan)
          (\accessToken _codeserverID -> pure $ Right accessToken)
          mayShareAccessToken
  -- Queue of Stanzas and Just index, or Nothing if the stanza was programmatically generated
  -- e.g. a unison-file update by a command like 'edit'
  inputQueue <- Q.prepopulatedIO . Seq.fromList $ stanzas `zip` (Just <$> [1 :: Int ..])
  -- Queue of UCM commands to run.
  -- Nothing indicates the end of a ucm block.
  cmdQueue <- Q.newIO @(Maybe UcmLine)
  -- Queue of scratch file updates triggered by UCM itself, e.g. via `edit`, `update`, etc.
  ucmScratchFileUpdatesQueue <- Q.newIO @(ScratchFileName, Text)
  ucmOutput <- newIORef mempty
  unisonFiles <- newIORef Map.empty
  out <- newIORef mempty
  hidden <- newIORef Shown
  allowErrors <- newIORef False
  hasErrors <- newIORef False
  mBlock <- newIORef Nothing
  let patternMap = Map.fromList $ (\p -> (patternName p, p) : ((,p) <$> aliases p)) =<< validInputs
  let output' :: Bool -> Stanza -> IO ()
      output' inputEcho msg = do
        hide <- readIORef hidden
        unless (hideOutput inputEcho hide) $ modifyIORef' out (<> pure msg)

      hideOutput :: Bool -> Hidden -> Bool
      hideOutput inputEcho = \case
        Shown -> False
        HideOutput -> not inputEcho
        HideAll -> True

      output, outputEcho :: Stanza -> IO ()
      output = output' False
      outputEcho = output' True

      outputUcm :: Text -> IO ()
      outputUcm line = modifyIORef' ucmOutput (<> pure line)

      apiRequest :: APIRequest -> IO [Text]
      apiRequest req =
        let input = Transcript.formatAPIRequest req
         in case req of
              APIComment {} -> pure $ pure input
              GetRequest path -> do
                req <- either (dieWithMsg . show) pure $ HTTP.parseRequest (Text.unpack $ baseURL <> path)
                respBytes <- HTTP.httpLbs req httpManager
                case Aeson.eitherDecode (HTTP.responseBody respBytes) of
                  Right (v :: Aeson.Value) ->
                    pure
                      [ input,
                        Text.pack . BL.unpack $ Aeson.encodePretty' (Aeson.defConfig {Aeson.confCompare = compare}) v
                      ]
                  Left err -> dieWithMsg ("Error decoding response from " <> Text.unpack path <> ": " <> err)

      endUcmBlock = do
        liftIO $ do
          -- NB: This uses a `CMarkCodeBlock` instead of `Ucm`, because `Ucm` can’t yet contain command output. This
          --     should change with #5199.
          output . Left . CMarkCodeBlock Nothing "ucm" . Text.unlines =<< readIORef ucmOutput
          writeIORef ucmOutput []
          dieUnexpectedSuccess
        atomically $ void $ do
          scratchFileUpdates <- Q.flush ucmScratchFileUpdatesQueue
          -- Push them onto the front stanza queue in the correct order.
          for (reverse scratchFileUpdates) \(fp, contents) ->
            -- Output blocks for any scratch file updates the ucm block triggered.
            --
            -- NB: This uses a `CMarkCodeBlock` instead of `Unison`, because `Unison` doesn’t yet support the
            --     `:added-by-ucm` token. This should change with #5199.
            Q.undequeue inputQueue (Left $ CMarkCodeBlock Nothing ("unison :added-by-ucm " <> fp) contents, Nothing)
        Cli.returnEarlyWithoutOutput

      processUcmLine p =
        case p of
          UcmComment {} -> do
            liftIO . outputUcm $ Transcript.formatUcmLine p
            Cli.returnEarlyWithoutOutput
          UcmCommand context lineTxt -> do
            curPath <- Cli.getCurrentProjectPath
            -- We're either going to run the command now (because we're in the right context), else we'll switch to
            -- the right context first, then run the command next.
            maybeSwitchCommand <- case context of
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
                      let projectBranch =
                            ProjectBranch {projectId, parentBranchId = Nothing, branchId, name = branchName}
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
                atomically . Q.undequeue cmdQueue $ Just p
                pure $ Right switchCommand
              Nothing -> do
                case words . Text.unpack $ lineTxt of
                  [] -> Cli.returnEarlyWithoutOutput
                  args -> do
                    liftIO . outputUcm $ Transcript.formatUcmLine p <> "\n"
                    numberedArgs <- use #numberedArgs
                    PP.ProjectAndBranch projId branchId <-
                      PP.toProjectAndBranch . NonEmpty.head <$> use #projectPathStack
                    let getProjectRoot = liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
                    liftIO (parseInput codebase curPath getProjectRoot numberedArgs patternMap args)
                      >>= either
                        -- invalid command is treated as a failure
                        ( \msg -> do
                            liftIO $ writeIORef hasErrors True
                            liftIO (readIORef allowErrors) >>= \case
                              True -> do
                                liftIO . outputUcm . Text.pack $ Pretty.toPlain terminalWidth msg
                                Cli.returnEarlyWithoutOutput
                              False -> liftIO . dieWithMsg $ Pretty.toPlain terminalWidth msg
                        )
                        -- No input received from this line, try again.
                        (maybe Cli.returnEarlyWithoutOutput $ pure . Right . snd)

      startProcessedBlock block = case block of
        Unison hide errOk filename txt -> do
          liftIO (writeIORef hidden hide)
          liftIO . outputEcho $ pure block
          liftIO (writeIORef allowErrors errOk)
          -- Open a ucm block which will contain the output from UCM after processing the `UnisonFileChanged` event.
          -- Close the ucm block after processing the UnisonFileChanged event.
          atomically . Q.enqueue cmdQueue $ Nothing
          let sourceName = fromMaybe "scratch.u" filename
          liftIO $ updateVirtualFile sourceName txt
          pure . Left $ UnisonFileChanged sourceName txt
        API apiRequests -> do
          liftIO $
            -- NB: This uses a `CMarkCodeBlock` instead of `API`, because `API` can’t yet contain API responses. This
            --     should change with #5199.
            output . Left . CMarkCodeBlock Nothing "api" . Text.unlines . fold =<< traverse apiRequest apiRequests
          Cli.returnEarlyWithoutOutput
        Ucm hide errOk cmds -> do
          liftIO (writeIORef hidden hide)
          liftIO (writeIORef allowErrors errOk)
          liftIO (writeIORef hasErrors False)
          traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
          atomically . Q.enqueue cmdQueue $ Nothing
          Cli.returnEarlyWithoutOutput

      showStatus alwaysShow indicator msg = unless (not alwaysShow && Verbosity.isSilent verbosity) do
        clearCurrentLine
        putStr $ "\r" <> indicator <> "   " <> msg
        IO.hFlush IO.stdout

      finishTranscript = do
        showStatus True "✔️" "Completed transcript.\n"
        pure $ Right QuitI

      processStanza stanza midx = do
        liftIO . showStatus False "⚙️" $
          maybe
            "Processing UCM-generated stanza."
            (\idx -> "Processing stanza " <> show idx <> " of " <> show (length stanzas) <> ".")
            midx
        either
          ( \node -> do
              liftIO . output $ Left node
              Cli.returnEarlyWithoutOutput
          )
          ( \block -> do
              liftIO . writeIORef mBlock $ pure block
              startProcessedBlock block
          )
          stanza

      whatsNext = do
        liftIO (dieUnexpectedSuccess)
        liftIO (writeIORef hidden Shown)
        liftIO (writeIORef allowErrors False)
        maybe (liftIO finishTranscript) (uncurry processStanza) =<< atomically (Q.tryDequeue inputQueue)

      awaitInput :: Cli (Either Event Input)
      awaitInput = maybe whatsNext (maybe endUcmBlock processUcmLine) =<< atomically (Q.tryDequeue cmdQueue)

      loadPreviousUnisonBlock name =
        maybe
          -- This lets transcripts use the `load` command, as in:
          --
          -- .> load someFile.u
          (fmap Cli.LoadSuccess (readUtf8 $ Text.unpack name) <|> pure Cli.InvalidSourceNameError)
          (pure . Cli.LoadSuccess)
          . Map.lookup name
          =<< readIORef unisonFiles

      writeSourceFile :: ScratchFileName -> Text -> IO ()
      writeSourceFile fp contents = do
        shouldShowSourceChanges <- (== Shown) <$> readIORef hidden
        when shouldShowSourceChanges . atomically $ Q.enqueue ucmScratchFileUpdatesQueue (fp, contents)
        updateVirtualFile fp contents

      updateVirtualFile :: ScratchFileName -> Text -> IO ()
      updateVirtualFile fp = modifyIORef' unisonFiles . Map.insert fp

      print :: Output.Output -> IO ()
      print o = do
        msg <- notifyUser dir o
        errOk <- readIORef allowErrors
        let rendered = Pretty.toPlain terminalWidth $ Pretty.indentN 2 msg <> "\n"
        outputUcm $ Text.pack rendered
        when (Output.isFailure o) $
          if errOk
            then writeIORef hasErrors True
            else dieWithMsg rendered

      printNumbered :: Output.NumberedOutput -> IO Output.NumberedArgs
      printNumbered o = do
        let (msg, numberedArgs) = notifyNumbered o
        errOk <- readIORef allowErrors
        let rendered = Pretty.toPlain terminalWidth $ Pretty.indentN 2 msg <> "\n"
        outputUcm $ Text.pack rendered
        when (Output.isNumberedFailure o) $
          if errOk
            then writeIORef hasErrors True
            else dieWithMsg rendered
        pure numberedArgs

      -- Looks at the current stanza and decides if it is contained in the
      -- output so far. Appends it if not.
      appendFailingStanza :: IO ()
      appendFailingStanza = do
        blockOpt <- readIORef mBlock
        currentOut <- readIORef out
        maybe
          (pure ())
          (\block -> unless (elem (pure block) currentOut) $ modifyIORef' out (<> pure (pure block)))
          blockOpt

      dieWithMsg :: forall a. String -> IO a
      dieWithMsg msg = do
        appendFailingStanza
        transcriptFailure out "The transcript failed due to an error in the stanza above. The error is:" . pure $
          Text.pack msg

      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        errOk <- readIORef allowErrors
        hasErr <- readIORef hasErrors
        when (errOk && not hasErr) $ do
          appendFailingStanza
          transcriptFailure
            out
            "The transcript was expecting an error in the stanza above, but did not encounter one."
            Nothing

  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion

  seedRef <- newIORef (0 :: Int)

  let env =
        Cli.Env
          { authHTTPClient = authenticatedHTTPClient,
            codebase,
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

  let loop :: Cli.LoopState -> IO (Seq Stanza)
      loop s0 = do
        Cli.runCli env s0 awaitInput >>= \case
          (Cli.Success input, s1) ->
            let next s = loop $ either (const s) (\inp -> s & #lastInput ?~ inp) input
             in Cli.runCli env s1 (HandleInput.loop input) >>= \case
                  (Cli.Success (), s2) -> next s2
                  (Cli.Continue, s2) -> next s2
                  (Cli.HaltRepl, _) -> onHalt
          (Cli.Continue, s1) -> loop s1
          (Cli.HaltRepl, _) -> onHalt
        where
          onHalt = readIORef out

  loop (Cli.loopState0 (PP.toIds initialPP))

transcriptFailure :: IORef (Seq Stanza) -> Text -> Maybe Text -> IO b
transcriptFailure out heading mbody = do
  texts <- readIORef out
  UnliftIO.throwIO . RunFailure $
    texts
      <> Seq.fromList
        ( Left
            <$> [ CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT "🛑") []],
                  CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT heading) []]
                ]
              <> foldr ((:) . CMarkCodeBlock Nothing "") [] mbody
        )

data Error
  = ParseError (P.ParseErrorBundle Text Void)
  | RunFailure (Seq Stanza)
  deriving stock (Show)
  deriving anyclass (Exception)
