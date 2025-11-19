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
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.These (These (..))
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Client qualified as HTTP
import System.IO qualified as IO
import Text.Megaparsec qualified as P
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..), ProjectBranchRow (..))
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
import Unison.Codebase.Transcript
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.Codebase.Verbosity (Verbosity, isSilent)
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine
import Unison.CommandLine.FuzzySelect qualified as Fuzzy
import Unison.CommandLine.InputPattern (aliases, patternName)
import Unison.CommandLine.InputPattern qualified as IP
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser, showIssueUrl)
import Unison.CommandLine.Welcome (asciiartUnison)
import Unison.Debug qualified as Debug
import Unison.MCP qualified as MCP
import Unison.MCP.Server qualified as MCP
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
import UnliftIO.Environment (setEnv)
import UnliftIO.STM
import Prelude hiding (readFile, writeFile)

-- | Render transcript errors at a width of 65 chars.
terminalWidth :: Pretty.Width
terminalWidth = 65

type Runner =
  -- | The name of the transcript to run.
  String ->
  -- | The contents of the transcript to run.
  ByteString ->
  Codebase IO Symbol Ann ->
  IO (Either Error Transcript)

withRunner ::
  forall m r.
  (UnliftIO.MonadUnliftIO m) =>
  -- | Whether to treat this transcript run as a transcript test, which will try to make output deterministic
  Bool ->
  Verbosity ->
  UCMVersion ->
  (Runner -> m r) ->
  m r
withRunner isTest verbosity ucmVersion action = do
  credMan <- AuthN.newCredentialManager
  authenticatedHTTPClient <- initTranscriptAuthenticatedHTTPClient credMan

  -- If we're in a transcript test, configure the environment to use a non-existent fzf binary
  -- so that errors are consistent.
  -- This also prevents automated transcript tests from mistakenly opening fzf and waiting for user input.
  when isTest $ do
    liftIO $ setEnv Fuzzy.fzfPathEnvVar "NONE"
  withRuntimes \runtime sbRuntime ->
    action \transcriptName transcriptSrc codebase -> do
      let workDir = Nothing
      mcpServerConfig <- MCP.initServer codebase runtime sbRuntime workDir ucmVersion authenticatedHTTPClient
      Server.startServer
        isTest
        Backend.BackendEnv
        Server.defaultCodebaseServerOpts
        runtime
        codebase
        (MCP.mcpServer mcpServerConfig)
        \case
          Nothing -> pure $ Left PortBindingFailure
          Just baseUrl -> do
            let baseUrlText = tShow @Server.BaseUrl baseUrl
            case (Transcript.parse transcriptName transcriptSrc) of
              Left parseError -> pure $ Left (ParseError parseError)
              Right stanzas ->
                run
                  isTest
                  verbosity
                  codebase
                  runtime
                  sbRuntime
                  ucmVersion
                  baseUrlText
                  authenticatedHTTPClient
                  credMan
                  stanzas
                  & catchExceptions
  where
    catchExceptions :: forall x. IO (Either Error x) -> IO (Either Error x)
    catchExceptions io =
      UnliftIO.tryAny (io >>= UnliftIO.evaluate) >>= \case
        Left someException -> pure $ Left (Exception someException)
        Right r -> pure r
    withRuntimes :: (RTI.Runtime Symbol -> RTI.Runtime Symbol -> m a) -> m a
    withRuntimes action =
      RTI.withRuntime False RTI.Persistent ucmVersion \runtime ->
        RTI.withRuntime True RTI.Persistent ucmVersion \sbRuntime ->
          action runtime sbRuntime
    initTranscriptAuthenticatedHTTPClient :: AuthN.CredentialManager -> m AuthN.AuthenticatedHttpClient
    initTranscriptAuthenticatedHTTPClient credMan = liftIO $ do
      let tokenProvider :: AuthN.TokenProvider
          tokenProvider = AuthN.newTokenProvider credMan
      AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion

isGeneratedBlock :: ProcessedBlock -> Bool
isGeneratedBlock = generated . getCommonInfoTags

run ::
  -- | Whether to treat this transcript run as a transcript test, which will try to make output deterministic
  Bool ->
  Verbosity ->
  Codebase IO Symbol Ann ->
  RTI.Runtime Symbol ->
  RTI.Runtime Symbol ->
  UCMVersion ->
  Text ->
  AuthN.AuthenticatedHttpClient ->
  AuthN.CredentialManager ->
  Transcript ->
  IO (Either Error Transcript)
run isTest verbosity codebase runtime sbRuntime ucmVersion baseURL authenticatedHTTPClient credMan transcript = UnliftIO.try do
  let behaviors = extractBehaviors $ settings transcript
  let stanzas' = stanzas transcript
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
  -- Queue of Stanzas and Just index, or Nothing if the stanza was programmatically generated
  -- e.g. a unison-file update by a command like 'edit'
  inputQueue <-
    Q.prepopulatedIO . Seq.fromList $
      filter (either (const True) (not . isGeneratedBlock)) stanzas' `zip` (Just <$> [1 :: Int ..])
  -- Queue of UCM commands to run.
  -- Nothing indicates the end of a ucm block.
  cmdQueue <- Q.newIO @(Maybe UcmLine)
  -- Queue of scratch file updates triggered by UCM itself, e.g. via `edit`, `update`, etc.
  ucmScratchFileUpdatesQueue <- Q.newIO @(ScratchFileName, Text)
  ucmOutput <- newIORef mempty
  unisonFiles <- newIORef Map.empty
  out <- newIORef mempty
  currentTags <- newIORef Nothing
  isHidden <- newIORef Shown
  allowErrors <- newIORef False
  expectFailure <- newIORef False
  hasErrors <- newIORef False
  mBlock <- newIORef Nothing
  let patternMap = Map.fromList $ (\p -> (patternName p, p) : ((,p) <$> aliases p)) =<< IP.validInputs
  let output' :: Bool -> Stanza -> IO ()
      output' inputEcho msg = do
        hide <- hideOutput inputEcho
        unless hide $ modifyIORef' out (<> pure msg)

      hideOutput' :: Bool -> Hidden -> Bool
      hideOutput' inputEcho = \case
        Shown -> False
        HideOutput -> not inputEcho
        HideAll -> True

      hideOutput :: Bool -> IO Bool
      hideOutput inputEcho = hideOutput' inputEcho <$> readIORef isHidden

      output, outputEcho :: Stanza -> IO ()
      output = output' False
      outputEcho = output' True

      outputUcmLine :: UcmLine -> IO ()
      outputUcmLine line = do
        prev <- readIORef ucmOutput
        modifyIORef' ucmOutput (<> ((if not (null prev) then pure (UcmOutputLine "\n") else mempty) <> pure line))

      outputUcmResult :: Pretty.Pretty Pretty.ColorText -> IO ()
      outputUcmResult line = do
        hide <- hideOutput False
        unless hide . outputUcmLine . UcmOutputLine $
          -- We shorten the terminal width, because "Transcript" manages a 2-space indent for output lines.
          Pretty.toPlain (terminalWidth - 2) line

      maybeDieWithMsg :: Pretty.Pretty Pretty.ColorText -> IO ()
      maybeDieWithMsg msg = do
        liftIO $ writeIORef hasErrors True
        liftIO (liftA2 (,) (readIORef allowErrors) (readIORef expectFailure)) >>= \case
          (False, False) -> liftIO . dieWithMsg $ Pretty.toPlain terminalWidth msg
          (True, True) -> do
            appendFailingStanza
            fixedBug (frontmatter transcript) out $
              Text.unlines
                [ "The stanza above marked with `:error :bug` is now failing with",
                  "",
                  "```",
                  Pretty.toPlain terminalWidth msg,
                  "```",
                  "",
                  "so you can remove `:bug` and close any appropriate Github issues. If the error message is different \
                  \from the expected error message, open a new issue and reference it in this transcript."
                ]
          (_, _) -> pure ()

      doHttpRequest :: HTTP.Request -> IO Text
      doHttpRequest req = do
        resp <- HTTP.responseBody <$> HTTP.httpLbs req httpManager
        case Aeson.eitherDecode @Aeson.Value resp of
          Left err -> dieWithMsg . Text.pack $ "Error decoding response from " <> (BSC.unpack (HTTP.method req)) <> ": " <> err
          Right v -> do
            let prettyBytes = Aeson.encodePretty' (Aeson.defConfig {Aeson.confCompare = compare}) v
            pure $ Text.pack . BL.unpack $ prettyBytes
      apiRequest :: APIRequest -> IO [APIRequest]
      apiRequest req = do
        hide <- hideOutput False
        case req of
          -- We just discard this, because the runner will produce new output lines.
          APIResponse {} -> pure []
          APIComment {} -> pure $ pure req
          GetRequest path -> do
            httpReq <- case HTTP.parseRequest (Text.unpack $ baseURL <> path) of
              Left err -> dieWithMsg (tShow err)
              Right r -> pure r
            respTxt <- doHttpRequest httpReq
            if hide
              then pure [req]
              else pure [req, APIResponse respTxt]
          PostRequest path body -> do
            httpReq <- case HTTP.parseRequest (Text.unpack $ baseURL <> path) of
              Left err -> dieWithMsg (tShow err)
              Right r ->
                pure $
                  r
                    { HTTP.method = "POST",
                      HTTP.requestBody = HTTP.RequestBodyBS (Text.encodeUtf8 body),
                      HTTP.requestHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]
                    }
            Debug.debugM Debug.Temp "POST REQUEST" httpReq
            respTxt <- doHttpRequest httpReq
            Debug.debugM Debug.Temp "RESPONSE" respTxt
            if hide
              then pure [req]
              else pure [req, APIResponse respTxt]

      endUcmBlock = do
        liftIO $ do
          tags <- readIORef currentTags
          ucmOut <- readIORef ucmOutput
          unless (null ucmOut && tags == Nothing) . outputEcho . pure $
            Ucm (fromMaybe (defaultInfoTags mempty) {generated = True} tags) ucmOut
          writeIORef ucmOutput []
          dieUnexpectedSuccess
        atomically $ void $ do
          scratchFileUpdates <- Q.flush ucmScratchFileUpdatesQueue
          -- Push them onto the front stanza queue in the correct order.
          for (reverse scratchFileUpdates) \(fp, contents) ->
            -- Output blocks for any scratch file updates the ucm block triggered.
            Q.undequeue inputQueue (pure $ Unison (defaultInfoTags $ pure fp) {generated = True} contents, Nothing)
        Cli.returnEarlyWithoutOutput

      processUcmLine p =
        case p of
          -- We just discard this, because the runner will produce new output lines.
          UcmOutputLine {} -> Cli.returnEarlyWithoutOutput
          UcmComment {} -> do
            liftIO $ outputUcmLine p
            Cli.returnEarlyWithoutOutput
          UcmCommand context lineTxt -> do
            curPath <- Cli.getCurrentProjectPath
            -- We're either going to run the command now (because we're in the right context), else we'll switch to
            -- the right context first, then run the command next.
            maybeSwitchCommand <- case context of
              UcmContextEmpty -> pure Nothing
              UcmContextProject (ProjectAndBranch projectName branchName) -> Cli.runTransaction do
                Project {projectId, name = projectName} <-
                  Q.loadProjectByName projectName
                    >>= \case
                      Nothing -> do
                        projectId <- Sqlite.unsafeIO (Db.ProjectId <$> UUID.nextRandom)
                        Q.insertProject projectId projectName
                        pure $ Project {projectId, name = projectName}
                      Just project -> pure project
                projectAndBranchIds <-
                  Q.loadProjectBranchByName projectId branchName >>= \case
                    Nothing -> do
                      branchId <- Sqlite.unsafeIO (Db.ProjectBranchId <$> UUID.nextRandom)
                      Q.insertProjectBranch
                        "Branch Created"
                        emptyCausalHashId
                        ProjectBranchRow {projectId, parentBranchId = Nothing, branchId, name = branchName}
                      pure (ProjectAndBranch projectId branchId)
                    Just projBranch -> pure (ProjectAndBranch projBranch.projectId projBranch.branchId)
                pure
                  if (PP.toProjectAndBranch . PP.toIds $ curPath) == projectAndBranchIds
                    then Nothing
                    else Just (ProjectSwitchI (ProjectAndBranchNames'Unambiguous (These projectName branchName)))
            case maybeSwitchCommand of
              Just switchCommand -> do
                atomically . Q.undequeue cmdQueue $ Just p
                pure $ Right switchCommand
              Nothing -> do
                case fromMaybe [] $ IP.parseArgs (Text.unpack lineTxt) of
                  [] -> Cli.returnEarlyWithoutOutput
                  args -> do
                    liftIO $ outputUcmLine p
                    numberedArgs <- use #numberedArgs
                    PP.ProjectAndBranch projId branchId <-
                      PP.toProjectAndBranch . NonEmpty.head <$> use #projectPathStack
                    let getProjectRoot = liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
                    liftIO (parseInput codebase curPath getProjectRoot numberedArgs patternMap args)
                      >>= either
                        -- invalid command is treated as a failure
                        ( \failure -> do
                            let msg = reportParseFailure failure
                            liftIO $ outputUcmResult msg
                            liftIO $ maybeDieWithMsg msg
                            Cli.returnEarlyWithoutOutput
                        )
                        -- No input received from this line, try again.
                        (maybe Cli.returnEarlyWithoutOutput $ pure . Right . snd)

      startProcessedBlock block = case block of
        Unison infoTags txt -> do
          -- Open a ucm block which will contain the output from UCM after processing the `UnisonFileChanged` event.
          -- Close the ucm block after processing the UnisonFileChanged event.
          atomically $ Q.enqueue cmdQueue Nothing
          liftIO do
            writeIORef isHidden $ (runIdentity $ getHidden behaviors) block
            outputEcho $ pure block
            writeIORef allowErrors $ expectingError infoTags
            writeIORef expectFailure $ hasBug infoTags
          let sourceName = fromMaybe "scratch.u" $ additionalTags infoTags
          liftIO $ updateVirtualFile sourceName txt
          when (runIdentity (autoupdate behaviors)) do
            liftIO $ writeIORef isHidden HideAll
            atomically . Q.enqueue cmdQueue . pure $ UcmCommand UcmContextEmpty "update"
            atomically $ Q.enqueue cmdQueue Nothing
          pure . Left $ UnisonFileChanged sourceName txt
        API infoTags apiRequests -> do
          liftIO do
            writeIORef isHidden $ (runIdentity $ getHidden behaviors) block
            writeIORef allowErrors $ expectingError infoTags
            writeIORef expectFailure $ hasBug infoTags
            outputEcho . pure . API infoTags . fold =<< traverse apiRequest apiRequests
          Cli.returnEarlyWithoutOutput
        Ucm infoTags cmds -> do
          liftIO do
            writeIORef currentTags $ pure infoTags
            writeIORef isHidden $ (runIdentity $ getHidden behaviors) block
            writeIORef allowErrors $ expectingError infoTags
            writeIORef expectFailure $ hasBug infoTags
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
            (\idx -> "Processing stanza " <> show idx <> " of " <> show (length stanzas') <> ".")
            midx
        either
          (bypassStanza . Left)
          ( \block ->
              if isGeneratedBlock block
                then bypassStanza $ pure block
                else do
                  liftIO . writeIORef mBlock $ pure block
                  startProcessedBlock block
          )
          stanza

      bypassStanza stanza = do
        liftIO $ output stanza
        Cli.returnEarlyWithoutOutput

      whatsNext = do
        liftIO dieUnexpectedSuccess
        liftIO $ writeIORef currentTags Nothing
        liftIO $ writeIORef isHidden Shown
        liftIO $ writeIORef allowErrors False
        liftIO $ writeIORef expectFailure False
        liftIO $ writeIORef hasErrors False
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

      writeSource :: ScratchFileName -> Text -> Bool -> IO ()
      writeSource fp contents _addFold = do
        shouldShowSourceChanges <- (== Shown) <$> readIORef isHidden
        when shouldShowSourceChanges . atomically $ Q.enqueue ucmScratchFileUpdatesQueue (fp, contents)
        updateVirtualFile fp contents

      updateVirtualFile :: ScratchFileName -> Text -> IO ()
      updateVirtualFile fp = modifyIORef' unisonFiles . Map.insert fp

      print :: Output.Output -> IO ()
      print o = do
        -- NB: We have a directory, but we don’t pass it to the notifier because it’s a temp dir, and if it ends up in
        --     transcript output, it makes transcripts non-reproducible.
        msg <- notifyUser Nothing showIssueUrl o
        outputUcmResult msg
        when (Output.isFailure o) $ maybeDieWithMsg msg

      printNumbered :: Output.NumberedOutput -> IO Output.NumberedArgs
      printNumbered o = do
        let (msg, numberedArgs) = notifyNumbered o
        outputUcmResult msg
        when (Output.isNumberedFailure o) $ maybeDieWithMsg msg
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

      dieWithMsg :: forall a. Text -> IO a
      dieWithMsg msg = do
        appendFailingStanza
        transcriptFailure
          (frontmatter transcript)
          out
          "The transcript failed due to an error in the stanza above. The error is:"
          . pure
          $ msg

      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        errOk <- readIORef allowErrors
        expectBug <- readIORef expectFailure
        hasErr <- readIORef hasErrors
        case (errOk, expectBug, hasErr) of
          (True, False, False) -> do
            appendFailingStanza
            transcriptFailure
              (frontmatter transcript)
              out
              "The transcript was expecting an error in the stanza above, but did not encounter one."
              Nothing
          (False, True, False) -> do
            fixedBug
              (frontmatter transcript)
              out
              "The stanza above with `:bug` is now passing! You can remove `:bug` and close any appropriate Github \
              \issues."
          (_, _, _) -> pure ()

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
            lspCheckForChanges = \_ -> pure (),
            writeSource,
            notify = print,
            notifyNumbered = printNumbered,
            runtime,
            sandboxedRuntime = sbRuntime,
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

  Transcript (frontmatter transcript) . toList <$> loop (Cli.loopState0 (PP.toIds initialPP))

transcriptFailure :: Aeson.Value -> IORef (Seq Stanza) -> Text -> Maybe Text -> IO b
transcriptFailure frontmatter out heading mbody = do
  texts <- readIORef out
  UnliftIO.throwIO . RunFailure . Transcript frontmatter $
    toList texts
      <> ( Left
             <$> [ CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT "🛑") []],
                   CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT heading) []]
                 ]
               <> foldr ((:) . CMarkCodeBlock Nothing "") [] mbody
         )

fixedBug :: Aeson.Value -> IORef (Seq Stanza) -> Text -> IO b
fixedBug frontmatter out body = do
  texts <- readIORef out
  -- `CMark.commonmarkToNode` returns a @DOCUMENT@, which won’t be rendered inside another document, so we strip the
  -- outer `CMark.Node`.
  let CMark.Node _ _DOCUMENT bodyNodes = CMark.commonmarkToNode [CMark.optNormalize] body
  UnliftIO.throwIO . RunFailure . Transcript frontmatter $
    toList texts
      <> ( Left
             <$> [ CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT "🎉") []],
                   CMark.Node Nothing (CMark.HEADING 2) [CMark.Node Nothing (CMark.TEXT "You fixed a bug!") []]
                 ]
               <> bodyNodes
         )

data Error
  = ParseError (P.ParseErrorBundle Text Void)
  | RunFailure Transcript
  | PortBindingFailure
  | Exception SomeException
  deriving stock (Show)
  deriving anyclass (Exception)
