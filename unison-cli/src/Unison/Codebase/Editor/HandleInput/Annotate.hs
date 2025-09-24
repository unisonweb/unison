module Unison.Codebase.Editor.HandleInput.Annotate (handleAnnotate) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Input (BranchId2)
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath
  ( ProjectPathG (..),
  )
import Unison.CommandLine.BranchRelativePath (BranchRelativePath (..))
import Unison.Core.Project (ProjectAndBranch (..))
import Unison.Prelude
import UnliftIO qualified
import UnliftIO.Directory (findExecutable)
import UnliftIO.Environment qualified as Env
import UnliftIO.Process qualified as Proc

handleAnnotate :: Maybe BranchId2 -> Maybe Text -> Cli ()
handleAnnotate mayThingToAnnotate mayMsg = do
  pp <- Cli.getCurrentProjectPath
  causalHash <- case mayThingToAnnotate of
    Nothing -> do
      Branch.headHash <$> Cli.getCurrentProjectRoot
    Just (Left sch) -> do
      Cli.runTransactionWithRollback \rollback -> Cli.resolveShortCausalHashToCausalHash rollback sch
    Just (Right brp) -> case brp of
      BranchPathInCurrentProject projectBranchName path
        | path == Path.Root -> do
            pab <- ProjectUtils.resolveProjectBranch (ProjectAndBranch Nothing (Just projectBranchName))
            Cli.runTransaction $ ProjectUtils.getProjectBranchCausalHash pab.branch
        | otherwise -> Cli.returnEarly $ InvalidAnnotationTarget "annotating paths is currently unsupported."
      QualifiedBranchPath projectName projectBranchName path
        | path == Path.Root -> do
            pab <- ProjectUtils.resolveProjectBranch (ProjectAndBranch (Just projectName) (Just projectBranchName))
            Cli.runTransaction $ ProjectUtils.getProjectBranchCausalHash pab.branch
        | otherwise -> Cli.returnEarly $ InvalidAnnotationTarget "annotating paths is currently unsupported."
      UnqualifiedPath {} -> Cli.returnEarly $ InvalidAnnotationTarget "annotating paths is currently unsupported."
  causalHashId <- Cli.runTransaction $ Q.expectCausalHashIdByCausalHash causalHash

  mayNewMessage <- liftIO $ editMessage (mayMsg <|> annotationTemplate)
  case mayNewMessage of
    Nothing -> Cli.respond $ AnnotationAborted
    Just newMessage -> do
      Cli.runTransaction $ Q.annotateCausal pp.project.projectId causalHashId newMessage
      Cli.respond $ AnnotatedSuccessfully
  where
    annotationTemplate = Nothing

unisonEditorEnvVar :: String
unisonEditorEnvVar = "UNISON_EDITOR"

editorEnvVar :: String
editorEnvVar = "EDITOR"

getEditorProgram :: (MonadIO m) => m (Maybe FilePath)
getEditorProgram = runMaybeT $ do
  fromEnvVar unisonEditorEnvVar
    <|> fromEnvVar editorEnvVar
    <|> fromEnvVar "VISUAL"
    <|> MaybeT (findExecutable "nano")
    <|> MaybeT (findExecutable "vi")
  where
    fromEnvVar var = do
      progName <- MaybeT $ Env.lookupEnv var
      guard (not (null progName))
      MaybeT $ findExecutable progName

-- | Trigger the user's preferred editing workflow to edit a message, using the provided message to pre-populate the editor.
-- Returns Nothing if the editor was closed with a non-zero exit code, or the message is empty.
editMessage :: (MonadUnliftIO m) => Maybe Text -> m (Maybe Text)
editMessage initialMessage = runMaybeT do
  editorProg <- MaybeT getEditorProgram
  MaybeT $ UnliftIO.withSystemTempFile "ucm-annotation" $ \tempFilePath tempHandle -> runMaybeT do
    -- Write the initial message to the temp file, if any
    liftIO $ for_ initialMessage $ \msg -> Text.hPutStrLn tempHandle msg
    UnliftIO.hClose tempHandle
    -- Launch the editor on the temp file
    liftIO (UnliftIO.tryAny (Proc.callProcess editorProg [tempFilePath])) >>= \case
      Left _ -> empty
      Right () -> pure ()
    result <- liftIO $ readUtf8 tempFilePath
    guard $ not (Text.null (Text.strip result))
    pure result
