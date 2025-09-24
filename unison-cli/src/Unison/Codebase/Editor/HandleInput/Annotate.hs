module Unison.Codebase.Editor.HandleInput.Annotate (handleAnnotate) where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Text (Text)
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.Input (BranchId2)
import Unison.CommandLine.BranchRelativePath (BranchRelativePath (..))
import Unison.Prelude
import UnliftIO.Directory (findExecutable)
import UnliftIO.Environment qualified as Env

handleAnnotate :: Maybe BranchId2 -> Cli ()
handleAnnotate mayThingToAnnotate = do
  _ <- getContents existingMessage
  pp <- Cli.getCurrentProjectPath
  causalHashId <- case mayThingToAnnotate of
    Left sch -> do
      _
    Right brp -> case brp of
      BranchPathInCurrentProject projectBranchName path
        | path == Path.AbsoluteRoot -> _
        | otherwise -> error "annotating paths is not supported yet."
      QualifiedBranchPath projectName projectBranchName path
        | path == Path.AbsoluteRoot -> _
        | otherwise -> error "annotating paths is not supported yet."
      UnqualifiedPath {} -> error "annotating paths is not supported yet."
  Q.annotateCausal pp.project.id causalHashId contents

unisonEditorEnvVar :: String
unisonEditorEnvVar = "UNISON_EDITOR"

editorEnvVar :: String
editorEnvVar = "EDITOR"

getEditorProgram :: Cli (Maybe FilePath)
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
editMessage :: Maybe Text -> Cli (Maybe Text)
editMessage initialMessage = do
  editor <- Cli.getEditor
  Cli.editWithEditor editor initialMessage
