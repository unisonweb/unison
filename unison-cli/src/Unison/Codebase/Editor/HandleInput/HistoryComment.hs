module Unison.Codebase.Editor.HandleInput.HistoryComment (handleHistoryComment) where

import Control.Monad.Reader
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock.POSIX qualified as Time
import Text.RawString.QQ (r)
import U.Codebase.Config qualified as Config
import U.Codebase.HashTags
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.CredentialManager qualified as CredMan
import Unison.Auth.PersonalKey qualified as PK
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Input (BranchId2)
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Path qualified as Path
import Unison.CommandLine.BranchRelativePath (BranchRelativePath (..))
import Unison.Core.Project (ProjectAndBranch (..))
import Unison.Hash qualified as Hash
import Unison.HistoryComment (HistoryComment (..), HistoryCommentRevision (..))
import Unison.HistoryComments.Hashing
  ( hashHistoryComment,
    hashHistoryCommentRevision,
  )
import Unison.Prelude
import UnliftIO qualified
import UnliftIO.Directory (findExecutable)
import UnliftIO.Environment qualified as Env
import UnliftIO.Process qualified as Proc

handleHistoryComment :: Maybe BranchId2 -> Cli ()
handleHistoryComment mayThingToAnnotate = do
  Cli.Env {credentialManager} <- ask
  personalKey <- liftIO (CredMan.getOrCreatePersonalKey credentialManager)
  let authorThumbprint = PK.personalKeyThumbprint personalKey
  mayAuthorName <-
    Cli.runTransaction do
      authorName <- Q.getAuthorName
      pure (authorName)
  authorName <- case mayAuthorName of
    Nothing -> Cli.returnEarly $ AuthorNameRequired
    Just authorName -> pure authorName
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
        | otherwise -> Cli.returnEarly $ InvalidCommentTarget "commenting on paths is currently unsupported."
      QualifiedBranchPath projectName projectBranchName path
        | path == Path.Root -> do
            pab <- ProjectUtils.resolveProjectBranch (ProjectAndBranch (Just projectName) (Just projectBranchName))
            Cli.runTransaction $ ProjectUtils.getProjectBranchCausalHash pab.branch
        | otherwise -> Cli.returnEarly $ InvalidCommentTarget "commenting on paths is currently unsupported."
      UnqualifiedPath {} -> Cli.returnEarly $ InvalidCommentTarget "commenting on paths is currently unsupported."
  (causalHashId, mayHistoryComment) <- Cli.runTransaction $ do
    causalHashId <- Q.expectCausalHashIdByCausalHash causalHash
    mayExistingCommentInfo <- Q.getLatestCausalComment causalHashId
    pure (causalHashId, mayExistingCommentInfo)
  let populatedMsg = fromMaybe commentInstructions $ do
        HistoryCommentRevision {subject, content} <- mayHistoryComment
        pure $ Text.unlines [subject, "", content, commentInstructions]
  mayNewMessage <- liftIO (editMessage (Just populatedMsg))
  case mayNewMessage of
    Nothing -> Cli.returnEarly $ CommentAborted
    Just (subject, content) -> do
      createdAt <- liftIO $ Time.getCurrentTime
      let historyComment =
            hashHistoryComment $
              HistoryComment
                { author =
                    Config.unAuthorName authorName,
                  commentId = (),
                  causal = causalHash,
                  createdAt,
                  authorThumbprint
                }
      let historyCommentRevision =
            hashHistoryCommentRevision $
              HistoryCommentRevision
                { revisionId = (),
                  subject,
                  content,
                  createdAt,
                  -- Hard coded for now, we can change this later if we want to support hiding comments
                  isHidden = False,
                  authorSignature = "",
                  comment = historyComment.commentId
                }
      let historyComment' = historyComment {causal = causalHashId}
      let historyCommentRevisionHashBytes =
            historyCommentRevision.revisionId
              & unHistoryCommentRevisionHash
              & Hash.toByteString
      PK.PersonalKeySignature authorSignature <-
        PK.signWithPersonalKey personalKey historyCommentRevisionHashBytes >>= \case
          Left err -> Cli.returnEarly $ CommentFailed (Text.pack (show err))
          Right sig -> pure sig
      Cli.runTransaction $ Q.commentOnCausal $ historyCommentRevision {comment = historyComment', authorSignature = authorSignature}
      Cli.respond $ CommentedSuccessfully
  where
    commentInstructions =
      [r|
-- Enter your comment, then save and quit your editor to continue.
-- Lines that start with '--' will be ignored.|]

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
editMessage :: (MonadUnliftIO m) => Maybe Text -> m (Maybe (Text, Text))
editMessage initialMessage = runMaybeT do
  editorProg <- MaybeT getEditorProgram
  MaybeT $ UnliftIO.withSystemTempFile "ucm-history-comment" $ \tempFilePath tempHandle -> runMaybeT do
    -- Write the initial message to the temp file, if any
    liftIO $ for_ initialMessage $ \msg -> Text.hPutStrLn tempHandle msg
    UnliftIO.hClose tempHandle
    -- Launch the editor on the temp file
    liftIO (UnliftIO.tryAny (Proc.callProcess editorProg [tempFilePath])) >>= \case
      Left _ -> empty
      Right () -> pure ()
    result <- liftIO (readUtf8 tempFilePath)
    let cleanedResult =
          result
            & Text.lines
            & filter (not . Text.isPrefixOf "--")
            & Text.unlines
            & Text.strip
    guard $ not (Text.null cleanedResult)
    let (subject, contents) =
          case Text.lines cleanedResult of
            [] -> ("", "")
            (s : rest) -> (Text.strip s, Text.strip $ Text.unlines rest)
    pure (subject, contents)
