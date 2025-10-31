{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Codebase.Editor.HandleInput.HistoryComment (handleHistoryComment) where

import Control.Monad.Reader
import Crypto.Hash qualified as CH
import Data.ByteArray qualified as BA
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX qualified as Time
import Text.RawString.QQ (r)
import U.Codebase.Config qualified as Config
import U.Codebase.HashTags (CausalHash, CommentHash (..), CommentRevisionHash (..))
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
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Hashing.V2 (ContentAddressable (..))
import Unison.HistoryComment (HistoryComment (..), HistoryCommentRevision (..))
import Unison.KeyThumbprint (KeyThumbprint (..))
import Unison.Prelude
import UnliftIO qualified
import UnliftIO.Directory (findExecutable)
import UnliftIO.Environment qualified as Env
import UnliftIO.Process qualified as Proc

commentHashingVersion :: Int32
commentHashingVersion = 1

-- Hash a base comment
instance ContentAddressable (HistoryComment UTCTime KeyThumbprint CausalHash ()) where
  contentHash HistoryComment {createdAt, author, causal, authorThumbprint} =
    CH.hashUpdates
      CH.hashInit
      [ BL.toStrict . Builder.toLazyByteString $ Builder.int32BE commentHashingVersion,
        Hash.toByteString (into @Hash causal),
        Text.encodeUtf8 $ thumbprintToText authorThumbprint,
        Text.encodeUtf8 author,
        -- Encode UTCTime as a UTC 8601 seconds since epoch
        createdAt
          & Time.utcTimeToPOSIXSeconds
          & floor
          & Builder.int64BE
          & Builder.toLazyByteString
          & BL.toStrict
      ]
      & CH.hashFinalize @CH.SHA3_512
      & BA.convert
      & Hash.fromByteString

-- Hash a comment revision
instance ContentAddressable (HistoryCommentRevision () UTCTime CommentHash) where
  contentHash HistoryCommentRevision {subject, content, createdAt, comment = commentHash} =
    CH.hashUpdates
      CH.hashInit
      [ BL.toStrict . Builder.toLazyByteString $ Builder.int32BE commentHashingVersion,
        Hash.toByteString (into @Hash commentHash),
        Text.encodeUtf8 subject,
        Text.encodeUtf8 content,
        -- Encode UTCTime as a UTC 8601 seconds since epoch
        createdAt
          & Time.utcTimeToPOSIXSeconds
          & floor
          & Builder.int64BE
          & Builder.toLazyByteString
          & BL.toStrict
      ]
      & CH.hashFinalize @CH.SHA3_512
      & BA.convert
      & Hash.fromByteString

handleHistoryComment :: Maybe BranchId2 -> Cli ()
handleHistoryComment mayThingToAnnotate = do
  Cli.Env {credentialManager} <- ask
  authorThumbprint <- PK.personalKeyThumbprint <$> liftIO (CredMan.getOrCreatePersonalKey credentialManager)
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
    Nothing -> Cli.respond $ CommentAborted
    Just (subject, content) -> do
      createdAt <- liftIO $ Time.getCurrentTime
      let historyComment =
            HistoryComment
              { author =
                  Config.unAuthorName authorName,
                commentId = (),
                causal = causalHash,
                createdAt,
                authorThumbprint
              }
      let commentHash = CommentHash $ contentHash historyComment
      let historyCommentRevision =
            HistoryCommentRevision
              { revisionId = (),
                subject,
                content,
                createdAt,
                comment = commentHash
              }
      let commentRevisionHash = CommentRevisionHash $ contentHash historyComment
      let hashedComment =
            historyCommentRevision {revisionId = commentRevisionHash, comment = historyComment {commentId = commentHash, causal = causalHashId}}

      Cli.runTransaction $ Q.commentOnCausal hashedComment
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
