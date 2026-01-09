{-# LANGUAGE DataKinds #-}

module Unison.LSP.OpenOnShare
  ( openOnShareHandler,
  )
where

import Control.Lens hiding (List)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Network.URI (escapeURIString, isUnreserved)
import U.Codebase.Sqlite.Project
import U.Codebase.Sqlite.ProjectBranch
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.ProjectPath
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Core.Project (ProjectBranchName, ProjectName (..))
import Unison.Debug qualified as Debug
import Unison.LSP.FileAnalysis (ppedForFile)
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Syntax.HashQualified qualified as SyntaxHQ
import Web.Browser qualified as Web

data OpenOnShareParams = OpenOnShareParams
  { textDocument :: LSP.TextDocumentIdentifier,
    position :: LSP.Position
  }
  deriving (Show, Eq)

data DefinitionInfo
  = DefinitionInfo
  { definitionFQN :: Text,
    definitionBranchRef :: Text,
    definitionProjectRef :: Text,
    definitionKind :: Text
  }
  deriving (Show, Eq)

instance Aeson.FromJSON DefinitionInfo where
  parseJSON = Aeson.withObject "DefinitionInfo" $ \v -> do
    definitionFQN <- v Aeson..: "fqn"
    definitionBranchRef <- v Aeson..: "branchRef"
    definitionProjectRef <- v Aeson..: "projectRef"
    definitionKind <- v Aeson..: "kind"
    pure DefinitionInfo {definitionFQN, definitionBranchRef, definitionProjectRef, definitionKind}

data DefinitionSearchResponse = DefinitionSearchResponse
  { definitions :: [DefinitionInfo]
  }
  deriving (Show, Eq)

instance Aeson.FromJSON DefinitionSearchResponse where
  parseJSON = Aeson.withObject "DefinitionSearchResponse" $ \v -> do
    definitions <- v Aeson..: "results"
    pure DefinitionSearchResponse {definitions}

instance Aeson.FromJSON OpenOnShareParams where
  parseJSON = Aeson.withObject "OpenOnShareParams" $ \v -> do
    textDocument <- v Aeson..: "textDocument"
    position <- v Aeson..: "position"
    pure OpenOnShareParams {textDocument, position}

-- | Handler for the 'unison/openOnShare' custom LSP request.
-- This resolves the symbol at the given position to its FQN and makes an HTTP POST
-- to the local Share service.
openOnShareHandler ::
  Msg.TRequestMessage ('Msg.Method_CustomMethod "unison/openOnShare") ->
  (Either Msg.ResponseError (Msg.MessageResult ('Msg.Method_CustomMethod "unison/openOnShare")) -> Lsp ()) ->
  Lsp ()
openOnShareHandler m respond = do
  result <- runMaybeT . runExceptT $ do
    pp <- lift getCurrentProjectPath
    Env {codebase} <- ask
    Debug.debugM Debug.Temp "OpenOnShare:resolving names" $ pp
    ProjectAndBranch remoteProjectName remoteBranchName <- lift . MaybeT $ resolveRemoteProjectBranch codebase pp
    Debug.debugM Debug.Temp "OpenOnShare:names" $ (remoteProjectName, remoteBranchName)
    let paramsJSON = m ^. LSP.params
    OpenOnShareParams {textDocument, position} <- case Aeson.fromJSON paramsJSON of
      Aeson.Error err -> throwError $ Msg.ResponseError (InR ErrorCodes_InvalidParams) (Text.pack err) Nothing
      Aeson.Success p -> pure p

    -- Get the symbol reference at the position
    ref <- lift $ LSPQ.refAtPosition textDocument._uri position
    Debug.debugM Debug.Temp "OpenOnShare:ref" $ ref

    -- Get the FQN for the reference
    pped <- lift $ ppedForFile textDocument._uri
    let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
    let (fqn, kind) = case ref of
          LD.TypeReference typeRef -> (PPE.typeName unsuffixifiedPPE typeRef, "types")
          LD.TermReferent termRef -> (PPE.termName unsuffixifiedPPE termRef, "terms")
    let fqnText = SyntaxHQ.toText fqn
    let encodedProjectName = Text.pack $ escapeURIString isUnreserved (Text.unpack $ into @Text remoteProjectName)
    let encodedBranchName = Text.pack $ escapeURIString isUnreserved (Text.unpack $ into @Text remoteBranchName)

    -- E.g. https://share.unison-lang.org/@unison/base/code/releases/7.12.0/latest/terms/data/List/map
    let shareUrl =
          Text.unpack $
            "https://share.unison-lang.org/"
              <> encodedProjectName
              <> "/code/"
              <> encodedBranchName
              <> "/latest/"
              <> kind
              <> "/"
              <> Text.replace "." "/" fqnText
    void . liftIO $ Web.openBrowser shareUrl

  -- Send the response
  case result of
    Just (Left err) -> respond (Left err)
    _ -> respond (Right Aeson.Null)

resolveRemoteProjectBranch :: Codebase IO v a -> PP.ProjectPath -> Lsp (Maybe (ProjectAndBranch ProjectName ProjectBranchName))
resolveRemoteProjectBranch codebase pp = do
  liftIO $ Codebase.runTransaction codebase $ runMaybeT $ do
    (remoteProjectId, remoteBranchId) <- MaybeT $ Q.loadDefaultMergeTargetForLocalProjectBranch pp.project.projectId Share.hardCodedUri pp.branch.branchId
    Debug.debugM Debug.Temp "OpenOnShare:remote IDs" $ (remoteProjectId, remoteBranchId)
    remoteProjectName <- lift $ Q.expectRemoteProjectName remoteProjectId Share.hardCodedUri
    remoteProjectBranchName <- lift $ Q.expectRemoteProjectBranchName Share.hardCodedUri remoteProjectId remoteBranchId
    pure $ ProjectAndBranch remoteProjectName remoteProjectBranchName
