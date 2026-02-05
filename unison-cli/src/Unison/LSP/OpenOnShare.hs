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
import U.Codebase.Sqlite.Project
import U.Codebase.Sqlite.ProjectBranch
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.ProjectPath
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Core.Project (ProjectBranchName, ProjectName (..))
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

instance Aeson.FromJSON OpenOnShareParams where
  parseJSON = Aeson.withObject "OpenOnShareParams" $ \v -> do
    textDocument <- v Aeson..: "textDocument"
    position <- v Aeson..: "position"
    pure OpenOnShareParams {textDocument, position}

data OpenOnShareResponse = OpenOnShareResponse
  { error :: Maybe Text
  }
  deriving (Show, Eq)

instance Aeson.ToJSON OpenOnShareResponse where
  toJSON (OpenOnShareResponse err) =
    Aeson.object
      [ "error" Aeson..= err
      ]

-- | Handler for the 'unison/openOnShare' custom LSP request.
-- This resolves the symbol at the given position to its FQN and makes an HTTP POST
-- to the local Share service.
openOnShareHandler ::
  Msg.TRequestMessage ('Msg.Method_CustomMethod "unison/openOnShare") ->
  (Either (Msg.TResponseError ('Msg.Method_CustomMethod "unison/openOnShare")) Aeson.Value -> Lsp ()) ->
  Lsp ()
openOnShareHandler m respond = do
  result <- runExceptT $ do
    pp <- lift getCurrentProjectPath
    Env {codebase} <- ask
    ProjectAndBranch remoteProjectName remoteBranchName <- orFail "No Share project found, have you pushed or pulled it yet?" $ resolveRemoteProjectBranch codebase pp
    let paramsJSON = m ^. LSP.params
    OpenOnShareParams {textDocument, position} <- case Aeson.fromJSON paramsJSON of
      Aeson.Error err -> throwError $ "Invalid parameters: " <> Text.pack err
      Aeson.Success p -> pure p

    -- Get the symbol reference at the position
    ref <- orFail "Error: Definition not found in Codebase" . runMaybeT $ LSPQ.refAtPosition textDocument._uri position

    -- Get the FQN for the reference
    pped <- lift $ ppedForFile textDocument._uri
    let unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
    let (fqn, kind) = case ref of
          LD.TypeReference typeRef -> (PPE.typeName unsuffixifiedPPE typeRef, "types")
          LD.TermReferent termRef -> (PPE.termName unsuffixifiedPPE termRef, "terms")
    let fqnText = SyntaxHQ.toText fqn

    let shareUrl =
          Text.unpack $
            "https://share.unison-lang.org/"
              <> into @Text remoteProjectName
              <> "/code/"
              <> into @Text remoteBranchName
              <> "/latest/"
              <> kind
              <> "/"
              <> Text.replace "." "/" fqnText
    void . liftIO $ Web.openBrowser shareUrl

  -- Send the response
  case result of
    (Left errMsg) -> respond (Right $ Aeson.toJSON $ OpenOnShareResponse (Just errMsg))
    _ -> respond (Right $ Aeson.toJSON $ OpenOnShareResponse Nothing)
  where
    orFail :: Text -> Lsp (Maybe a) -> ExceptT Text Lsp a
    orFail err action = do
      ma <- lift action
      case ma of
        Just a -> pure a
        Nothing -> throwError err

resolveRemoteProjectBranch :: Codebase IO v a -> PP.ProjectPath -> Lsp (Maybe (ProjectAndBranch ProjectName ProjectBranchName))
resolveRemoteProjectBranch codebase pp = do
  liftIO $ Codebase.runTransaction codebase $ do
    let ids = (ProjectAndBranch pp.project.projectId pp.branch.branchId)
    Q.resolveRemoteProjectBranchNames ids Share.hardCodedUri
