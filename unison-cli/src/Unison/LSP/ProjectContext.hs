{-# LANGUAGE DataKinds #-}

module Unison.LSP.ProjectContext
  ( projectContextHandler,
  )
where

import Data.Aeson qualified as Aeson
import Language.LSP.Protocol.Message qualified as Msg
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Core.Project
import Unison.LSP.Types

data ProjectContextResponse = ProjectContextSuccess
  { projectName :: ProjectName,
    projectBranch :: ProjectBranchName
  }
  deriving (Show, Eq)

instance Aeson.ToJSON ProjectContextResponse where
  toJSON = \case
    ProjectContextSuccess {projectName, projectBranch} ->
      Aeson.object
        [ "projectName" Aeson..= projectName,
          "projectBranch" Aeson..= projectBranch
        ]

-- | Handler for the 'unison/projectContext' custom LSP request.
-- This returns the current project's name and branch.
projectContextHandler ::
  Msg.TRequestMessage ('Msg.Method_CustomMethod "unison/projectContext") ->
  (Either Msg.ResponseError Aeson.Value -> Lsp ()) ->
  Lsp ()
projectContextHandler _m respond = do
  pp <- getCurrentProjectPath
  let PP.ProjectPath {project, branch} = PP.toNames pp
  respond . Right . Aeson.toJSON $ ProjectContextSuccess project branch
