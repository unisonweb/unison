-- | Utilities that have to do with constructing pretty-print environments, given stateful information in the Cli monad
-- state/environment, such as the current path.
module Unison.Cli.PrettyPrintUtils
  ( prettyPrintEnvDeclFromNames,
    currentPrettyPrintEnvDecl,
    projectBranchPPED,
  )
where

import U.Codebase.Sqlite.ProjectBranch (ProjectBranch)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Names (Names)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED

-- | Builds a pretty print env decl from a names object.
prettyPrintEnvDeclFromNames :: Names -> Cli PPE.PrettyPrintEnvDecl
prettyPrintEnvDeclFromNames ns =
  Cli.runTransaction Codebase.hashLength <&> \hashLen ->
    PPED.makePPED (PPE.hqNamer hashLen ns) (PPE.suffixifyByHash ns)

-- | Get a pretty print env decl for the current names at the current path.
--
-- Prefer using 'prettyPrintEnvDeclFromNames' when you've already got
-- a 'Names' value around, since using 'currentPrettyPrintEnvDecl' rebuilds the underlying
-- names object.
currentPrettyPrintEnvDecl :: Cli PPE.PrettyPrintEnvDecl
currentPrettyPrintEnvDecl = do
  Cli.currentNames >>= prettyPrintEnvDeclFromNames

projectBranchPPED :: ProjectBranch -> Cli PPED.PrettyPrintEnvDecl
projectBranchPPED pb = do
  Cli.projectBranchNames pb >>= prettyPrintEnvDeclFromNames
