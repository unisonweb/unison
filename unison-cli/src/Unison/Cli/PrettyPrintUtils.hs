-- | Utilities that have to do with constructing pretty-print environments, given stateful information in the Cli monad
-- state/environment, such as the current path.
module Unison.Cli.PrettyPrintUtils
  ( prettyPrintEnvDeclFromNames,
    prettyPrintEnvDeclFromNames3,
    currentPrettyPrintEnvDecl,
    projectBranchPPED,
  )
where

import U.Codebase.Sqlite.ProjectBranch (ProjectBranch)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Namer qualified as Namer
import Unison.Names (Names)
import Unison.Names3 (Names3 (..))
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Suffixifier qualified as Suffixifier

-- | Builds a pretty print env decl from a names object.
--
-- TODO delete me
prettyPrintEnvDeclFromNames :: Names -> Cli PPE.PrettyPrintEnvDecl
prettyPrintEnvDeclFromNames ns =
  prettyPrintEnvDeclFromNames3 Names3 {local = ns, directDeps = mempty, indirectDeps = mempty}

-- | Builds a pretty print env decl from a names object.
prettyPrintEnvDeclFromNames3 :: Names3 -> Cli PPE.PrettyPrintEnvDecl
prettyPrintEnvDeclFromNames3 ns =
  Cli.runTransaction Codebase.hashLength <&> \hashLen ->
    PPED.makePPED (Namer.makeHqNamer hashLen ns) (Suffixifier.suffixifyByHash ns)

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
