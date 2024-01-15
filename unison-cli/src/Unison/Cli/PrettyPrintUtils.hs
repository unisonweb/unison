-- | Utilities that have to do with constructing pretty-print environments, given stateful information in the Cli monad
-- state/environment, such as the current path.
module Unison.Cli.PrettyPrintUtils
  ( prettyPrintEnvDeclFromNames,
    currentPrettyPrintEnvDecl,
    projectRootPPED,
    projectRootPPEDWithoutTransitiveLibs,
  )
where

import Control.Monad.Reader
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names.Cache qualified as NamesCache
import Unison.Names (Names)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo)
import Unison.PrettyPrintEnvDecl.Names qualified as PPE

-- | Builds a pretty print env decl from a names object.
prettyPrintEnvDeclFromNames :: Names -> Cli PPE.PrettyPrintEnvDecl
prettyPrintEnvDeclFromNames ns =
  Cli.runTransaction Codebase.hashLength <&> (`PPE.fromNamesSuffixifiedByHash` ns)

-- | Get a pretty print env decl for the current names at the current path.
--
-- Prefer using 'prettyPrintEnvDeclFromNames' when you've already got
-- a 'Names' value around, since using 'currentPrettyPrintEnvDecl' rebuilds the underlying
-- names object.
currentPrettyPrintEnvDecl :: Cli PPE.PrettyPrintEnvDecl
currentPrettyPrintEnvDecl = do
  Cli.currentNames >>= prettyPrintEnvDeclFromNames

projectRootPPED :: Cli PPE.PrettyPrintEnvDecl
projectRootPPED = do
  Cli.Env {codebase} <- ask
  causalHash <- Branch.headHash <$> Cli.getProjectRootBranch
  NamesCache.BranchNames {branchPPED} <- liftIO $ NamesCache.expectNamesForBranch codebase causalHash
  pure branchPPED

projectRootPPEDWithoutTransitiveLibs :: Cli PPE.PrettyPrintEnvDecl
projectRootPPEDWithoutTransitiveLibs = do
  Cli.Env {codebase} <- ask
  causalHash <- Branch.headHash <$> Cli.getProjectRootBranch
  NamesCache.BranchNames {branchPPEDWithoutTransitiveLibs} <- liftIO $ NamesCache.expectNamesForBranch codebase causalHash
  pure branchPPEDWithoutTransitiveLibs
