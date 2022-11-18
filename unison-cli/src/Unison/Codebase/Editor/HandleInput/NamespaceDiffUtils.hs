-- | Helpers/utils that have to do with namespace diffs.
module Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils
  ( diffHelper,
  )
where

import Control.Monad.Reader (ask)
import qualified Data.Map as Map
import qualified Unison.Builtin as Builtin
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.PrettyPrintUtils (prettyPrintEnvDecl)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch0 (..))
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.BranchDiff as BranchDiff
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBranchDiff
import qualified Unison.Codebase.Path as Path
import qualified Unison.DataDeclaration as DD
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import Unison.Reference (Reference (..))
import qualified Unison.Reference as Reference
import qualified Unison.Server.Backend as Backend
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)

diffHelper ::
  Branch0 IO ->
  Branch0 IO ->
  Cli (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput Symbol Ann)
diffHelper before after =
  Cli.time "diffHelper" do
    Cli.Env {codebase} <- ask
    rootBranch <- Cli.getRootBranch
    currentPath <- Cli.getCurrentPath
    hqLength <- Cli.runTransaction Codebase.hashLength
    diff <- liftIO (BranchDiff.diff0 before after)
    let (_parseNames, prettyNames0, _local) = Backend.namesForBranch rootBranch (Backend.AllNames $ Path.unabsolute currentPath)
    ppe <- PPE.suffixifiedPPE <$> prettyPrintEnvDecl (NamesWithHistory prettyNames0 mempty)
    fmap (ppe,) do
      OBranchDiff.toOutput
        (Cli.runTransaction . Codebase.getTypeOfReferent codebase)
        (Cli.runTransaction . declOrBuiltin codebase)
        hqLength
        (Branch.toNames before)
        (Branch.toNames after)
        ppe
        diff

declOrBuiltin :: Codebase m Symbol Ann -> Reference -> Sqlite.Transaction (Maybe (DD.DeclOrBuiltin Symbol Ann))
declOrBuiltin codebase r = case r of
  Reference.Builtin {} ->
    pure . fmap DD.Builtin $ Map.lookup r Builtin.builtinConstructorType
  Reference.DerivedId id ->
    fmap DD.Decl <$> Codebase.getTypeDeclaration codebase id
