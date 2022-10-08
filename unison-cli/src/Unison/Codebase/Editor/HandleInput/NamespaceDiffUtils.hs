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
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch0 (..))
import qualified Unison.Codebase.Branch.Names as Branch
import qualified Unison.Codebase.BranchDiff as BranchDiff
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBranchDiff
import qualified Unison.DataDeclaration as DD
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE hiding (biasTo)
import Unison.Reference (Reference (..))
import qualified Unison.Reference as Reference
import Unison.Symbol (Symbol)

diffHelper ::
  Branch0 IO ->
  Branch0 IO ->
  Cli r (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput Symbol Ann)
diffHelper before after =
  Cli.time "diffHelper" do
    Cli.Env {codebase} <- ask
    hqLength <- liftIO (Codebase.hashLength codebase)
    diff <- liftIO (BranchDiff.diff0 before after)
    ppe <- PPE.suffixifiedPPE <$> Cli.getCurrentPPED
    liftIO do
      fmap (ppe,) do
        OBranchDiff.toOutput
          (Codebase.getTypeOfReferent codebase)
          (declOrBuiltin codebase)
          hqLength
          (Branch.toNames before)
          (Branch.toNames after)
          ppe
          diff

declOrBuiltin :: Applicative m => Codebase m Symbol Ann -> Reference -> m (Maybe (DD.DeclOrBuiltin Symbol Ann))
declOrBuiltin codebase r = case r of
  Reference.Builtin {} ->
    pure . fmap DD.Builtin $ Map.lookup r Builtin.builtinConstructorType
  Reference.DerivedId id ->
    fmap DD.Decl <$> Codebase.getTypeDeclaration codebase id
