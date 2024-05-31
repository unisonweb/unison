-- | Helpers/utils that have to do with namespace diffs.
module Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils
  ( diffHelper,
    diffFromTypecheckedUnisonFile,
  )
where

import Control.Monad.Reader (ask)
import Data.Map qualified as Map
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchDiff qualified as BranchDiff
import Unison.Codebase.Editor.Output.BranchDiff qualified as OBranchDiff
import Unison.DataDeclaration qualified as DD
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as Names

diffHelper ::
  Branch0 IO ->
  Branch0 IO ->
  Cli (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput Symbol Ann)
diffHelper before after =
  Cli.time "diffHelper" do
    Cli.Env {codebase} <- ask
    hqLength <- Cli.runTransaction Codebase.hashLength
    diff <- liftIO (BranchDiff.diff0 before after)
    names <- Cli.currentNames
    pped <- Cli.prettyPrintEnvDeclFromNames names
    let suffixifiedPPE = PPED.suffixifiedPPE pped
    fmap (suffixifiedPPE,) do
      OBranchDiff.toOutput
        (Cli.runTransaction . Codebase.getTypeOfReferent codebase)
        (Cli.runTransaction . declOrBuiltin codebase)
        hqLength
        (Branch.toNames before)
        (Branch.toNames after)
        diff

-- | Like diffHelper, but allows providing definitions from a file which may not have been added to the codebase
-- yet.
diffFromTypecheckedUnisonFile ::
  TypecheckedUnisonFile Symbol Ann ->
  Branch0 IO ->
  Branch0 IO ->
  Cli (PPE.PrettyPrintEnv, OBranchDiff.BranchDiffOutput Symbol Ann)
diffFromTypecheckedUnisonFile tf before after = do
  Cli.time "diffFromTypecheckedUnisonFile" do
    Cli.Env {codebase} <- ask
    hqLength <- Cli.runTransaction Codebase.hashLength
    diff <- liftIO (BranchDiff.diff0 before after)
    names <- Cli.currentNames
    pped <- Cli.prettyPrintEnvDeclFromNames names
    let suffixifiedPPE = PPED.suffixifiedPPE pped
    let beforeNames = Branch.toNames before
    let afterNames = Names.addNamesFromTypeCheckedUnisonFile tf (Branch.toNames after)
    fmap (suffixifiedPPE,) do
      OBranchDiff.toOutput
        (getTypeOfReferent codebase)
        (getDeclOrBuiltin codebase)
        hqLength
        beforeNames
        afterNames
        diff
  where
    TL.TypeLookup {dataDecls, effectDecls} = UF.typeLookupForTypecheckedFile tf
    referentTypeFromFile :: Referent.Referent -> (Maybe (Type Symbol Ann))
    referentTypeFromFile ref = UF.typeOfReferentFromTypecheckedUnisonFile tf ref
    getDeclOrBuiltin :: Codebase m Symbol Ann -> Reference -> Cli (Maybe (DD.DeclOrBuiltin Symbol Ann))
    getDeclOrBuiltin codebase ref = runMaybeT do
      hoistMaybe (Map.lookup ref dataDecls <&> DD.Decl . Right)
        <|> hoistMaybe ((Map.lookup ref effectDecls) <&> DD.Decl . Left)
        <|> (MaybeT (Cli.runTransaction $ declOrBuiltin codebase ref))
    getTypeOfReferent codebase ref =
      runMaybeT $
        do
          (hoistMaybe $ referentTypeFromFile ref)
          <|> (MaybeT . Cli.runTransaction $ Codebase.getTypeOfReferent codebase ref)

declOrBuiltin :: Codebase m Symbol Ann -> Reference -> Sqlite.Transaction (Maybe (DD.DeclOrBuiltin Symbol Ann))
declOrBuiltin codebase r = case r of
  Reference.Builtin {} ->
    pure . fmap DD.Builtin $ Map.lookup r Builtin.builtinConstructorType
  Reference.DerivedId id ->
    fmap DD.Decl <$> Codebase.getTypeDeclaration codebase id
