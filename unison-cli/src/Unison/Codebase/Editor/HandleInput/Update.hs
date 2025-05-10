module Unison.Codebase.Editor.HandleInput.Update
  ( doSlurpAdds,
  )
where

import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import Unison.Codebase.Editor.SlurpComponent qualified as SC
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (unsafeParseVar)
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Var qualified as Var

-- updates the namespace for adding `slurp`
doSlurpAdds ::
  forall m.
  (Monad m) =>
  SlurpComponent ->
  TypecheckedUnisonFile Symbol Ann ->
  (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.batchUpdates (typeActions <> termActions)
  where
    typeActions = map doType . toList $ SC.types slurp
    termActions =
      map doTerm . toList $
        SC.terms slurp <> UF.constructorsForDecls (SC.types slurp) uf
    names = UF.typecheckedToNames uf
    doTerm :: Symbol -> (Path, Branch0 m -> Branch0 m)
    doTerm v = case toList (Names.termsNamed names (Name.unsafeParseVar v)) of
      [] -> errorMissingVar v
      [r] ->
        let split = Path.splitFromName (Name.unsafeParseVar v)
         in BranchUtil.makeAddTermName split r
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple terms named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    doType :: Symbol -> (Path, Branch0 m -> Branch0 m)
    doType v = case toList (Names.typesNamed names (Name.unsafeParseVar v)) of
      [] -> errorMissingVar v
      [r] ->
        let split = Path.splitFromName (Name.unsafeParseVar v)
         in BranchUtil.makeAddTypeName split r
      wha ->
        error $
          "Unison bug, typechecked file w/ multiple types named "
            <> Var.nameStr v
            <> ": "
            <> show wha
    errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf
