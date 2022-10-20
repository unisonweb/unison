{-# LANGUAGE RecordWildCards #-}

module Unison.DataDeclaration.Names (bindNames, dataDeclToNames', effectDeclToNames') where

import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (DataDeclaration (DataDeclaration), EffectDeclaration)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Name as Name
import Unison.Names (Names (Names))
import qualified Unison.Names.ResolutionResult as Names
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Type.Names as Type.Names
import qualified Unison.Util.Relation as Rel
import Unison.Var (Var)
import Prelude hiding (cycle)

-- implementation of dataDeclToNames and effectDeclToNames
toNames :: Var v => (v -> Name.Name) -> CT.ConstructorType -> v -> Reference.Id -> DataDeclaration v a -> Names
toNames unsafeVarToName ct typeSymbol (Reference.DerivedId -> r) dd =
  -- constructor names
  foldMap names (DD.constructorVars dd `zip` [0 ..])
    -- name of the type itself
    <> Names mempty (Rel.singleton (unsafeVarToName typeSymbol) r)
  where
    names (ctor, i) =
      Names (Rel.singleton (unsafeVarToName ctor) (Referent.Con (ConstructorReference r i) ct)) mempty

dataDeclToNames :: Var v => (v -> Name.Name) -> v -> Reference.Id -> DataDeclaration v a -> Names
dataDeclToNames unsafeVarToName = toNames unsafeVarToName CT.Data

effectDeclToNames :: Var v => (v -> Name.Name) -> v -> Reference.Id -> EffectDeclaration v a -> Names
effectDeclToNames unsafeVarToName typeSymbol r ed = toNames unsafeVarToName CT.Effect typeSymbol r $ DD.toDataDecl ed

dataDeclToNames' :: Var v => (v -> Name.Name) -> (v, (Reference.Id, DataDeclaration v a)) -> Names
dataDeclToNames' unsafeVarToName (v, (r, d)) = dataDeclToNames unsafeVarToName v r d

effectDeclToNames' :: Var v => (v -> Name.Name) -> (v, (Reference.Id, EffectDeclaration v a)) -> Names
effectDeclToNames' unsafeVarToName (v, (r, d)) = effectDeclToNames unsafeVarToName v r d

bindNames ::
  Var v =>
  (v -> Name.Name) ->
  Set v ->
  Names ->
  DataDeclaration v a ->
  Names.ResolutionResult v a (DataDeclaration v a)
bindNames unsafeVarToName keepFree names (DataDeclaration m a bound constructors) = do
  constructors <- for constructors $ \(a, v, ty) ->
    (a,v,) <$> Type.Names.bindNames unsafeVarToName keepFree names ty
  pure $ DataDeclaration m a bound constructors
