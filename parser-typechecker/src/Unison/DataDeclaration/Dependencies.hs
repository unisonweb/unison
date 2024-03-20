module Unison.DataDeclaration.Dependencies
  ( -- Too many variants of decl dependencies. Read carefully to choose the right one.
    DD.declTypeDependencies,
    DD.typeDependencies,
    DD.labeledTypeDependencies,
    DD.labeledDeclTypeDependencies,
    DD.labeledDeclDependenciesIncludingSelf,
    labeledDeclDependenciesIncludingSelfAndFieldAccessors,
    fieldAccessorRefs,
    hashFieldAccessors,
  )
where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import U.Codebase.Reference qualified as V2Reference
import Unison.DataDeclaration qualified as DD
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Syntax.Var qualified as Var (namespaced)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup (..))
import Unison.Typechecker.TypeLookup qualified as TypeLookup
import Unison.Var qualified as Var

-- | Generate the LabeledDependencies for everything in a Decl, including the Decl itself, all
-- its constructors, all referenced types, and all possible record accessors.
--
-- Note that we can't actually tell whether the Decl was originally a record or not, so we
-- include all possible accessors, but they may or may not exist in the codebase.
labeledDeclDependenciesIncludingSelfAndFieldAccessors :: Var.Var v => V2Reference.TypeReference -> (DD.Decl v a) -> Set LD.LabeledDependency
labeledDeclDependenciesIncludingSelfAndFieldAccessors selfRef decl =
  DD.labeledDeclDependenciesIncludingSelf selfRef decl
    <> case decl of
      Left _effect -> mempty
      Right dataDecl ->
        fieldAccessorRefs selfRef dataDecl
          & maybe Set.empty (Set.map LD.TermReferent)

-- | Generate Referents for all possible field accessors of a Decl.
-- Returns 'Nothing' if typechecking of any accessor fails.
fieldAccessorRefs :: forall v a. (Var.Var v) => Reference -> DD.DataDeclaration v a -> Maybe (Set Referent)
fieldAccessorRefs declRef dd = do
  -- This ppe is only used for typechecking errors.
  let ppe = PPE.empty
  typ <- case DD.constructors dd of
    [(_, typ)] -> Just typ
    _ -> Nothing
  -- This name isn't important, we just need a name to generate field names from.
  -- The field names are thrown away afterwards.
  let typeName = Var.named "Type"
  -- These names are arbitrary and don't show up anywhere.
  let vars :: [v]
      vars = [Var.freshenId (fromIntegral n) (Var.named "_") | n <- [0 .. Type.arity typ - 1]]
  hashFieldAccessors ppe typeName vars declRef dd
    <&> \accs ->
      Map.elems accs
        & setOf (folded . _1 . to (Reference.DerivedId >>> Referent.Ref))

-- | Generate Referents for all possible field accessors of a Decl.
-- Returns 'Nothing' if typechecking of any accessor fails (which shouldn't happen).
hashFieldAccessors ::
  forall v a.
  (Var.Var v) =>
  PrettyPrintEnv ->
  v ->
  [v] ->
  Reference ->
  DD.DataDeclaration v a ->
  ( Maybe
      (Map v (Reference.Id, Term.Term v (), Type.Type v ()))
  )
hashFieldAccessors ppe declName vars declRef dd = do
  let accessors :: [(v, (), Term.Term v ())]
      accessors = DD.generateRecordAccessors Var.namespaced mempty (map (,()) vars) declName declRef
  let typeLookup :: TypeLookup v ()
      typeLookup =
        TypeLookup
          { TypeLookup.typeOfTerms = mempty,
            TypeLookup.dataDecls = Map.singleton declRef (void dd),
            TypeLookup.effectDecls = mempty
          }
  let typecheckingEnv :: Typechecker.Env v ()
      typecheckingEnv =
        Typechecker.Env
          { ambientAbilities = mempty,
            typeLookup,
            termsByShortname = mempty
          }
  accessorsWithTypes :: [(v, Term.Term v (), Type.Type v ())] <-
    for accessors \(v, _a, trm) ->
      case Result.result (Typechecker.synthesize ppe Typechecker.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled typecheckingEnv trm) of
        Nothing -> Nothing
        -- Note: Typechecker.synthesize doesn't normalize the output
        -- type. We do so here using `Type.cleanup`, mirroring what's
        -- done when typechecking a whole file and ensuring we get the
        -- same inferred type.
        Just typ -> Just (v, trm, Type.cleanup typ)
  pure $
    accessorsWithTypes
      & fmap (\(v, trm, typ) -> (v, (trm, typ, ())))
      & Map.fromList
      & Hashing.hashTermComponents
      & fmap (\(id, trm, typ, _a) -> (id, trm, typ))
