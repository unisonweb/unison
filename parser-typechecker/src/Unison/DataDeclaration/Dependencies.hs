module Unison.DataDeclaration.Dependencies
  ( -- Too many variants of decl dependencies. Read carefully to choose the right one.
    DD.declTypeDependencies,
    DD.typeDependencies,
    hashFieldAccessors,
  )
where

import Control.Lens
import Data.Map qualified as Map
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Records (generateRecordAccessors)
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Reference (TermReferenceId, TypeReference)
import Unison.Result qualified as Result
import Unison.Syntax.Var qualified as Var (namespaced)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup (..))
import Unison.Util.Tuple qualified as Tuple
import Unison.Var qualified as Var

-- | Generate Referents for all possible field accessors of a Decl.
--
-- Returns @Nothing@ if inferring/typechecking of any accessor fails, which shouldn't normally happen, but does when
-- record fields are higher rank, because the higher rank types can't be inferred.
--
-- See https://github.com/unisonweb/unison/issues/498
hashFieldAccessors ::
  forall v a.
  (Var.Var v) =>
  PrettyPrintEnv ->
  v ->
  [v] ->
  TypeReference ->
  DD.DataDeclaration v a ->
  Maybe (Map v (TermReferenceId, Term v (), Type v ()))
hashFieldAccessors ppe declName vars declRef dd = do
  let accessors :: [(v, (), Term v ())]
      accessors =
        generateRecordAccessors Var.namespaced id (map (,()) vars) declName declRef

  typecheckedAccessors <-
    for accessors \(v, _a, term) -> do
      typ <- typecheck term
      Just (v, (term, typ, ()))

  typecheckedAccessors
    & Map.fromList
    & Hashing.hashTermComponents
    & Map.map Tuple.drop4th
    & Just
  where
    typecheck :: Term v () -> Maybe (Type v ())
    typecheck term = do
      typ <- Result.result (Typechecker.synthesize ppe Typechecker.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled typecheckingEnv term)
      -- Note: Typechecker.synthesize doesn't normalize the output
      -- type. We do so here using `Type.cleanup`, mirroring what's
      -- done when typechecking a whole file and ensuring we get the
      -- same inferred type.
      Just (Type.cleanup typ)

    typecheckingEnv :: Typechecker.Env v ()
    typecheckingEnv =
      Typechecker.Env
        { ambientAbilities = mempty,
          typeLookup =
            TypeLookup
              { typeOfTerms = mempty,
                dataDecls = Map.singleton declRef (void dd),
                effectDecls = mempty
              },
          termsByShortname = mempty,
          topLevelComponents = Map.empty
        }
