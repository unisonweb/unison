module Unison.DataDeclaration.Dependencies
  ( -- Too many variants of decl dependencies. Read carefully to choose the right one.
    DD.declTypeDependencies,
    DD.typeDependencies,
    DD.labeledTypeDependencies,
    DD.labeledDeclTypeDependencies,
    DD.labeledDeclDependenciesIncludingSelf,
    labeledDeclDependenciesIncludingSelfAndFieldAccessors,
    hashFieldAccessors,
  )
where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import U.Codebase.Reference qualified as V2Reference
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Records (generateRecordAccessors)
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
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup (..))
import Unison.Var (Var)
import Unison.Var qualified as Var

-- | Generate the LabeledDependencies for everything in a Decl, including the Decl itself, all
-- its constructors, all referenced types, and all possible record accessors.
--
-- Note that we can't actually tell whether the Decl was originally a record or not, so we
-- include all possible accessors, but they may or may not exist in the codebase.
labeledDeclDependenciesIncludingSelfAndFieldAccessors :: Var v => V2Reference.TypeReference -> (DD.Decl v a) -> Set LD.LabeledDependency
labeledDeclDependenciesIncludingSelfAndFieldAccessors selfRef decl =
  DD.labeledDeclDependenciesIncludingSelf selfRef decl
    <> case decl of
      Left _effect -> mempty
      Right dataDecl ->
        fieldAccessorRefs selfRef dataDecl
          & maybe Set.empty (Set.map LD.TermReferent)

-- | Generate Referents for all possible field accessors of a Decl.
--
-- Returns Nothing if this couldn't be a record because it doesn't contain exactly one constructor.
fieldAccessorRefs :: forall v a. (Var v) => Reference -> DD.DataDeclaration v a -> Maybe (Set Referent)
fieldAccessorRefs declRef dd = do
  [(_, typ)] <- Just (DD.constructors dd)

  -- This name isn't important, we just need a name to generate field names from.
  -- The field names are thrown away afterwards.
  let typeName = Var.named "Type"
  -- These names are arbitrary and don't show up anywhere.
  let vars :: [v]
      -- We add `n` to the end of the variable name as a quick fix to #4752, but we suspect there's a more
      -- fundamental fix to be made somewhere in the term printer to automatically suffix a var name with its
      -- freshened id if it would be ambiguous otherwise.
      vars = [Var.freshenId (fromIntegral n) (Var.named ("_" <> tShow n)) | n <- [0 .. Type.arity typ - 1]]
  hashFieldAccessors PPE.empty typeName vars declRef dd
    & Map.elems
    & setOf (folded . _1 . to (Reference.DerivedId >>> Referent.Ref))
    & Just

-- | Generate Referents for all possible field accessors of a Decl.
hashFieldAccessors ::
  forall v a.
  (Var.Var v) =>
  PrettyPrintEnv ->
  v ->
  [v] ->
  Reference ->
  DD.DataDeclaration v a ->
  Map v (Reference.Id, Term.Term v (), Type.Type v ())
hashFieldAccessors ppe declName vars declRef dd =
  generateRecordAccessors Var.namespaced id (map (,()) vars) declName declRef
    & map (\(v, _a, term) -> (v, (term, typecheck term, ())))
    & Map.fromList
    & Hashing.hashTermComponents
    & fmap (\(rid, trm, typ, _a) -> (rid, trm, typ))
  where
    typecheck :: Term v () -> Type v ()
    typecheck term =
      case Typechecker.synthesize ppe Typechecker.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled typecheckingEnv term of
        Result.Result _notes Nothing ->
          error (reportBug "E497674" ("Generated record accessors of " ++ show declRef ++ " didn't typecheck"))
        -- Note: Typechecker.synthesize doesn't normalize the output
        -- type. We do so here using `Type.cleanup`, mirroring what's
        -- done when typechecking a whole file and ensuring we get the
        -- same inferred type.
        Result.Result _notes (Just typ) -> Type.cleanup typ
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
          termsByShortname = mempty
        }
