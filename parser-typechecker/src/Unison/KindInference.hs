-- | Kind inference for Unison
--
-- Unison has Type, ->, and Ability kinds
--
-- An algorithm sketch: First break all decls into strongly connected
-- components in reverse topological order. Then, for each component,
-- generate kind constraints that arise from the constructors in the
-- decl to discover constraints on the decl vars. These constraints
-- are then given to a constraint solver that determines a unique kind
-- for each type variable. Unconstrained variables are defaulted to
-- kind Type (just like Haskell 98). This is done by 'inferDecls'.
--
-- Afterwards, the 'SolveState' holds the kinds of all decls and we
-- can check that type annotations in terms that may mention the
-- decls are well-kinded with 'kindCheckAnnotations'.
module Unison.KindInference
  ( inferDecls,
    inferDeclsFromState,
    inferAliases,
    kindCheckAnnotations,
    initialState,
    kindEnv,
    KindError,
  )
where

import Data.Foldable (foldlM)
import Data.Graph (flattenSCC, stronglyConnCompR)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as Nel
import Data.Map.Strict qualified as Map
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation)
import Unison.DataDeclaration
import Unison.KindInference.Generate (aliasComponentConstraints, declComponentConstraints, termConstraints)
import Unison.KindInference.Solve (KindError (..), defaultUnconstrainedVars, initialState, step, verify)
import Unison.KindInference.Solve.Monad (Env (..), SolveState, runGen, runSolve)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PrettyPrintEnv
import Unison.Reference
import Unison.Term qualified as Term
import Unison.TypeAlias (TypeAlias)
import Unison.Var qualified as Var

-- | Build a 'Env' used by the solver from a 'PrettyPrintEnv'.
kindEnv :: PrettyPrintEnv.PrettyPrintEnv -> Env
kindEnv = Env

-- | Check that all annotations in a term are well-kinded
kindCheckAnnotations ::
  forall v loc.
  (Var.Var v, Ord loc, Show loc, BuiltinAnnotation loc) =>
  PrettyPrintEnv.PrettyPrintEnv ->
  SolveState v loc ->
  Term.Term v loc ->
  Either (NonEmpty (KindError v loc)) ()
kindCheckAnnotations ppe st t = do
  let env = Env ppe
  (cs, st') <- mapLeft (Nel.singleton . SolveError) $ runSolve env st (runGen $ termConstraints t)
  step env st' cs $> ()

-- | Infer the kinds of all decl vars
inferDecls ::
  forall v loc.
  (Var.Var v, BuiltinAnnotation loc, Ord loc, Show loc) =>
  PrettyPrintEnv.PrettyPrintEnv ->
  Map Reference (Decl v loc) ->
  Either (NonEmpty (KindError v loc)) (SolveState v loc)
inferDecls ppe = inferDeclsFromState ppe (initialState (Env ppe))

-- | Like 'inferDecls', but threads an existing 'SolveState' (lets
-- callers pre-populate it, e.g. with alias refs before decls run).
inferDeclsFromState ::
  forall v loc.
  (Var.Var v, BuiltinAnnotation loc, Ord loc, Show loc) =>
  PrettyPrintEnv.PrettyPrintEnv ->
  SolveState v loc ->
  Map Reference (Decl v loc) ->
  Either (NonEmpty (KindError v loc)) (SolveState v loc)
inferDeclsFromState ppe initState declMap =
  let components :: [[(Reference, Decl v loc)]]
      components = intoComponents declMap

      env = Env ppe

      handleComponent ::
        SolveState v loc ->
        [(Reference, Decl v loc)] ->
        Either (NonEmpty (KindError v loc)) (SolveState v loc)
      handleComponent s c = do
        (cs, st) <- mapLeft (Nel.singleton . SolveError) $ runSolve env s (runGen $ declComponentConstraints c)
        step env st cs

      handleComponents ::
        [[(Reference, Decl v loc)]] ->
        Either (NonEmpty (KindError v loc)) (SolveState v loc)
      handleComponents = verify <=< foldlM phi initState
        where
          phi b a = handleComponent b a
   in defaultUnconstrainedVars <$> handleComponents components

-- | Break the decls into strongly connected components in reverse
-- topological order
intoComponents :: forall v a. (Ord v) => Map Reference (Decl v a) -> [[(Reference, Decl v a)]]
intoComponents declMap =
  let graphInput :: [(Decl v a, Reference, [Reference])]
      graphInput = Map.foldrWithKey (\k a b -> (a, k, declReferences a) : b) [] declMap
   in map (\(a, b, _) -> (b, a)) . flattenSCC <$> stronglyConnCompR graphInput
  where
    declReferences :: Decl v a -> [Reference]
    declReferences = toList . typeDependencies . asDataDecl

-- | Extend an existing 'SolveState' with kind info for the given type
-- aliases. Aliases are processed all at once; their bodies must reference
-- only decls (already in the SolveState) or other aliases in this batch.
inferAliases ::
  forall v loc.
  (Var.Var v, BuiltinAnnotation loc, Ord loc, Show loc) =>
  PrettyPrintEnv.PrettyPrintEnv ->
  SolveState v loc ->
  Map Reference (TypeAlias v loc) ->
  Either (NonEmpty (KindError v loc)) (SolveState v loc)
inferAliases ppe st0 aliasMap
  | Map.null aliasMap = Right st0
  | otherwise =
      let env = Env ppe
          aliases = Map.toList aliasMap
       in do
            (cs, st) <- mapLeft (Nel.singleton . SolveError) $ runSolve env st0 (runGen $ aliasComponentConstraints aliases)
            step env st cs
