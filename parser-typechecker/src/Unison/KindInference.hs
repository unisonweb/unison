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
    kindCheckAnnotations,
    KindError,
  )
where

import Data.Foldable (foldlM)
import Data.Graph (flattenSCC, stronglyConnCompR)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation)
import Unison.DataDeclaration
import Unison.KindInference.Generate (declComponentConstraints, termConstraints)
import Unison.KindInference.Solve (KindError, defaultUnconstrainedVars, initialState, step, verify)
import Unison.KindInference.Solve.Monad (Env (..), SolveState, run, runGen)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PrettyPrintEnv
import Unison.Reference
import Unison.Term qualified as Term
import Unison.Var qualified as Var

-- | Check that all annotations in a term are well-kinded
kindCheckAnnotations ::
  forall v loc.
  (Var.Var v, Ord loc, Show loc, BuiltinAnnotation loc) =>
  PrettyPrintEnv.PrettyPrintEnv ->
  SolveState v loc ->
  Term.Term v loc ->
  Either (NonEmpty (KindError v loc)) ()
kindCheckAnnotations ppe st t =
  let (cs, st') = run env st (runGen $ termConstraints t)
      env = Env ppe
   in step env st' cs $> ()

-- | Infer the kinds of all decl vars
inferDecls ::
  forall v loc.
  (Var.Var v, BuiltinAnnotation loc, Ord loc, Show loc) =>
  PrettyPrintEnv.PrettyPrintEnv ->
  Map Reference (Decl v loc) ->
  Either (NonEmpty (KindError v loc)) (SolveState v loc)
inferDecls ppe declMap =
  let components :: [[(Reference, Decl v loc)]]
      components = intoComponents declMap

      env = Env ppe

      handleComponent ::
        SolveState v loc ->
        [(Reference, Decl v loc)] ->
        Either (NonEmpty (KindError v loc)) (SolveState v loc)
      handleComponent s c =
        let (cs, st) = run env s (runGen $ declComponentConstraints c)
         in step env st cs

      handleComponents ::
        [[(Reference, Decl v loc)]] ->
        Either (NonEmpty (KindError v loc)) (SolveState v loc)
      handleComponents = verify <=< foldlM phi (initialState env)
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
