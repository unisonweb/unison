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
import Unison.KindInference.Solve (KindError, finalize, initialState, step)
import Unison.KindInference.Solve.Monad (Env (..), SolveState, run, runGenList)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PrettyPrintEnv
import Unison.Reference
import Unison.Term qualified as Term
import Unison.Var qualified as Var

kindCheckAnnotations ::
  forall v loc.
  (Var.Var v, Ord loc, Show loc, BuiltinAnnotation loc) =>
  PrettyPrintEnv.PrettyPrintEnv ->
  SolveState v loc ->
  Term.Term v loc ->
  Either (NonEmpty (KindError v loc)) ()
kindCheckAnnotations ppe st t =
  let (cs, st') = run env st (runGenList $ termConstraints t)
      env = Env ppe
   in step env st' cs $> ()

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
        let (cs, st) = run env s (runGenList $ declComponentConstraints c)
         in step env st cs

      handleComponents ::
        [[(Reference, Decl v loc)]] ->
        Either (NonEmpty (KindError v loc)) (SolveState v loc)
      handleComponents = finalize <=< foldlM phi (initialState env)
        where
          phi b a = handleComponent b a
   in handleComponents components

intoComponents :: forall v a. Ord v => Map Reference (Decl v a) -> [[(Reference, Decl v a)]]
intoComponents declMap =
  let graphInput :: [(Decl v a, Reference, [Reference])]
      graphInput = Map.foldrWithKey (\k a b -> (a, k, declReferences a) : b) [] declMap
   in map (\(a, b, _) -> (b, a)) . flattenSCC <$> stronglyConnCompR graphInput
  where
    declReferences :: Decl v a -> [Reference]
    declReferences = toList . typeDependencies . asDataDecl
