module Unison.TypeAlias.Expand
  ( normalize,
    NormalizationError (..),
  )
where

import Data.List (nubBy)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.TypeAlias (TypeAlias (..))
import Unison.TypeAlias qualified as TypeAlias
import Unison.Var (Var)

-- | Errors produced while normalizing a set of aliases.
data NormalizationError v
  = -- | Aliases form a dependency cycle. The set names the alias members
    -- involved in some cycle.
    AliasCycle (Set v)
  deriving stock (Show, Eq)

-- | Normalize a set of aliases so each body no longer mentions any other
-- alias from the input map. Detects dependency cycles.
--
-- After normalization, alias bodies are alias-free at the Var level — any
-- cross-alias references have been inlined. This runs at parse time,
-- before name resolution; the typechecker handles use-site expansion of
-- alias refs lazily via 'Unison.Typechecker.Context.whnfAlias'.
normalize ::
  forall v a.
  (Var v, Semigroup a) =>
  Map v (TypeAlias v a) ->
  Either (NormalizationError v) (Map v (TypeAlias v a))
normalize aliases = do
  order <- topoSort aliases
  pure (foldl' step Map.empty order)
  where
    step :: Map v (TypeAlias v a) -> v -> Map v (TypeAlias v a)
    step acc v =
      let alias = aliases Map.! v
          body' = expandVarRefs acc alias.body
       in Map.insert v (alias {body = body'}) acc

-- | Substitute alias-name Vars in a type with the alias's body. Used
-- internally by 'normalize' to flatten alias-of-alias references at
-- parse time.
--
-- Aliases referenced in the type must be present in the given map.
-- Unknown alias names pass through as ordinary variables. Aliases are
-- assumed saturated; under-application leaves the type as-is (downstream
-- typechecking will report the real error).
expandVarRefs ::
  forall v a.
  (Var v, Semigroup a) =>
  Map v (TypeAlias v a) ->
  Type v a ->
  Type v a
expandVarRefs aliases = go
  where
    go :: Type v a -> Type v a
    go t = case Type.unApps t of
      Just (Type.Var' v, args)
        | Just alias <- Map.lookup v aliases,
          length args >= TypeAlias.arity alias ->
            let arity = TypeAlias.arity alias
                (sat, extra) = splitAt arity args
                body' = ABT.substsInheritAnnotation (zip alias.paramNames (map go sat)) alias.body
             in Type.apps' (go body') (map go extra)
      _ -> case ABT.out t of
        ABT.Var v
          | Just alias <- Map.lookup v aliases,
            TypeAlias.arity alias == 0 ->
              go alias.body
        _ -> goABT t

    -- Recurse into the structure when the head isn't an alias application.
    -- 'Type.Effects' is handled specially: an ability-row alias used as an
    -- element of an Effects list splices its body's elements into the
    -- surrounding row, with set semantics applied afterwards (flatten +
    -- dedupe).
    goABT :: Type v a -> Type v a
    goABT t = case ABT.out t of
      ABT.Var v -> ABT.annotatedVar (ABT.annotation t) v
      ABT.Cycle body -> ABT.cycle' (ABT.annotation t) (goABT body)
      ABT.Abs v body -> ABT.abs' (ABT.annotation t) v (goABT body)
      ABT.Tm (Type.Effects es) ->
        let expandedElems = map go es
            flattened = concatMap spliceRowElement expandedElems
         in ABT.tm' (ABT.annotation t) (Type.Effects (dedupRow flattened))
      ABT.Tm f -> ABT.tm' (ABT.annotation t) (fmap go f)

spliceRowElement :: Type v a -> [Type v a]
spliceRowElement t = case ABT.out t of
  ABT.Tm (Type.Effects es) -> es
  _ -> [t]

dedupRow :: (Var v) => [Type v a] -> [Type v a]
dedupRow = nubBy (\x y -> stripAnns x == stripAnns y)
  where
    stripAnns = ABT.amap (const ())

-- | Topologically sort aliases by dependency. The body of each alias is
-- inspected for free variables that match other alias names; those are
-- treated as edges.
topoSort ::
  forall v a.
  (Var v) =>
  Map v (TypeAlias v a) ->
  Either (NormalizationError v) [v]
topoSort aliases = reverse <$> dfsAll Set.empty Set.empty (Map.keys aliases) []
  where
    edges :: Map v (Set v)
    edges =
      aliases & Map.map \alias ->
        ABT.freeVars alias.body
          & Set.intersection (Map.keysSet aliases)
          & flip Set.difference (Set.fromList alias.paramNames)

    dfsAll ::
      Set v -> -- done
      Set v -> -- in progress (path)
      [v] ->
      [v] -> -- accumulator (post-order, reversed at the end)
      Either (NormalizationError v) [v]
    dfsAll _ _ [] acc = pure acc
    dfsAll done inProgress (v : vs) acc
      | Set.member v done = dfsAll done inProgress vs acc
      | otherwise = do
          (done', acc') <- dfs done inProgress v acc
          dfsAll done' inProgress vs acc'

    dfs ::
      Set v ->
      Set v ->
      v ->
      [v] ->
      Either (NormalizationError v) (Set v, [v])
    dfs done inProgress v acc
      | Set.member v done = pure (done, acc)
      | Set.member v inProgress = Left (AliasCycle (Set.insert v inProgress))
      | otherwise = do
          let inProgress' = Set.insert v inProgress
              deps = Map.findWithDefault Set.empty v edges
          (done', acc') <- foldM (\(d, a) u -> dfs d inProgress' u a) (done, acc) (Set.toList deps)
          pure (Set.insert v done', v : acc')
