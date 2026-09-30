module Unison.TypeAlias.Expand
  ( inDependencyOrder,
    NormalizationError (..),
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.TypeAlias (TypeAlias (..))
import Unison.Var (Var)

-- | Errors produced while ordering a set of aliases.
data NormalizationError v
  = -- | Aliases form a dependency cycle. The set names the alias members
    -- involved in some cycle.
    AliasCycle (Set v)
  deriving stock (Show, Eq)

-- | Return the aliases in dependency order (each alias appears after the
-- aliases it references). Detects cycles. Bodies pass through unchanged;
-- callers that need use-site expansion go through the typechecker's
-- 'Unison.Typechecker.Context.whnfAlias'.
inDependencyOrder ::
  forall v a.
  (Var v) =>
  Map v (TypeAlias v a) ->
  Either (NormalizationError v) [(v, TypeAlias v a)]
inDependencyOrder aliases = do
  order <- reverse <$> dfsAll Set.empty Set.empty (Map.keys aliases) []
  pure [(v, aliases Map.! v) | v <- order]
  where
    edges :: Map v (Set v)
    edges =
      aliases & Map.map \alias ->
        ABT.freeVars alias.body
          & Set.intersection (Map.keysSet aliases)
          & flip Set.difference (Set.fromList alias.paramNames)

    dfsAll :: Set v -> Set v -> [v] -> [v] -> Either (NormalizationError v) [v]
    dfsAll _ _ [] acc = pure acc
    dfsAll done inProgress (v : vs) acc
      | Set.member v done = dfsAll done inProgress vs acc
      | otherwise = do
          (done', acc') <- dfs done inProgress v acc
          dfsAll done' inProgress vs acc'

    dfs :: Set v -> Set v -> v -> [v] -> Either (NormalizationError v) (Set v, [v])
    dfs done inProgress v acc
      | Set.member v done = pure (done, acc)
      | Set.member v inProgress = Left (AliasCycle (Set.insert v inProgress))
      | otherwise = do
          let inProgress' = Set.insert v inProgress
              deps = Map.findWithDefault Set.empty v edges
          (done', acc') <- foldM (\(d, a) u -> dfs d inProgress' u a) (done, acc) (Set.toList deps)
          pure (Set.insert v done', v : acc')
