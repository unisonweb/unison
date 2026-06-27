module Unison.OpaqueDeclaration.Expand
  ( inDependencyOrder,
    NormalizationError (..),
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.OpaqueDeclaration (OpaqueDeclaration (..))
import Unison.Prelude
import Unison.Var (Var)

-- | Errors produced while ordering a set of opaque-type declarations.
data NormalizationError v
  = -- | Opaque types form a dependency cycle (including a single opaque type
    -- whose RHS mentions itself). The set names the opaques involved.
    OpaqueCycle (Set v)
  deriving stock (Show, Eq)

-- | Return the opaque declarations in dependency order (each opaque appears
-- after the opaques it references in its RHS). Detects cycles, including the
-- self-reference case @opaque type T = T@ which is invalid by design (RHS may
-- not mention the LHS).
--
-- Bodies are ignored for ordering purposes — only the RHS contributes to the
-- type-level dependency graph, since body items reference the opaque type's
-- own name freely and are processed separately.
inDependencyOrder ::
  forall v a.
  (Var v) =>
  Map v (OpaqueDeclaration v a) ->
  Either (NormalizationError v) [(v, OpaqueDeclaration v a)]
inDependencyOrder opaques = do
  order <- reverse <$> dfsAll Set.empty Set.empty (Map.keys opaques) []
  pure [(v, opaques Map.! v) | v <- order]
  where
    edges :: Map v (Set v)
    edges =
      opaques & Map.map \od ->
        ABT.freeVars od.rhs
          & Set.intersection (Map.keysSet opaques)
          & flip Set.difference (Set.fromList od.paramNames)

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
      | Set.member v inProgress = Left (OpaqueCycle (Set.insert v inProgress))
      | otherwise = do
          let inProgress' = Set.insert v inProgress
              deps = Map.findWithDefault Set.empty v edges
          (done', acc') <- foldM (\(d, a) u -> dfs d inProgress' u a) (done, acc) (Set.toList deps)
          pure (Set.insert v done', v : acc')
