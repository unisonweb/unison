module U.Codebase.Branch
  ( module X,
    nonEmptyChildren,
    childStats,
    isEmpty,
  )
where

import U.Codebase.Branch.Type as X
import qualified U.Codebase.Causal as Causal
import qualified U.Codebase.Sqlite.Operations as Ops
import Unison.Prelude
import Unison.Sqlite (Transaction)

isEmpty :: Branch m -> Transaction Bool
isEmpty b@(Branch {types, terms}) = do
  noChildren <- null <$> nonEmptyChildren b
  pure $ null types && null terms && noChildren

nonEmptyChildren :: Branch m -> Transaction (Map NameSegment (CausalBranch m))
nonEmptyChildren branch = do
  childrenWithStats <- childStats branch
  pure $
    childrenWithStats
      & mapMaybe
        ( \(cb, stats) ->
            if nonZeroStats stats
              then Just cb
              else Nothing
        )
  where
    nonZeroStats (NamespaceStats numContainedTerms numContainedTypes _numContainedPatches) =
      numContainedTerms + numContainedTypes > 0

childStats :: Branch m -> Transaction (Map NameSegment (CausalBranch m, NamespaceStats))
childStats Branch {children} =
  for children \cb -> do
    stats <- Ops.expectNamespaceStatsByHash (Causal.valueHash cb)
    pure (cb, stats)
