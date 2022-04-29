{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Branch.Names
  ( findHistoricalHQs,
    findHistoricalRefs,
    findHistoricalRefs',
    namesDiff,
    toNames,
  )
where

import qualified Data.Set as Set
import Unison.Codebase.Branch
import qualified Unison.Codebase.Causal.FoldHistory as Causal
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import Unison.Names (Names (..))
import qualified Unison.Names as Names
import qualified Unison.NamesWithHistory as Names
import Unison.Prelude hiding (empty)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as R
import Prelude hiding (head, read, subtract)

toNames :: Branch0 m -> Names
toNames b =
  Names
    (R.swap . deepTerms $ b)
    (R.swap . deepTypes $ b)

-- This stops searching for a given HashQualified once it encounters
-- any term or type in any Branch0 that satisfies that HashQualified.
findHistoricalHQs ::
  Monad m =>
  Set (HashQualified Name) ->
  Branch m ->
  m (Set (HashQualified Name), Names)
findHistoricalHQs =
  findInHistory
    (\hq r n -> HQ.matchesNamedReferent n r hq)
    (\hq r n -> HQ.matchesNamedReference n r hq)

findHistoricalRefs ::
  Monad m =>
  Set LabeledDependency ->
  Branch m ->
  m (Set LabeledDependency, Names)
findHistoricalRefs =
  findInHistory
    (\query r _n -> LD.fold (const False) (== r) query)
    (\query r _n -> LD.fold (== r) (const False) query)

findHistoricalRefs' ::
  Monad m =>
  Set Reference ->
  Branch m ->
  m (Set Reference, Names)
findHistoricalRefs' =
  findInHistory
    (\queryRef r _n -> r == Referent.Ref queryRef)
    (\queryRef r _n -> r == queryRef)

findInHistory ::
  forall m q.
  (Monad m, Ord q) =>
  (q -> Referent -> Name -> Bool) ->
  (q -> Reference -> Name -> Bool) ->
  Set q ->
  Branch m ->
  m (Set q, Names)
findInHistory termMatches typeMatches queries b =
  (Causal.foldHistoryUntil f (queries, mempty) . _history) b <&> \case
    -- could do something more sophisticated here later to report that some SH
    -- couldn't be found anywhere in the history.  but for now, I assume that
    -- the normal thing will happen when it doesn't show up in the namespace.
    Causal.Satisfied (_, names) -> (mempty, names)
    Causal.Unsatisfied (missing, names) -> (missing, names)
  where
    -- in order to not favor terms over types, we iterate through the ShortHashes,
    -- for each `remainingQueries`, if we find a matching Referent or Reference,
    -- we remove `q` from the accumulated `remainingQueries`, and add the Ref* to
    -- the accumulated `names0`.
    f acc@(remainingQueries, _) b0 = (acc', null remainingQueries')
      where
        acc'@(remainingQueries', _) = foldl' findQ acc remainingQueries
        findQ :: (Set q, Names) -> q -> (Set q, Names)
        findQ acc sh =
          foldl'
            (doType sh)
            ( foldl'
                (doTerm sh)
                acc
                (R.toList $ deepTerms b0)
            )
            (R.toList $ deepTypes b0)
        doTerm q acc@(remainingSHs, names0) (r, n) =
          if termMatches q r n
            then (Set.delete q remainingSHs, Names.addTerm n r names0)
            else acc
        doType q acc@(remainingSHs, names0) (r, n) =
          if typeMatches q r n
            then (Set.delete q remainingSHs, Names.addType n r names0)
            else acc

namesDiff :: Branch m -> Branch m -> Names.Diff
namesDiff b1 b2 = Names.diff (toNames (head b1)) (toNames (head b2))
