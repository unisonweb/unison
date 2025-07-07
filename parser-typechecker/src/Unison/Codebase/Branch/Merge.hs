module Unison.Codebase.Branch.Merge
  ( MergeMode (..),
    merge'',
  )
where

import Data.Map qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import Unison.Codebase.Branch
  ( Branch (..),
    Branch0,
    branch0,
    cons,
    discardHistory0,
    empty0,
    head,
    isEmpty,
    isEmpty0,
  )
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.BranchDiff (BranchDiff (BranchDiff))
import Unison.Codebase.Branch.BranchDiff qualified as BDiff
import Unison.Codebase.Causal qualified as Causal
import Unison.Prelude hiding (empty)
import Unison.Util.Map (unionWithM)
import Unison.Util.Star2 qualified as Star2
import Prelude hiding (head, read, subtract)

data MergeMode = RegularMerge | SquashMerge deriving (Eq, Ord, Show)

merge'' ::
  forall m.
  (Monad m) =>
  (Branch m -> Branch m -> m (Maybe (Branch m))) -> -- lca calculator
  MergeMode ->
  Branch m ->
  Branch m ->
  m (Branch m)
merge'' _ _ b1 b2 | isEmpty b1 = pure b2
merge'' _ mode b1 b2 | isEmpty b2 = case mode of
  RegularMerge -> pure b1
  SquashMerge -> pure $ cons (discardHistory0 (head b1)) b2
merge'' lca mode (Branch x) (Branch y) =
  Branch <$> case mode of
    RegularMerge -> Causal.threeWayMerge' lca' combine x y
    SquashMerge -> Causal.squashMerge' lca' (pure . discardHistory0) combine x y
  where
    lca' c1 c2 = fmap _history <$> lca (Branch c1) (Branch c2)
    combine :: Maybe (Branch0 m) -> Branch0 m -> Branch0 m -> m (Branch0 m)
    combine Nothing l r = merge0 lca mode l r
    combine (Just ca) l r = do
      let dl = BDiff.diff0 ca l
      let dr = BDiff.diff0 ca r
      let head0 = apply ca (dl <> dr)
      children <-
        Map.mergeA
          (Map.traverseMaybeMissing $ combineMissing ca)
          (Map.traverseMaybeMissing $ combineMissing ca)
          (Map.zipWithAMatched $ const (merge'' lca mode))
          (l ^. Branch.children_)
          (r ^. Branch.children_)
      pure $ branch0 (head0 ^. Branch.terms_) (head0 ^. Branch.types_) children (head0 ^. Branch.edits_)

    combineMissing ca k cur =
      case Map.lookup k (ca ^. Branch.children_) of
        Nothing -> pure $ Just cur
        Just old -> do
          nw <- merge'' lca mode (cons empty0 old) cur
          if isEmpty0 $ head nw
            then pure Nothing
            else pure $ Just nw

    apply :: Branch0 m -> BranchDiff -> Branch0 m
    apply b0 (BranchDiff addedTerms removedTerms addedTypes removedTypes) = do
      branch0
        (Star2.difference (b0 ^. Branch.terms_) removedTerms <> addedTerms)
        (Star2.difference (b0 ^. Branch.types_) removedTypes <> addedTypes)
        (b0 ^. Branch.children_)
        Map.empty

merge0 ::
  forall m.
  (Monad m) =>
  (Branch m -> Branch m -> m (Maybe (Branch m))) ->
  MergeMode ->
  Branch0 m ->
  Branch0 m ->
  m (Branch0 m)
merge0 lca mode b1 b2 = do
  c3 <- unionWithM (merge'' lca mode) (b1 ^. Branch.children_) (b2 ^. Branch.children_)
  pure $
    branch0
      (b1 ^. Branch.terms_ <> b2 ^. Branch.terms_)
      (b1 ^. Branch.types_ <> b2 ^. Branch.types_)
      c3
      Map.empty
