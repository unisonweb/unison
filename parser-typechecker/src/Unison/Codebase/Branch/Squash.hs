module Unison.Codebase.Branch.Squash where

import Control.Lens ((.~), (^.))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.These (These (..))
import Data.Zip as Zip
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.NameSegment (NameSegment)
import Unison.Prelude

-- | Given a base branch containing the desired branch history and a branch containing the
-- desired final state, this method returns a new branch
--
-- squash all changes from that point onwards into a single causal Cons.
--
-- This happens recursively, all changes on children will be squashed against their
-- corresponding base causals.
squashOnto ::
  forall m.
  Monad m =>
  Branch m ->
  Branch m ->
  Branch m
squashOnto baseBranch headBranch =
  Branch.Branch $
    Causal.consDistinct
      (Branch.head headBranch & Branch.children .~ squashedChildren)
      (Branch._history baseBranch)
  where
    squashChild :: These (Branch m) (Branch m) -> Maybe (Branch m)
    squashChild = \case
      -- If we have a matching child in both base and head, squash recursively.
      (These base head) -> Just $ squashOnto base head
      -- This child has been deleted in the new head, leave it deleted.
      (This _base) -> Nothing
      -- This child didn't exist in the base, flatten the history completely down to a single cons.
      (That head) -> Just $ Branch.discardHistory head
    squashedChildren :: Map NameSegment (Branch m)
    squashedChildren =
      Map.mapMaybe id $
        Zip.alignWith
          squashChild
          (Branch.head baseBranch ^. Branch.children)
          (Branch.head headBranch ^. Branch.children)
