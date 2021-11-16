module Unison.Codebase.Branch.Squash
  ( squashOnto
  ) where

import Control.Lens ((.~), (^.))
import qualified Data.Map as Map
import Data.These (These (..))
import Data.Zip as Zip
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.NameSegment (NameSegment)
import Unison.Prelude

-- | Applies any differences from baseBranch -> headBranch as a single Causal Cons on top of
-- baseBranch's history.
-- We do the same recursively for child branches.
--
-- The two branches don't need to share a common ancestor.
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
      -- If we have a matching child in both base and head, squash the child head onto the
      -- child base recursively.
      (These base head) -> Just $ squashOnto base head
      -- This child has been deleted, remove it in the result
      (This _base) -> Nothing
      -- This child didn't exist in the base, flatten the history completely down to a step.
      (That head) -> Just $ Branch.discardHistory head
    squashedChildren :: Map NameSegment (Branch m)
    squashedChildren =
      Map.mapMaybe id $
        Zip.alignWith
          squashChild
          (Branch.head baseBranch ^. Branch.children)
          (Branch.head headBranch ^. Branch.children)
