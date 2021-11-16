module Unison.Codebase.Branch.Squash where
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal

-- | Given a hash which exists in the provided branch's causal spine,
-- squash all changes from that point into a single causal Cons.
--
-- This happens recursively, using the corresponding hash for each child branch according
-- to the starting point, for all children.
squashFrom :: Causal.RawHash Branch.Raw -> Branch m -> Branch m
squashFrom startHash branch = _
