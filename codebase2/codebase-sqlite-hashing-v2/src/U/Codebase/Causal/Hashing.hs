module U.Codebase.Causal.Hashing where

import Data.Set
import Data.Set qualified as Set
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as Hashing

hashCausal :: BranchHash -> Set CausalHash -> CausalHash
hashCausal branchHash ancestors =
  CausalHash . Hash32.fromHash . Hashing.contentHash $
    Hashing.Causal
      { Hashing.branchHash = Hash32.toHash $ unBranchHash branchHash,
        Hashing.parents = Set.map (Hash32.toHash . unCausalHash) ancestors
      }
