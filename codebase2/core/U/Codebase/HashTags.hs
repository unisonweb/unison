module U.Codebase.HashTags where

import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Prelude

-- | Represents a hash of a type or term component
newtype ComponentHash = ComponentHash {unComponentHash :: Hash32}
  deriving stock (Eq, Ord)

newtype BranchHash = BranchHash {unBranchHash :: Hash32}
  deriving (Eq, Ord)

-- | Represents a hash of a causal containing values of the provided type.
newtype CausalHash = CausalHash {unCausalHash :: Hash32}
  deriving stock (Eq, Ord)

newtype PatchHash = PatchHash {unPatchHash :: Hash32}
  deriving stock (Eq, Ord)

instance Show ComponentHash where
  show h = "ComponentHash (" ++ show (unComponentHash h) ++ ")"

instance Show BranchHash where
  show h = "BranchHash (" ++ show (unBranchHash h) ++ ")"

instance Show CausalHash where
  show h = "CausalHash (" ++ show (unCausalHash h) ++ ")"

instance Show PatchHash where
  show h = "PatchHash (" ++ show (unPatchHash h) ++ ")"

instance From ComponentHash Text where
  from = from @Hash32 @Text . unComponentHash

instance From BranchHash Text where
  from = from @Hash32 @Text . unBranchHash

instance From CausalHash Text where
  from = from @Hash32 @Text . unCausalHash

instance From PatchHash Text where
  from = from @Hash32 @Text . unPatchHash

instance From ComponentHash Hash32

instance From BranchHash Hash32

instance From CausalHash Hash32

instance From PatchHash Hash32

instance From Hash32 ComponentHash

instance From Hash32 BranchHash

instance From Hash32 CausalHash

instance From Hash32 PatchHash

instance From ComponentHash Hash where
  from = from @Hash32 @Hash . unComponentHash

instance From BranchHash Hash where
  from = from @Hash32 @Hash . unBranchHash

instance From CausalHash Hash where
  from = from @Hash32 @Hash . unCausalHash

instance From PatchHash Hash where
  from = from @Hash32 @Hash . unPatchHash

instance From Hash ComponentHash where
  from = ComponentHash . from @Hash @Hash32

instance From Hash BranchHash where
  from = BranchHash . from @Hash @Hash32

instance From Hash CausalHash where
  from = CausalHash . from @Hash @Hash32

instance From Hash PatchHash where
  from = PatchHash . from @Hash @Hash32
