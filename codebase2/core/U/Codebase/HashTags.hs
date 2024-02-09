module U.Codebase.HashTags where

import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Prelude

-- | Represents a hash of a type or term component
newtype ComponentHash = ComponentHash {unComponentHash :: Hash}
  deriving stock (Eq, Ord)

newtype BranchHash = BranchHash {unBranchHash :: Hash}
  deriving stock (Eq, Ord)

-- | Represents a hash of a causal containing values of the provided type.
newtype CausalHash = CausalHash {unCausalHash :: Hash}
  deriving stock (Eq, Ord)

newtype PatchHash = PatchHash {unPatchHash :: Hash}
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
  from = from @Hash @Text . unComponentHash

instance From BranchHash Text where
  from = from @Hash @Text . unBranchHash

instance From CausalHash Text where
  from = from @Hash @Text . unCausalHash

instance From PatchHash Text where
  from = from @Hash @Text . unPatchHash

instance From ComponentHash Hash

instance From BranchHash Hash

instance From CausalHash Hash

instance From PatchHash Hash

instance From Hash ComponentHash

instance From Hash BranchHash

instance From Hash CausalHash

instance From Hash PatchHash

instance From ComponentHash Hash32 where
  from = from @Hash @Hash32 . unComponentHash

instance From BranchHash Hash32 where
  from = from @Hash @Hash32 . unBranchHash

instance From CausalHash Hash32 where
  from = from @Hash @Hash32 . unCausalHash

instance From PatchHash Hash32 where
  from = from @Hash @Hash32 . unPatchHash

instance From Hash32 ComponentHash where
  from = ComponentHash . from @Hash32 @Hash

instance From Hash32 BranchHash where
  from = BranchHash . from @Hash32 @Hash

instance From Hash32 CausalHash where
  from = CausalHash . from @Hash32 @Hash

instance From Hash32 PatchHash where
  from = PatchHash . from @Hash32 @Hash
