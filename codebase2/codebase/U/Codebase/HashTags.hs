{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module U.Codebase.HashTags where

import U.Util.Hash (Hash)

newtype BranchHash = BranchHash { unBranchHash :: Hash } deriving (Eq, Ord)

newtype CausalHash = CausalHash { unCausalHash :: Hash } deriving (Eq, Ord)

newtype EditHash = EditHash { unEditHash :: Hash } deriving (Eq, Ord)

newtype PatchHash = PatchHash { unPatchHash :: Hash } deriving (Eq, Ord)

newtype DefnHash = DefnHash { unDefnHash :: Hash } deriving (Eq, Ord)

instance Show BranchHash where
  show h = "BranchHash (" ++ show (unBranchHash h) ++ ")"

instance Show CausalHash where
  show h = "CausalHash (" ++ show (unCausalHash h) ++ ")"

instance Show PatchHash where
  show h = "PatchHash (" ++ show (unPatchHash h) ++ ")"

instance Show EditHash where
  show h = "EditHash (" ++ show (unEditHash h) ++ ")"

instance Show DefnHash where
  show h = "DefnHash (" ++ show (unDefnHash h) ++ ")"
