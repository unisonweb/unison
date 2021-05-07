module U.Codebase.TypeEdit where

import U.Codebase.Reference (Reference)
import U.Util.Hashable (Hashable)
import qualified U.Util.Hashable as H

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)

instance Hashable TypeEdit where
  tokens (Replace r) = H.Tag 0 : H.tokens r
  tokens Deprecate   = [H.Tag 1]
