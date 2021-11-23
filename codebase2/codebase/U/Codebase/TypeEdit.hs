{- ORMOLU_DISABLE -}
module U.Codebase.TypeEdit where

import U.Codebase.Reference (Reference)

data TypeEdit = Replace Reference | Deprecate
  deriving (Eq, Ord, Show)
