module U.Codebase.TermEdit where

import U.Codebase.Referent (Referent)

data TermEdit = Replace Referent Typing | Deprecate
  deriving (Eq, Ord, Show)

-- Replacements with the Same type can be automatically propagated.
-- Replacements with a Subtype can be automatically propagated but may result in dependents getting more general types, so requires re-inference.
-- Replacements of a Different type need to be manually propagated by the programmer.
data Typing = Same | Subtype | Different
  deriving (Eq, Ord, Show)
