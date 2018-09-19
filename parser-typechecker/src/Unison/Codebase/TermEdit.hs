module Unison.Codebase.TermEdit where

import Unison.Reference (Reference)

data TermEdit = Replace Reference Typing | Deprecate

-- Replacements with the Same type can be automatically propagated.
-- Replacements with a Subtype can be automatically propagated but may result in dependents getting more general types, so requires re-inference.
-- Replacements of a Different type need to be manually propagated by the programmer.
data Typing = Same | Subtype | Different
