{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Codebase.TermEdit where

import Unison.Reference (Reference)

data TermEdit = Replace Reference Typing | Deprecate
  deriving (Eq, Ord, Show)

references :: TermEdit -> [Reference]
references (Replace r _) = [r]
references Deprecate = []

-- Replacements with the Same type can be automatically propagated.
-- Replacements with a Subtype can be automatically propagated but may result in dependents getting more general types, so requires re-inference.
-- Replacements of a Different type need to be manually propagated by the programmer.
data Typing = Same | Subtype | Different
  deriving (Eq, Ord, Show)

toReference :: TermEdit -> Maybe Reference
toReference (Replace r _) = Just r
toReference Deprecate     = Nothing

isTypePreserving :: TermEdit -> Bool
isTypePreserving e = case e of
  Replace _ Same -> True
  Replace _ Subtype -> True
  _ -> False

isSame :: TermEdit -> Bool
isSame e = case e of
  Replace _ Same -> True
  _              -> False
