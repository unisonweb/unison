module Unison.Codebase.TermEdit where

import Unison.Hashable (Hashable)
import qualified Unison.Hashable as H
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Reference (Reference)
import qualified Unison.Typechecker as Typechecker
import Unison.Type (Type)
import Unison.Var (Var)

data TermEdit = Replace Referent Typing | Deprecate
  deriving (Eq, Ord, Show)

dependencies :: TermEdit -> [Reference]
dependencies Deprecate = []
dependencies (Replace r _) = [Referent.toReference r]

referents :: TermEdit -> [Referent]
referents (Replace r _) = [r]
referents Deprecate = []

-- Replacements with the Same type can be automatically propagated.
-- Replacements with a Subtype can be automatically propagated but may result in dependents getting more general types, so requires re-inference.
-- Replacements of a Different type need to be manually propagated by the programmer.
data Typing = Same | Subtype | Different
  deriving (Eq, Ord, Show)

instance Hashable Typing where
  tokens Same = [H.Tag 0]
  tokens Subtype = [H.Tag 1]
  tokens Different = [H.Tag 2]

instance Hashable TermEdit where
  tokens (Replace r t) = [H.Tag 0] ++ H.tokens r ++ H.tokens t
  tokens Deprecate = [H.Tag 1]

toReferent :: TermEdit -> Maybe Referent
toReferent (Replace r _) = Just r
toReferent Deprecate     = Nothing

isTypePreserving :: TermEdit -> Bool
isTypePreserving e = case e of
  Replace _ Same -> True
  Replace _ Subtype -> True
  _ -> False

isSame :: TermEdit -> Bool
isSame e = case e of
  Replace _ Same -> True
  _              -> False

typing :: Var v => Type v loc -> Type v loc -> Typing
typing newType oldType | Typechecker.isEqual newType oldType = Same
                       | Typechecker.isSubtype newType oldType = Subtype
                       | otherwise = Different

