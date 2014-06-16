module Unison.Language.Term (Term, hash, hashes) where

import qualified Unison.Language.Type as T
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Term as ST
import qualified Unison.Language.Term.Literal as L

-- | A term in the Unison language
type Term = ST.Term L.Literal T.Type

hash :: Term -> H.Hash
hash e = H.term hashLit T.hash e

hashes :: [Term] -> [H.Hash]
hashes e = H.terms hashLit T.hash e

hashLit :: L.Literal -> H.Hash
hashLit (L.Hash h) = h
hashLit (L.Number n) = H.zero `H.append` H.hashDouble n
hashLit (L.String s) = H.one `H.append` H.hashText s
hashLit (L.Vector vec) = H.two `H.append` go vec where
  go vec = error "todo: hashLit vector"
