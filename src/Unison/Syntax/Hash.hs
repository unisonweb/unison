module Unison.Syntax.Hash where

import Data.ByteString as B
import Unison.Syntax.Term as E
import Unison.Syntax.Type as T

-- Hash which uniquely identifies a Unison type or term
newtype Hash = Hash B.ByteString

-- | Compute a `Hash` for the given `Term`
term :: (l -> Hash)
     -> (t -> Hash)
     -> E.Term l t
     -> Hash
term hashLit hashTyp e = error "todo: Hash.term"

-- | Compute a `Hash` for a mutually recursive list of terms
terms :: (l -> Hash)
      -> (t -> Hash)
      -> [E.Term l t]
      -> [Hash]
terms hashLit hashTyp es = error "todo: Hash.terms"

typ :: (l -> Hash)
    -> (c -> Hash)
    -> T.Type l c
    -> Hash
typ hashLit hashConstraint t = error "todo: Hash.typ"

-- | Compute a `Hash` for a mutually recursive list of types
types :: (l -> Hash)
    -> (c -> Hash)
    -> [T.Type l c]
    -> [Hash]
types hashLit hashConstraint ts = error "todo: Hash.types"
