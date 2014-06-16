module Unison.Syntax.Hash (
  Hash,
  append, finalize, hashDouble, hashText,
  zero, one, two, three,
  term, terms, typ, types) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Unison.Syntax.Term as E
import Unison.Syntax.Type as T

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash B.ByteString deriving (Eq,Show,Ord)

append :: Hash -> Hash -> Hash
append (Hash a) (Hash b) = Hash (B.append a b)

hashDouble :: Double -> Hash
hashDouble = error "todo: hashDouble"

hashText :: T.Text -> Hash
hashText = error "todo: hashText"

finalize :: Hash -> B.ByteString
finalize (Hash bs) = bs

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

word8 :: Word8 -> Hash
word8 byte = Hash (B.singleton byte)

zero :: Hash
zero = word8 0

one :: Hash
one = word8 1

two :: Hash
two = word8 2

three :: Hash
three = word8 3

