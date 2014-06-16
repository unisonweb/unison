module Unison.Syntax.Hash (
  Hash,
  append, finalize, hashDouble, hashText,
  zero, one, two, three) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Text as T

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

