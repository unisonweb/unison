{-# LANGUAGE TemplateHaskell #-}
module Unison.Symbol where

import Data.Aeson.TH
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

-- NB: freshId is first field, so given a `Set Symbol`, the max element of
-- the set will also have the highest `freshId`.
data Symbol a = Symbol { freshId :: !Int, name :: Text, annotation :: a }

instance Functor Symbol where
  fmap f (Symbol id name a) = Symbol id name (f a)

-- Note: The `annotation` field not part of identity, is "just" metadata
instance Eq (Symbol a) where
  Symbol id1 name1 _ == Symbol id2 name2 _ = id1 == id2 && name1 == name2
instance Ord (Symbol a) where
  Symbol id1 name1 _ <= Symbol id2 name2 _ = id1 <= id2 || name1 <= name2

instance Show (Symbol a) where
  show s | freshId s == 0 = Text.unpack (name s)
  show s = Text.unpack (name s) ++ show (freshId s)

symbol :: Text -> Symbol ()
symbol n = Symbol 0 n ()

-- | Returns a fresh version of the given symbol, guaranteed to
-- be distinct from all symbols in the given set. Takes time
-- logarithmic in the size of the symbol set.
freshIn :: Set (Symbol a) -> Symbol a2 -> Symbol a2
freshIn vs s | Set.null vs = s -- already fresh!
freshIn vs s |
  Set.notMember (s { annotation = annotation (Set.findMin vs) }) vs = s -- already fresh!
freshIn vs s@(Symbol i n a) = case Set.elemAt (Set.size vs - 1) vs of
  Symbol i2 _ _ -> if i > i2 then s else Symbol (i2+1) n a

prefix :: Text -> Symbol ()
prefix name = symbol name

deriveJSON defaultOptions ''Symbol
