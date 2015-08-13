{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Unison.Symbol where

import Data.Aeson.TH
import Data.Text (Text)
import Unison.Var (Var(..))
import Unison.View (View)
import qualified Unison.View as View
import qualified Data.Set as Set
import qualified Data.Text as Text

-- NB: freshId is first field, so given a `Set Symbol`, the max element of
-- the set will also have the highest `freshId`.
data Symbol a = Symbol !Int Text a

freshId :: Symbol a -> Int
freshId (Symbol id _ _) = id

annotation :: Symbol a -> a
annotation (Symbol _ _ a) = a

annotate :: a -> Symbol b -> Symbol a
annotate a (Symbol id name _) = Symbol id name a

instance View op => Var (Symbol op) where
  name (Symbol _ n _) = n
  named n = Symbol 0 n View.prefix
  clear (Symbol id n _) = Symbol id n View.prefix
  qualifiedName s = name s `Text.append` (Text.pack (show (freshId s)))
  freshIn vs s | Set.null vs = s -- already fresh!
  freshIn vs s | Set.notMember s vs = s -- already fresh!
  freshIn vs s@(Symbol i n a) = case Set.elemAt (Set.size vs - 1) vs of
    Symbol i2 _ _ -> if i > i2 then s else Symbol (i2+1) n a

instance Functor Symbol where
  fmap f (Symbol id name a) = Symbol id name (f a)

-- Note: The `annotation` field not part of identity, is "just" metadata
instance Eq (Symbol a) where
  Symbol id1 name1 _ == Symbol id2 name2 _ = id1 == id2 && name1 == name2
instance Ord (Symbol a) where
  Symbol id1 name1 _ `compare` Symbol id2 name2 _ = (id1,name1) `compare` (id2,name2)

instance View op => Show (Symbol op) where
  show s | freshId s == 0 = Text.unpack (name s)
  show s = Text.unpack (name s) ++ show (freshId s)

symbol :: View op => Text -> Symbol op
symbol n = Symbol 0 n View.prefix

prefix :: View op => Text -> Symbol op
prefix name = symbol name

deriveJSON defaultOptions ''Symbol
