{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Unison.Symbol where

import Data.Text (Text)
import GHC.Generics
import Unison.Var (Var(..))
import qualified Data.Set as Set
import qualified Data.Text as Text

data Symbol = Symbol !Word Text deriving (Generic, Show)

freshId :: Symbol -> Word
freshId (Symbol id _) = id

instance Var Symbol where
  rename n (Symbol id _) = Symbol id n
  name (Symbol _ n) = n
  named n = Symbol 0 n
  clear (Symbol id n) = Symbol id n
  qualifiedName s = name s `Text.append` (Text.pack (show (freshId s)))
  freshIn vs s | Set.null vs || Set.notMember s vs = s -- already fresh!
  freshIn vs s@(Symbol i n) = case Set.elemAt (Set.size vs - 1) vs of
    Symbol i2 _ -> if i > i2 then s else Symbol (i2+1) n
  freshenId id (Symbol _ n) = Symbol id n

instance Eq Symbol where
  Symbol id1 name1 == Symbol id2 name2 = id1 == id2 && name1 == name2
instance Ord Symbol where
  Symbol id1 name1 `compare` Symbol id2 name2 = (id1,name1) `compare` (id2,name2)

symbol :: Text -> Symbol
symbol n = Symbol 0 n
