{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Unison.Symbol where

import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics
import Unison.Var (Var(..))
import qualified Data.Set as Set
import qualified Unison.Var as Var

data Symbol = Symbol !Word64 Var.Type deriving (Generic)

instance Var Symbol where
  typed t = Symbol 0 t
  typeOf (Symbol _ t) = t
  retype t (Symbol id _) = Symbol id t
  freshId (Symbol id _) = id
  freshIn vs s | Set.null vs || Set.notMember s vs = s -- already fresh!
  freshIn vs s@(Symbol i n) = case Set.elemAt (Set.size vs - 1) vs of
    Symbol i2 _ -> if i > i2 then s else Symbol (i2+1) n
  freshenId id (Symbol _ n) = Symbol id n

instance Eq Symbol where
  Symbol id1 name1 == Symbol id2 name2 = id1 == id2 && name1 == name2
instance Ord Symbol where
  Symbol id1 name1 `compare` Symbol id2 name2 = (id1,name1) `compare` (id2,name2)
instance Show Symbol where
  show (Symbol 0 n) = show n
  show (Symbol id n) = show n ++ "-" ++ show id

symbol :: Text -> Symbol
symbol = Var.named
