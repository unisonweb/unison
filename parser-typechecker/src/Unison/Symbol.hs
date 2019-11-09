{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Unison.Symbol where

import Unison.Prelude

import Data.Text (pack)
import Unison.Var (Var(..))
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Var as Var

data Symbol = Symbol !Word64 Text deriving (Generic)

instance ABT.Var Symbol where
  freshIn vs s | Set.null vs || Set.notMember s vs = s -- already fresh!
  freshIn vs s@(Symbol i n) = case Set.elemAt (Set.size vs - 1) vs of
    Symbol i2 _ -> if i > i2 then s else Symbol (i2+1) n

instance Var Symbol where
  named n = Symbol 0 n
  reset (Symbol _ n) = Symbol 0 n
  name (Symbol id n) = n <> showid id where
    showid 0 = ""
    showid n = pack (show n)

instance Eq Symbol where
  Symbol id1 name1 == Symbol id2 name2 = id1 == id2 && name1 == name2
instance Ord Symbol where
  Symbol id1 name1 `compare` Symbol id2 name2 = (id1,name1) `compare` (id2,name2)
instance Show Symbol where
  show (Symbol 0 n) = show n
  show (Symbol id n) = show n ++ "-" ++ show id

symbol :: Text -> Symbol
symbol = Var.named
