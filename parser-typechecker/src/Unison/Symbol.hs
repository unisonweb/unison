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

data Symbol = Symbol !Word64 Var.Type deriving (Generic)

instance ABT.Var Symbol where
  freshIn vs s | Set.null vs || Set.notMember s vs = s -- already fresh!
  freshIn vs s@(Symbol i n) = case Set.elemAt (Set.size vs - 1) vs of
    Symbol i2 _ -> if i > i2 then s else Symbol (i2+1) n

instance Var Symbol where
  typed t = Symbol 0 t
  freshenId id (Symbol _ n) = Symbol id n
  name (Symbol id t) = case t of
    Var.User n -> n <> showid id
    Var.Inference Var.Ability -> "𝕖" <> showid id
    Var.Inference Var.Input -> "𝕒" <> showid id
    Var.Inference Var.Output -> "𝕣" <> showid id
    Var.Inference Var.Other -> "𝕩" <> showid id
    Var.Inference Var.PatternPureE -> "𝕞" <> showid id
    Var.Inference Var.PatternPureV -> "𝕧" <> showid id
    Var.Inference Var.PatternBindE -> "𝕞" <> showid id
    Var.Inference Var.PatternBindV -> "𝕧" <> showid id
    Var.Inference Var.TypeConstructor -> "𝕗" <> showid id
    Var.Inference Var.TypeConstructorArg -> "𝕦" <> showid id
    Var.UnnamedWatch k guid -> fromString k <> "." <> guid <> showid id
    where
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
