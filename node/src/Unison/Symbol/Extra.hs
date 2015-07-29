{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Symbol.Extra where

import Unison.Symbol
import Data.Bytes.Serial (Serial(..))
import Data.Bytes.VarInt

instance Serial a => Serial (Symbol a) where
  serialize (Symbol i n a) =
    serialize (VarInt i) *> serialize n *> serialize a
  deserialize =
    Symbol <$> (unVarInt <$> deserialize) <*> deserialize <*> deserialize

