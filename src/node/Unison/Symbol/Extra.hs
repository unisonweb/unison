{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Symbol.Extra where

import Unison.Symbol
import Data.Bytes.Serial (Serial(..))
import Data.Bytes.VarInt

instance Serial Fixity where
  serialize = serialize . VarInt . fromEnum
  deserialize = toEnum . unVarInt <$> deserialize

instance Serial Symbol where
  serialize (Symbol i n f p) =
    serialize (VarInt i) *> serialize n *> serialize f *> serialize (VarInt p)
  deserialize =
    Symbol <$> (unVarInt <$> deserialize)
           <*> deserialize
           <*> deserialize
           <*> (unVarInt <$> deserialize)

