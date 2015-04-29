{-# LANGUAGE TemplateHaskell #-}
module Unison.Symbol where

import Control.Applicative
import Data.Aeson.TH
import Data.Text (Text)
import Data.Set (Set)
import Data.Bytes.Serial (Serial(..))
import Data.Bytes.VarInt
import qualified Data.Set as Set
import qualified Data.Text as Text

data Fixity = InfixL | InfixR | Infix | Prefix deriving (Eq,Ord,Show,Enum)

-- NB: freshId is first field, so given a `Set Symbol`, the max element of
-- the set will also have the highest `freshId`.
data Symbol = Symbol { freshId :: !Int, name :: Text, fixity :: !Fixity, precedence :: !Int } deriving (Eq,Ord)

instance Show Symbol where
  show s | freshId s == 0 = Text.unpack (name s)
  show s = Text.unpack (name s) ++ show (freshId s)

symbol :: Text -> Fixity -> Int -> Symbol
symbol n f p = Symbol 0 n f p

-- | Returns a fresh version of the given symbol, guaranteed to
-- be distinct from all symbols in the given set. Takes time
-- logarithmic in the size of the symbol set.
freshIn :: Set Symbol -> Symbol -> Symbol
freshIn vs s | Set.notMember s vs = s -- already fresh!
freshIn vs s@(Symbol i n f p) = case Set.elemAt (Set.size vs - 1) vs of
  Symbol i2 _ _ _ -> if i > i2 then s else Symbol (i2+1) n f p

prefix :: Text -> Symbol
prefix name = symbol name Prefix 9

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

deriveJSON defaultOptions ''Fixity
deriveJSON defaultOptions ''Symbol
