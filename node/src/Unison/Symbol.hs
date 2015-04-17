{-# LANGUAGE TemplateHaskell #-}
module Unison.Symbol where

import Control.Applicative
import Data.Aeson.TH
import Data.Text (Text)
import Data.Bytes.Serial (Serial(..))
import Data.Bytes.VarInt

data Fixity = InfixL | InfixR | Infix | Prefix deriving (Eq,Ord,Show,Enum)
data Symbol = Symbol { name :: Text, freshId :: !Int, fixity :: !Fixity, precedence :: !Int } deriving (Eq,Ord,Show)

symbol :: Text -> Fixity -> Int -> Symbol
symbol n f p = Symbol n 0 f p

freshen :: Symbol -> Symbol
freshen (Symbol n i f p) = Symbol n (i+1) f p

prefix :: Text -> Symbol
prefix name = symbol name Prefix 9

instance Serial Fixity where
  serialize = serialize . VarInt . fromEnum
  deserialize = toEnum . unVarInt <$> deserialize

instance Serial Symbol where
  serialize (Symbol n i f p) =
    serialize n *> serialize (VarInt i) *> serialize f *> serialize (VarInt p)
  deserialize =
    Symbol <$> deserialize
           <*> (unVarInt <$> deserialize)
           <*> deserialize
           <*> (unVarInt <$> deserialize)

deriveJSON defaultOptions ''Fixity
deriveJSON defaultOptions ''Symbol
