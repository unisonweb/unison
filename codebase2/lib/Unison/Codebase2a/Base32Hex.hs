{-# LANGUAGE DerivingVia #-}

module Unison.Codebase2a.Base32Hex (
  Base32Hex, fromHash, toHash
) where

import Unison.Hash (Hash)
import qualified Unison.Hash as Hash

import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)

import Data.Text (Text)

-- let's put this in a new module and leave the constructor private
-- so that conversions can be trusted / safe
newtype Base32Hex = Base32Hex Text
  deriving (Eq, Ord, Show, ToField, FromField) via Text

fromHash :: Hash -> Base32Hex
fromHash = Base32Hex . Hash.base32Hex

toHash :: Base32Hex -> Hash
toHash (Base32Hex t) = Hash.unsafeFromBase32Hex t

