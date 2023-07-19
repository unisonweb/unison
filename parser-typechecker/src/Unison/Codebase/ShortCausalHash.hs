module Unison.Codebase.ShortCausalHash
  ( toString,
    toHash,
    fromHash,
    fromText,
    ShortCausalHash (..),
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Util.Base32Hex qualified as Base32Hex
import Unison.Hash qualified as Hash
import Unison.Prelude

-- | Causal Hash Prefix
newtype ShortCausalHash = ShortCausalHash {toText :: Text} -- base32hex characters
  deriving stock (Eq, Ord, Generic)

toString :: ShortCausalHash -> String
toString = Text.unpack . toText

toHash :: (Coercible Hash.Hash h) => ShortCausalHash -> Maybe h
toHash = fmap coerce . Hash.fromBase32HexText . toText

fromHash :: (Coercible h Hash.Hash) => Int -> h -> ShortCausalHash
fromHash len =
  ShortCausalHash . Text.take len . Hash.toBase32HexText . coerce

-- abc -> SCH abc
-- #abc -> SCH abc
fromText :: Text -> Maybe ShortCausalHash
fromText (Text.dropWhile (== '#') -> t)
  | Text.all (`Set.member` Base32Hex.validChars) t =
      Just $
        ShortCausalHash t
fromText _ = Nothing

instance Show ShortCausalHash where
  show (ShortCausalHash h) = '#' : Text.unpack h
