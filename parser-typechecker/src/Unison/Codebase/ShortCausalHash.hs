module Unison.Codebase.ShortCausalHash
  ( toString,
    toHash,
    fromHash,
    fromText,
    ShortCausalHash (..),
  )
where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Hash as Hash
import Unison.Prelude

-- | Causal Hash Prefix
newtype ShortCausalHash = ShortCausalHash {toText :: Text} -- base32hex characters
  deriving stock (Eq, Ord, Generic)

toString :: ShortCausalHash -> String
toString = Text.unpack . toText

toHash :: Coercible Hash.Hash h => ShortCausalHash -> Maybe h
toHash = fmap coerce . Hash.fromBase32Hex . toText

fromHash :: Coercible h Hash.Hash => Int -> h -> ShortCausalHash
fromHash len =
  ShortCausalHash . Text.take len . Hash.base32Hex . coerce

-- abc -> SBH abc
-- #abc -> SBH abc
fromText :: Text -> Maybe ShortCausalHash
fromText (Text.dropWhile (== '#') -> t)
  | Text.all (`Set.member` Hash.validBase32HexChars) t =
      Just $
        ShortCausalHash t
fromText _ = Nothing

instance Show ShortCausalHash where
  show (ShortCausalHash h) = '#' : Text.unpack h
