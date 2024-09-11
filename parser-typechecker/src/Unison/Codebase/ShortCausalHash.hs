module Unison.Codebase.ShortCausalHash
  ( fromHash,
    fromFullHash,
    fromText,
    ShortCausalHash (..),
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.HashTags (CausalHash (unCausalHash))
import U.Util.Base32Hex qualified as Base32Hex
import Unison.Hash qualified as Hash
import Unison.Prelude

-- | Causal Hash Prefix
newtype ShortCausalHash = ShortCausalHash {toText :: Text} -- base32hex characters
  deriving stock (Eq, Ord, Generic)

fromHash :: Int -> CausalHash -> ShortCausalHash
fromHash len =
  ShortCausalHash . Text.take len . Hash.toBase32HexText . unCausalHash

-- | This allows a full hash to be preserved as a `ShortCausalHash`.
--
--  `ShortCausalHash` is used for input when we expect a user to enter a hash on the command line, so they aren’t
--   required to enter the full hash. However, these inputs may also come from an internal source, and in such cases,
--   there is no reason to truncate the hash.
fromFullHash :: (Coercible h Hash.Hash) => h -> ShortCausalHash
fromFullHash = ShortCausalHash . Hash.toBase32HexText . coerce

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

instance From ShortCausalHash Text where
  from = toText
