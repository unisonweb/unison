module Unison.Codebase.ShortBranchHash
  ( prefixName,
    toString,
    toText,
    toHash,
    fromHash,
    fullFromHash,
    fromText,
    ShortBranchHash (..),
    Unison.Codebase.ShortBranchHash.base32HexLength,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Hash as Hash
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Prelude

newtype ShortBranchHash = ShortBranchHash
  { -- | Gets the raw base32 hex of the ShortBranchHash, e.g. "hkrqt3tm05"
    toBase32Hex :: Text
  }
  deriving stock (Eq, Ord, Generic)

-- | Get the length of the underlying base32Hex
base32HexLength :: ShortBranchHash -> Int
base32HexLength (ShortBranchHash h) = Text.length h

-- | Converts a short branch hash into text, including a '#' prefix.
-- E.g. "#hkrqt3tm05"
toText :: ShortBranchHash -> Text
toText (ShortBranchHash h) = "#" <> h

prefixName :: ShortBranchHash -> Name -> Name
prefixName sbh name = Name.cons (NameSegment $ toText sbh) name

-- | Converts a short branch hash into a string, including a '#' prefix.
-- E.g. "#hkrqt3tm05"
toString :: ShortBranchHash -> String
toString = Text.unpack . toText

toHash :: Coercible Hash.Hash h => ShortBranchHash -> Maybe h
toHash = fmap coerce . Hash.fromBase32Hex . toBase32Hex

fromHash :: Coercible h Hash.Hash => Int -> h -> ShortBranchHash
fromHash len =
  ShortBranchHash . Text.take len . Hash.base32Hex . coerce

fullFromHash :: Coercible h Hash.Hash => h -> ShortBranchHash
fullFromHash = ShortBranchHash . Hash.base32Hex . coerce

-- abc -> SBH abc
-- #abc -> SBH abc
fromText :: Text -> Maybe ShortBranchHash
fromText (Text.dropWhile (== '#') -> t)
  | Text.all (`Set.member` Hash.validBase32HexChars) t =
    Just $
      ShortBranchHash t
fromText _ = Nothing

-- | Converts a short branch hash into a string, including a '#' prefix.
-- E.g. @#hkrqt3tm05@
instance Show ShortBranchHash where
  show = toString
