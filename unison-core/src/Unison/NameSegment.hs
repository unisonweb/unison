{-# LANGUAGE OverloadedStrings #-}

module Unison.NameSegment where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical, compareAlphabetical)

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment {toText :: Text} deriving (Eq, Ord)

instance Alphabetical NameSegment where
  compareAlphabetical n1 n2 = compareAlphabetical (toText n1) (toText n2)

-- Split text into segments. A smarter version of `Text.splitOn` that handles
-- the name `.` properly.
segments' :: Text -> [Text]
segments' n = go split
  where
    split = Text.splitOn "." n
    go [] = []
    go ("" : "" : z) = "." : go z
    go ("" : z) = go z
    go (x : y) = x : go y

-- Same as reverse . segments', but produces the output as a
-- lazy list, suitable for suffix-based ordering purposes or
-- building suffix tries. Examples:
--
--   reverseSegments' "foo.bar.baz"  => ["baz","bar","foo"]
--   reverseSegments' ".foo.bar.baz" => ["baz","bar","foo"]
--   reverseSegments' ".."           => ["."]
--   reverseSegments' "Nat.++"       => ["++","Nat"]
--   reverseSegments' "Nat.++.zoo"   => ["zoo","++","Nat"]
reverseSegments' :: Text -> [Text]
reverseSegments' = go
  where
    go "" = []
    go t =
      let seg0 = Text.takeWhileEnd (/= '.') t
          seg = if Text.null seg0 then Text.takeEnd 1 t else seg0
          rem = Text.dropEnd (Text.length seg + 1) t
       in seg : go rem

isEmpty :: NameSegment -> Bool
isEmpty ns = toText ns == mempty

isPrefixOf :: NameSegment -> NameSegment -> Bool
isPrefixOf n1 n2 = Text.isPrefixOf (toText n1) (toText n2)

toString :: NameSegment -> String
toString = Text.unpack . toText

toTextBuilder :: NameSegment -> Text.Builder
toTextBuilder =
  coerce Text.Builder.fromText

instance Show NameSegment where
  show = Text.unpack . toText

instance IsString NameSegment where
  fromString = NameSegment . Text.pack
