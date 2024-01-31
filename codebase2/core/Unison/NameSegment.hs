module Unison.NameSegment
  ( NameSegment (UnsafeNameSegment),
    unsafeFromUnescapedText,
    toUnescapedText,
    isEmpty,
    isPrefixOf,
    toTextBuilder,
    libSegment,
  )
where

import Data.Text qualified as Text
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical)

-- Represents the parts of a name between the `.`s
newtype NameSegment
  = UnsafeNameSegment Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Alphabetical)

instance Show NameSegment where
  show = show . toUnescapedText

-- | Convert a text to a name segment, when the text is known to be a valid name segment.
--
-- For example, to make a name segment containing the text ".~", use @unsafeFromUnescapedText ".~"@, even if that
-- operator would need to be escaped (e.g. "`.~`") when written by a user.
unsafeFromUnescapedText :: Text -> NameSegment
unsafeFromUnescapedText =
  UnsafeNameSegment

-- | Convert a name segment to unescaped text.
--
-- > toUnescapedText (unsafeFromText ".~") = ".~"
toUnescapedText :: NameSegment -> Text
toUnescapedText =
  coerce

isEmpty :: NameSegment -> Bool
isEmpty =
  coerce Text.null

isPrefixOf :: NameSegment -> NameSegment -> Bool
isPrefixOf =
  coerce Text.isPrefixOf

toTextBuilder :: NameSegment -> Text.Builder
toTextBuilder =
  coerce Text.Builder.fromText

libSegment :: NameSegment
libSegment =
  unsafeFromUnescapedText "lib"
