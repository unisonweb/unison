module Unison.NameSegment
  ( NameSegment (UnsafeNameSegment),
    toUnescapedText,
    isEmpty,
    isPrefixOf,
    toTextBuilder,

    -- * Sentinel name segments
    defaultPatchSegment,
    docSegment,
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

instance IsString NameSegment where
  fromString =
    UnsafeNameSegment . Text.pack

instance Show NameSegment where
  show = show . toUnescapedText

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

defaultPatchSegment :: NameSegment
defaultPatchSegment =
  "patch"

docSegment :: NameSegment
docSegment =
  "doc"

libSegment :: NameSegment
libSegment =
  "lib"
