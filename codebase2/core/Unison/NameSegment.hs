module Unison.NameSegment
  ( NameSegment (..),
    toUnescapedText,
    isPrefixOf,

    -- * Sentinel name segments
    defaultPatchSegment,
    docSegment,
    libSegment,
  )
where

import Data.Text qualified as Text
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical)

-- Represents the parts of a name between the `.`s
newtype NameSegment
  = NameSegment Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Alphabetical)

instance IsString NameSegment where
  fromString =
    NameSegment . Text.pack

instance Show NameSegment where
  show =
    Text.unpack . toUnescapedText

-- | Convert a name segment to unescaped text.
--
-- > toUnescapedText (unsafeFromText ".~") = ".~"
toUnescapedText :: NameSegment -> Text
toUnescapedText =
  coerce

isPrefixOf :: NameSegment -> NameSegment -> Bool
isPrefixOf =
  coerce Text.isPrefixOf

defaultPatchSegment :: NameSegment
defaultPatchSegment =
  "patch"

docSegment :: NameSegment
docSegment =
  "doc"

libSegment :: NameSegment
libSegment =
  "lib"
