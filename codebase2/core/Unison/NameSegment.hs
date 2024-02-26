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
-- You might use this when storing a name segment as text in a database, where the literal name segment bytes are all
-- that matter. However, you wouldn't use this to display the name segment to a user - that depends on concrete syntax.
-- See Unison.Syntax.NameSegment (or indeed, some actual yet-built interface that abstracts concrete syntax) for that
-- kind of function.
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
