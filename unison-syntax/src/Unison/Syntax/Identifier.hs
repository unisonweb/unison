-- | An identifer, as seen by the default syntax.
module Unison.Syntax.Identifier
  ( Identifier (..),

    -- * Basic manipulation
    appendSegment,

    -- * String conversions
    toText,
  )
where

import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Text as Text
import qualified Text.Builder
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical (..))

-- | An identifier is a non-empty list of segments, plus a bit tracking whether or not there was a leading dot.
data Identifier = Identifier
  { leadingDot :: Bool,
    segments :: List.NonEmpty Text
  }
  deriving stock (Eq, Show, Ord)

instance Alphabetical Identifier where
  compareAlphabetical x y =
    compareAlphabetical (toText x) (toText y)

-- | Append a segment to an identifier.
appendSegment :: Identifier -> Text -> Identifier
appendSegment Identifier {leadingDot, segments} seg =
  Identifier
    { leadingDot,
      segments = segments <> (seg List.NonEmpty.:| [])
    }

-- | Render an identifier as text, escaping all segments that contain '.'
toText :: Identifier -> Text
toText Identifier {leadingDot, segments} =
  Text.Builder.run (foldl' (\xs x -> xs <> segmentToText x) (if leadingDot then "." else mempty) segments)
  where
    segmentToText :: Text -> Text.Builder.Builder
    segmentToText seg =
      if isJust (Text.find (== '.') seg)
        then backtick <> Text.Builder.text seg <> backtick
        else Text.Builder.text seg
    backtick :: Text.Builder.Builder
    backtick =
      "`"
