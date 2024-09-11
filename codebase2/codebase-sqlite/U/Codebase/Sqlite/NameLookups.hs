-- | Types related to working with NameLookups.
-- We define these low-level types rather than use Path's because we don't have
-- access to those domain types given the package dependency tree.
module U.Codebase.Sqlite.NameLookups
  ( ReversedName (..),
    ReversedPath (..),
    PathSegments (..),
    NamespaceText,
    pathSegmentsToText,
    textToPathSegments,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Unison.Prelude

newtype ReversedName = ReversedName (NonEmpty Text)
  deriving stock (Eq, Ord, Show)

instance From ReversedName (NonEmpty Text)

instance From (NonEmpty Text) ReversedName

instance From ReversedName [Text] where
  from (ReversedName n) = toList n

newtype ReversedPath = ReversedPath [Text]
  deriving (Eq, Ord, Show)

instance From ReversedPath [Text]

instance From [Text] ReversedPath

newtype PathSegments = PathSegments [Text]
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance From PathSegments [Text]

instance From [Text] PathSegments

-- | A namespace rendered as a path, no leading '.'
-- E.g. "base.data"
type NamespaceText = Text

-- |
-- >>> pathSegmentsToText (PathSegments ["base", "data", "List"])
-- "base.data.List"
pathSegmentsToText :: PathSegments -> Text
pathSegmentsToText (PathSegments txt) = Text.intercalate "." txt

-- |
-- >>> textToPathSegments "base.data.List"
-- PathSegments ["base","data","List"]
textToPathSegments :: Text -> PathSegments
textToPathSegments txt = PathSegments $ Text.splitOn "." txt
