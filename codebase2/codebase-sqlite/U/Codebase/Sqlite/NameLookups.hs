-- | Types related to working with NameLookups.
-- We define these low-level types rather than use Path's because we don't have
-- access to those domain types given the package dependency tree.
module U.Codebase.Sqlite.NameLookups
  ( ReversedName (..),
    ReversedPath (..),
    PathSegments (..),
    NamespaceText,
    reversedNameToNamespaceText,
    nameLookupForPerspective,
    pathSegmentsToText,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text
import U.Codebase.Sqlite.DbId
  ( BranchHashId (..),
  )
import Unison.Prelude
import Unison.Sqlite

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
  deriving (Eq, Ord, Show)

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
-- >>> reversedSegmentsToNamespaceText (["List", "data", "base"])
-- "base.data.List"
reversedNameToNamespaceText :: ReversedName -> NamespaceText
reversedNameToNamespaceText (ReversedName txt) = Text.intercalate "." . reverse . toList $ txt

-- | Determine which nameLookup is the closest parent of the provided perspective.
--
-- Returns (rootBranchId of the closest index, namespace that index is mounted at, location of the perspective within the mounted namespace)
--
-- E.g.
-- If your namespace is "lib.distributed.lib.base.data.List", you'd get back
-- (rootBranchId of the lib.base name lookup, "lib.distributed.lib.base", "data.List")
--
-- Or if your namespace is "subnamespace.user", you'd get back
-- (the rootBranchId you provided, "", "subnamespace.user")
nameLookupForPerspective :: BranchHashId -> PathSegments -> Transaction (BranchHashId, PathSegments, PathSegments)
nameLookupForPerspective = undefined
