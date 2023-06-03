-- | Helpers which are specific to the remote share server.
module Unison.Server.Share (relocateToNameRoot) where

import Control.Lens hiding ((??))
import Data.List.NonEmpty qualified as NonEmpty
import U.Codebase.HashTags (BranchHash)
import U.Codebase.Sqlite.NameLookups (PathSegments (..))
import U.Codebase.Sqlite.Operations (NamesPerspective (..))
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

-- | Given an arbitrary query and perspective, find the name root the query belongs in,
-- then return that root and the query relocated to that root.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ or @.myproject.lib.base.List -> .myproject.lib.base@
relocateToNameRoot :: Path -> HQ.HashQualified Name -> BranchHash -> Sqlite.Transaction (NamesPerspective, HQ.HashQualified Name)
relocateToNameRoot perspective query rootBh = do
  -- The namespace containing the name path
  let nameLocation = case HQ.toName query of
        Just name ->
          name
            & Name.reverseSegments
            & NonEmpty.tail
            & Path.fromList
        Nothing -> Path.empty
  let fullPath = perspective <> nameLocation
  namesPerspective@NamesPerspective {relativePerspective} <- Ops.namesPerspectiveForRootAndPath rootBh (PathSegments . coerce . Path.toList $ fullPath)
  let reprefixName name = Name.fromReverseSegments $ (NonEmpty.head $ Name.reverseSegments name) NonEmpty.:| (coerce relativePerspective)
  pure (namesPerspective, reprefixName <$> query)
