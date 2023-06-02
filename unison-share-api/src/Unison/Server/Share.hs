-- | Helpers which are specific to the remote share server.
module Unison.Server.Share (relocateToNameRoot) where

import Control.Lens hiding ((??))
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Projects as Projects
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Prelude
import Unison.Server.Backend
import Unison.Sqlite qualified as Sqlite

-- | Given an arbitrary query and perspective, find the name root the query belongs in,
-- then return that root and the query relocated to that root.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ or @.myproject.lib.base.List -> .myproject.lib.base@
relocateToNameRoot :: Path -> HQ.HashQualified Name -> V2Branch.Branch Sqlite.Transaction -> Sqlite.Transaction (Either BackendError (Path, HQ.HashQualified Name))
relocateToNameRoot perspective query rootBranch = undefined
