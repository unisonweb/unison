-- | Project-related utilities.
module Unison.Cli.ProjectUtils
  ( getCurrentProjectBranch,
    projectPath,
    projectBranchPath,

    -- ** Path prisms
    projectBranchPathPrism,
  )
where

import Control.Lens
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Codebase.Path as Path
import Unison.NameSegment (NameSegment (..))

-- | Get the current project+branch that a user is on.
--
-- Note that if a user has (somehow) cd'd into a namespace *within* a branch, this function will return Nothing; that
-- is, it only returns Just if the user's current namespace is the root of a branch, and no deeper.
--
-- This should be fine: we don't want users to be able to cd around willy-nilly within projects (right?...)
getCurrentProjectBranch :: Cli (Maybe (Queries.ProjectId, Queries.BranchId))
getCurrentProjectBranch = do
  path <- Cli.getCurrentPath
  pure (preview projectBranchPathPrism path)

-- | Get the path that a project is stored at. Users aren't supposed to go here.
--
-- >>> projectPath "ABCD"
-- .__projects._ABCD
projectPath :: Queries.ProjectId -> Path.Absolute
projectPath projectId =
  review projectPathPrism projectId

-- | Get the path that a branch is stored at. Users aren't supposed to go here.
--
-- >>> projectBranchPath "ABCD" "DEFG"
-- .__projects._ABCD.branches._DEFG
projectBranchPath :: Queries.ProjectId -> Queries.BranchId -> Path.Absolute
projectBranchPath projectId branchId =
  review projectBranchPathPrism (projectId, branchId)

------------------------------------------------------------------------------------------------------------------------

pattern UUIDNameSegment :: UUID -> NameSegment
pattern UUIDNameSegment uuid <-
  NameSegment (Text.uncons -> Just ('_', UUID.fromText . Text.map (\c -> if c == '_' then '-' else c) -> Just uuid))
  where
    UUIDNameSegment uuid = NameSegment (Text.cons '_' (Text.map (\c -> if c == '-' then '_' else c) (UUID.toText uuid)))

-- | The prism between paths like
--
-- @
-- .__projects._XX_XX
-- @
--
-- and the project id
--
-- @
-- XX-XX
-- @
projectPathPrism :: Prism' Path.Absolute Queries.ProjectId
projectPathPrism =
  prism' toPath toId
  where
    toPath :: Queries.ProjectId -> Path.Absolute
    toPath projectId =
      Path.Absolute $
        Path.fromList
          [ "__projects",
            UUIDNameSegment (Queries.unProjectId projectId)
          ]

    toId :: Path.Absolute -> Maybe Queries.ProjectId
    toId path =
      case Path.toList (Path.unabsolute path) of
        ["__projects", UUIDNameSegment projectId] -> Just (Queries.ProjectId projectId)
        _ -> Nothing

-- | The prism between paths like
--
-- @
-- .__projects._XX_XX.branches._YY_YY
-- @
--
-- and the @(project id, branch id)@ pair
--
-- @
-- (XX-XX, YY-YY)
-- @
projectBranchPathPrism :: Prism' Path.Absolute (Queries.ProjectId, Queries.BranchId)
projectBranchPathPrism =
  prism' toPath toIds
  where
    toPath :: (Queries.ProjectId, Queries.BranchId) -> Path.Absolute
    toPath (projectId, branchId) =
      Path.Absolute $
        Path.fromList
          [ "__projects",
            UUIDNameSegment (Queries.unProjectId projectId),
            "branches",
            UUIDNameSegment (Queries.unBranchId branchId)
          ]

    toIds :: Path.Absolute -> Maybe (Queries.ProjectId, Queries.BranchId)
    toIds path =
      case Path.toList (Path.unabsolute path) of
        ["__projects", UUIDNameSegment projectId, "branches", UUIDNameSegment branchId] ->
          Just (Queries.ProjectId projectId, Queries.BranchId branchId)
        _ -> Nothing
