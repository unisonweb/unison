-- | References to things on Unison Share, as written in the @share.*@ commands.
module Unison.Cli.Share.Ref
  ( ShareBranch (..),
    ShareRef (..),
    parseShareRef,
  )
where

import Data.Text qualified as Text
import Unison.Prelude

-- | Which branch (or release) of a project to look at.
data ShareBranch
  = -- | The latest release, or the default branch if there are no releases.
    ShareLatest
  | -- | A release, by version, e.g. @2.1.2@ for @\@user/project/releases/2.1.2@.
    ShareRelease Text
  | ShareBranchNamed Text
  deriving stock (Eq, Show)

-- | @\@user/project[/branch]:path.to.definition@
data ShareRef = ShareRef
  { -- | The owner's handle, without the leading @\@@.
    handle :: Text,
    slug :: Text,
    branch :: ShareBranch,
    path :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Parse @\@user/project[/branch]:path@, where the branch may be @releases/<version>@.
parseShareRef :: Text -> Either Text ShareRef
parseShareRef input = do
  let (projectPart, pathPart) = Text.breakOn ":" input
  path <-
    case Text.stripPrefix ":" pathPart of
      Nothing -> Right Nothing
      Just "" -> Left ("Expected a name after the `:` in " <> input)
      Just p -> Right (Just p)
  case Text.splitOn "/" projectPart of
    (Text.stripPrefix "@" -> Just handle) : slug : rest
      | not (Text.null handle) && not (Text.null slug) -> do
          branch <-
            case rest of
              [] -> Right ShareLatest
              ["releases", version] | not (Text.null version) -> Right (ShareRelease version)
              [name] | not (Text.null name) -> Right (ShareBranchNamed name)
              _ -> Left ("I don't understand the branch in " <> input <> ". Use @user/project/branch or @user/project/releases/1.2.3")
          Right ShareRef {handle, slug, branch, path}
    _ -> Left ("Expected a reference like @user/project, @user/project/releases/1.2.3 or @user/project:some.name, but got " <> input)
