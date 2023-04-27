-- | @release.draft@ input handler
module Unison.Codebase.Editor.HandleInput.ReleaseDraft
  ( handleReleaseDraft,
  )
where

import Control.Lens ((^.))
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import Unison.Codebase.Editor.HandleInput.Branch (CreateFrom (..), doCreateBranch)
import Unison.Prelude
import Unison.Project (Semver)
import Witch (unsafeFrom)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase.Editor.Output as Output

-- | Handle a @release.draft@ command.
handleReleaseDraft :: Semver -> Cli ()
handleReleaseDraft ver = do
  currentProjectAndBranch <- ProjectUtils.expectCurrentProjectBranch

  let branchName = unsafeFrom @Text ("releases/drafts/" <> into @Text ver)

  doCreateBranch
    (CreateFrom'Branch currentProjectAndBranch)
    (currentProjectAndBranch ^. #project)
    branchName
    ("release.draft " <> into @Text ver)

  Cli.respond (Output.DraftingRelease branchName ver)
