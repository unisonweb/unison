-- | @release.draft@ input handler
module Unison.Codebase.Editor.HandleInput.ReleaseDraft
  ( handleReleaseDraft,
  )
where

import Control.Lens ((^.))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase.Editor.HandleInput.Branch (CreateFrom (..), doCreateBranch)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (Semver)
import Witch (unsafeFrom)

-- | Handle a @release.draft@ command.
handleReleaseDraft :: Semver -> Cli ()
handleReleaseDraft ver = do
  currentProjectAndBranch <- fst <$> ProjectUtils.expectCurrentProjectBranch

  let branchName = unsafeFrom @Text ("releases/drafts/" <> into @Text ver)

  _ <-
    doCreateBranch
      (CreateFrom'Branch currentProjectAndBranch)
      (currentProjectAndBranch ^. #project)
      branchName
      ("release.draft " <> into @Text ver)

  Cli.respond (Output.DraftingRelease branchName ver)
