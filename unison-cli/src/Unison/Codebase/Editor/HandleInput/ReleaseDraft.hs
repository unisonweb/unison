-- | @release.draft@ input handler
module Unison.Codebase.Editor.HandleInput.ReleaseDraft
  ( handleReleaseDraft,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Editor.HandleInput.Branch (CreateFrom (..), createBranch)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (Semver)
import Witch (unsafeFrom)

-- | Handle a @release.draft@ command.
handleReleaseDraft :: Semver -> Cli ()
handleReleaseDraft ver = do
  currentProjectAndBranch <- Cli.getCurrentProjectAndBranch

  let branchName = unsafeFrom @Text ("releases/drafts/" <> into @Text ver)

  _ <-
    createBranch
      ("release.draft " <> into @Text ver)
      (CreateFrom'ParentBranch (currentProjectAndBranch ^. #branch))
      (currentProjectAndBranch ^. #project)
      (pure branchName)

  Cli.respond (Output.DraftingRelease branchName ver)
