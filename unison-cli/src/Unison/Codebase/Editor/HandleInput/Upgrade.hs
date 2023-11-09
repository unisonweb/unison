module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Control.Lens (over)
import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Update2 (addDefinitionsToUnisonFile)
import Unison.Codebase.Path qualified as Path
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.Symbol (Symbol)
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UnisonFile

handleUpgrade :: NameSegment -> NameSegment -> Cli ()
handleUpgrade oldDepName newDepName = do
  Cli.Env {codebase} <- ask

  oldDepBranch <- Cli.expectBranch0AtPath (Path.fromList [Name.libSegment, oldDepName])
  newDepBranch <- Cli.expectBranch0AtPath (Path.fromList [Name.libSegment, newDepName])
  currentBranch <- Cli.getCurrentBranch0

  let namesIncludingLibdeps = Branch.toNames currentBranch
  let namesExcludingLibdeps = Branch.toNames (currentBranch & over Branch.children (Map.delete Name.libSegment))
  let namesExcludingOldDep = wundefined

  -- Compute "fake names": these are all of things in `lib.old`, with the `old` segment swapped out for `new`
  let fakeNames :: Names = wundefined
        where
          -- rename "lib.old.X" to "lib.new.X"
          rename :: Name -> Name
          rename = wundefined

  -- Create a Unison file that contains all of our dependents of things in `lib.old`.
  (unisonFile, printPPE) <-
    Cli.runTransaction do
      dependents <-
        Operations.dependentsWithinScope
          (Names.referenceIds namesExcludingLibdeps)
          (Branch.deepTermReferences oldDepBranch <> Branch.deepTypeReferences newDepBranch)
      unisonFile <- addDefinitionsToUnisonFile codebase namesExcludingLibdeps dependents UnisonFile.emptyUnisonFile
      -- Construct a PPE to use for rendering the Unison file full of dependents.
      printPPE :: PrettyPrintEnvDecl <- wundefined (namesExcludingOldDep <> fakeNames)
      pure (unisonFile, printPPE)

  -- Construct a PPE to use for rendering the Unison file full of dependents.
  let printPPE :: PrettyPrintEnvDecl
      printPPE =
        wundefined (namesExcludingOldDep <> fakeNames)

  -- Round-trip that bad boy through a bad String
  wundefined

  -- Happy path: save updated things to codebase, cons namespace. Don't forget to delete `lib.old`
  wundefined

  -- Sad path:
  --   1. Make a new project branch, stepped forward one causal (tossing `lib.old`).
  --   2. Put the busted dependents into scratch.u
  --   3. Output message or something.
  wundefined

  pure ()
