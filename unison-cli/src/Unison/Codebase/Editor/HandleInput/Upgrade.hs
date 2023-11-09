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

  let namesExcludingLibdeps = Branch.toNames (currentBranch & over Branch.children (Map.delete Name.libSegment))

  unisonFile <-
    Cli.runTransaction do
      dependents <-
        Operations.dependentsWithinScope
          (Names.referenceIds namesExcludingLibdeps)
          (Branch.deepTermReferences oldDepBranch <> Branch.deepTypeReferences newDepBranch)
      addDefinitionsToUnisonFile codebase namesExcludingLibdeps dependents UnisonFile.emptyUnisonFile

  let allNames = Branch.toNames currentBranch

  -- Compute "fake names": these are all of things in `lib.old`, with the `old` segment swapped out for `new`
  let fakeNames :: Names = wundefined
        where
          -- rename "lib.old.X" to "lib.new.X"
          rename :: Name -> Name
          rename = wundefined

  -- Construct a PPE to use for rendering the Unison file full of dependents. This PPE should shadow everything in
  -- `allNames` with `fakeNames`.
  --
  -- Note to implementor: this isn't exactly possible with the existing PPE API
  let printPPE :: PrettyPrintEnvDecl
      printPPE = wundefined

  -- Make a biggol Unison file with all of the dependents of everything in `lib.old`
  let unisonFile :: UnisonFile Symbol Ann
      unisonFile = wundefined

  -- Round-trip that bad boy through a bad String
  wundefined

  -- Happy path: save updated things to codebase, cons namespace. Don't forget to delete `lib.old`
  wundefined

  -- Sad path: put the busted dependents into scratch.u. None of the string names in that file should resolve to
  -- anything in `lib.old` since they were all shadowed by the PPE.

  pure ()
