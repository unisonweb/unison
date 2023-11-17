module Unison.Codebase.Editor.HandleInput.Merge
  ( handleMerge,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Project (ProjectBranchName)

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  -- (1) Getting situated: expect alice/bob's branches to exist
  --
  -- Alice: Cli.expectCurrentProjectBranch
  -- Bob:   Cli.expectProjectBranchByName

  -- (2) Is this a three-way or two-way merge? To find the lca, it's something like
  --
  --     aliceCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
  --     bobCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)
  --     Operations.lca (Causal.causalHash aliceCausal) (Causal.causalHash bobCausal)

  -- (3) Compute the diff. See and/or copy over topic/merge `nameBasedNamespaceDiff`.
  --   (3a) Oh yeah fix synhashing for constructors (idea: use decl's synhash?)
  --   (3b) Oh yeah fix synhashing for decls (tumble in constructor names, in their canonical ordering?)
  --   ^^^^ If we skip these steps we can circle back later, after writing transcripts that demonstrate why they are
  --        needed. A couple of cases to ponder:
  --
  --        (1)
  --            LCA:
  --              type Foo.Bar = Holler
  --              type Foo = Hello.Momma
  --
  --            Alice:
  --              type Foo.Bar = inner.Holler -- I've moved the constructor down into the "inner" namespace
  --              type Foo = Bar.Holler -- I've moved the constructor to where a different decl's constructor used to be
  --
  --        (2)
  --            LCA:
  --              type Foo = Bar | Baz | Qux
  --
  --            Alice:
  --              > move.term Foo.Bar Temp.TeeHee
  --              > move.term Foo.Baz Foo.Bar
  --              > move.term Temp.TeeHee Foo.Baz
  --
  -- One issue (which is probably pervasive): code in topic/merge is all essentially avoiding "V1 Branch" stuff as much
  -- as possible, but we should probably just rewrite it to use V1 Branch stuff instead.
  --
  -- For example, `nameBasedNamespaceDiff` (on topic/merge) accepts Alice and Bob's namespaces as
  --
  --     Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
  --
  -- but this is just (roughtly equivalent to)
  --
  --     Unison.Codebase.Branch.Branch Transaction
  --
  -- except that the latter is older and cruftier, its names may be conflicted, it may have patches, etc etc.
  --
  -- Remember, too, that wherever topic/merge uses this `Defns` thing, it doesn't have any contents from lib.* (those
  -- are called `libdeps` on that branch.

  -- (4) Not sure if this should be step 4 necessarily but at some point we need to identify Alice and Bob's libdeps, so
  -- we can merge 'em. This will pull in all of the weird fresh-name logic that (for example) might merge Alice's
  -- `lib.base` and Bob's `lib.base` into a namespace with `lib.base__1` and `lib.base__2` (because their `lib.base`
  -- were different).
  --
  -- See "Load and merge libdeps" in topic/merge

  -- (5) We have two cases to handle (but I'm not sure that will actually correspond to two different code paths):
  --     (1) Name conflicts: we have to put all conflicts and all dependents of updates into scratch file
  --     (2) No conflicts: we have to put all dependents of updates into scratch file

  -- (6) Happy path, it typechecked: add to codebase, cons namespace
  --     Final namespace will have unconflicted things + updated-and-typechecking things-and-dependents + merged libdeps

  -- (7) Sad path, it didn't parse or didn't typecheck: new branch (see `handleUpgrade`), put unconflicted things in
  --     namespace and sad things in scratch file

  -- (8) Probably missed a million details above

  pure ()
