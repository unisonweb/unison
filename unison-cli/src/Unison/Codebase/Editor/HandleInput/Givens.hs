-- | Handlers for the @mark.given@, @unmark.given@, and @givens@
-- commands.
--
-- These commands toggle and inspect the @##Builtin.Given@ sentinel
-- stored in 'Unison.Codebase.Branch.Type.MdValues'. They are pure
-- namespace operations: the term hash of the referent is unchanged
-- when (un)marking, so dependents are not disturbed.
module Unison.Codebase.Editor.HandleInput.Givens
  ( handleMarkGiven,
    handleUnmarkGiven,
    handleGivens,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Givens qualified as Givens
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Referent (Referent)
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 qualified as Star2

-- | Resolve a hash-qualified name to a unique 'Referent' at the given
-- split path within the current project. Returns the referent, the
-- absolute parent path together with the trailing name segment, and
-- the bare hash-qualified name (for diagnostic output).
resolveSingleTerm ::
  HQ'.HashQualified (Path.Split Path') ->
  Cli (Referent, Path.Split Path.Absolute, HQ'.HashQualified Name)
resolveSingleTerm hq = do
  hqResolved <- traverse Cli.resolveSplit' hq
  termsAt <- Cli.getTermsAt hqResolved
  case Set.toList termsAt of
    [r] -> do
      let absSplit :: Path.Split Path.Absolute
          absSplit = first (view PP.absPath_) (HQ'.toName hqResolved)
      let nameOnly :: HQ'.HashQualified Name
          nameOnly = fmap Path.nameFromSplit hq
      pure (r, absSplit, nameOnly)
    [] -> Cli.returnEarly (TermNotFound hq)
    _ -> Cli.returnEarly (DeleteNameAmbiguous 10 hq termsAt Set.empty)

handleMarkGiven :: HQ'.HashQualified (Path.Split Path') -> Cli ()
handleMarkGiven hq = do
  (referent, absSplit, hqName) <- resolveSingleTerm hq
  -- Walk the parent path from the *project root*, not from the
  -- current namespace: 'absSplit' is rooted at the project root, so
  -- starting from 'getCurrentBranch0' (which is already at the
  -- current sub-namespace) would walk into an already-deep branch
  -- and mis-locate the parent. Using the project root makes the
  -- read path consistent with the write side ('stepManyAt' below,
  -- which also takes a project-root-absolute path).
  projectRoot <- Cli.getCurrentProjectRoot0
  let (parentAbsPath, seg) = absSplit
  -- Resolve the branch at the parent path so we can ask if the
  -- referent is already marked /there/.
  let parentBranch :: Branch0 IO
      parentBranch =
        Branch.getAt0 (Path.unabsolute parentAbsPath) projectRoot
  if Givens.isGivenAt referent seg parentBranch
    then Cli.respond (AlreadyMarkedGiven hqName)
    else do
      pb <- Cli.getCurrentProjectBranch
      Cli.stepManyAt
        pb
        ("mark.given " <> HQ'.toTextWith Name.toText hqName)
        [(parentAbsPath, Givens.markGivenAt referent seg)]
      Cli.respond (MarkedGiven hqName)

handleUnmarkGiven :: HQ'.HashQualified (Path.Split Path') -> Cli ()
handleUnmarkGiven hq = do
  (referent, absSplit, hqName) <- resolveSingleTerm hq
  -- See comment in 'handleMarkGiven' for why we read from the
  -- project root rather than from the current branch.
  projectRoot <- Cli.getCurrentProjectRoot0
  let (parentAbsPath, seg) = absSplit
  let parentBranch :: Branch0 IO
      parentBranch =
        Branch.getAt0 (Path.unabsolute parentAbsPath) projectRoot
  if not (Givens.isGivenAt referent seg parentBranch)
    then Cli.respond (NotMarkedGiven hqName)
    else do
      pb <- Cli.getCurrentProjectBranch
      Cli.stepManyAt
        pb
        ("unmark.given " <> HQ'.toTextWith Name.toText hqName)
        [(parentAbsPath, Givens.unmarkGivenAt referent seg)]
      Cli.respond (UnmarkedGiven hqName)

-- | List the (deep) referents under the current namespace whose
-- metadata carries the given sentinel, paired with the names they are
-- known by within the namespace. Per-referent: a referent that
-- appears under several aliases will appear once per alias.
--
-- 'Givens.isGiven' inspects the metadata stored on the supplied
-- branch alone, so to find nested givens we walk children recursively
-- and consult @isGiven@ at each level.
handleGivens :: Cli ()
handleGivens = do
  branch0 <- Cli.getCurrentBranch0
  -- Skip the 'lib' subtree so 'givens' does not surface dependency
  -- givens. This matches 'find', 'edit.namespace', and the
  -- dependents commands, which all consult 'Branch.withoutLib'
  -- before walking the namespace.
  let searchBranch = Branch.withoutLib branch0
  let givens :: [(Name, Referent)]
      givens = collectGivens [] searchBranch
  Cli.respond (ListGivens givens)
  where
    collectGivens :: [NameSegment] -> Branch0 m -> [(Name, Referent)]
    collectGivens revPrefix b0 =
      let here :: [(Name, Referent)]
          here =
            [ (Name.fromReverseSegments (seg :| revPrefix), r)
            | (r, seg) <- Relation.toList (Star2.d1 (view Branch.terms_ b0)),
              Givens.isGiven r b0
            ]
          there =
            concatMap
              ( \(seg, child) ->
                  collectGivens (seg : revPrefix) (Branch.head child)
              )
              (Map.toList (view Branch.children_ b0))
       in here ++ there
