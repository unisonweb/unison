-- | Pattern match coverage checking is implemented following the
-- algorithm described in [Lower Your
-- Guards](https://simon.peytonjones.org/assets/pdfs/lower-your-guards.pdf). The
-- goal of pattern match coverage checking is to identify the
-- following problems that may arise in a pattern match:
--
-- * It is missing clauses (/i.e./ it is non-exhaustive)
-- * It contains redundant patterns (/i.e./ the case can be deleted without altering the program)
-- * It contains inaccessible patterns (/i.e/ the rhs can never be entered)
--
-- Furthermore, in the case of a non-exhaustive match, the goal to
-- present the user with concrete values that do not match any of the
-- existing patterns.
--
-- /N.B./ An inaccessible pattern in unison would be one that performs
-- effects in a guard although the constraints are unsatisfiable. Such
-- a pattern cannot be deleted without altering the program.
--
-- == High-level algorithm overview
--
-- 1. [Desugar]("Unison.PatternMatchCoverage.Desugar") a match expression into a 'Unison.PatternMatchCoverage.GrdTree.GrdTree'.
-- 2. Annotate the @GrdTree@ nodes with [refinement types]("Unison.PatternMatchCoverage.NormalizedConstraints")
-- representing values that match this node. Redundant and inaccessible patterns are then identified by @GrdTree@ leaves
-- with uninhabited refinement types. Inaccessible patterns are distinguished by an effect being performed between the
-- @GrdTree@ root and the leaf.
-- 3. Traverse the @GrdTree@ building up a refinement type representing uncovered values. If the resulting refinement type
-- is inhabited then the match is missing clauses.
-- 4. Find inhabitants of the uncovered refinement type to present to the user.
--
-- Step (1) is implemented by 'desugarMatch'. Steps (2) and (3) are
-- implemented as a single traversal: 'uncoverAnnotate'. Step (4) is
-- implemented by 'expandSolution'.
module Unison.PatternMatchCoverage
  ( checkMatch,
  )
where

import qualified Data.Set as Set
import Debug.Trace
import Unison.Pattern (Pattern)
import Unison.PatternMatchCoverage.Class (Pmc (..))
import Unison.PatternMatchCoverage.Desugar (desugarMatch)
import Unison.PatternMatchCoverage.GrdTree (prettyGrdTree)
import qualified Unison.PatternMatchCoverage.NormalizedConstraints as NC
import Unison.PatternMatchCoverage.PmGrd (prettyPmGrd)
import Unison.PatternMatchCoverage.Solve (classify, expand, expandSolution, uncoverAnnotate)
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Util.Pretty as P

-- | Perform pattern match coverage checking on a match expression
checkMatch ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  -- | the match location
  loc ->
  -- | scrutinee type
  Type.Type vt loc ->
  -- | match cases
  [Term.MatchCase loc (Term.Term' vt v loc)] ->
  -- | (redundant locations, inaccessible locations, inhabitants of uncovered refinement type)
  m ([loc], [loc], [Pattern ()])
checkMatch matchLocation scrutineeType cases = do
  v0 <- fresh
  grdtree0 <- desugarMatch matchLocation scrutineeType v0 cases
  (uncovered, grdtree1) <- uncoverAnnotate (Set.singleton (NC.declVar v0 scrutineeType id NC.emptyNormalizedConstraints)) grdtree0
  uncoveredExpanded <- concat . fmap Set.toList <$> traverse (expandSolution v0) (Set.toList uncovered)
  let sols = map (expand v0) uncoveredExpanded
  let (_accessible, inaccessible, redundant) = classify grdtree1
  let debugOutput =
        P.sep
          "\n"
          [ P.hang "desugared:" (prettyGrdTree prettyPmGrd (\_ -> "<loc>") grdtree0),
            P.hang "annotated:" (prettyGrdTree NC.prettyDnf (NC.prettyDnf . fst) grdtree1),
            P.hang "uncovered:" (NC.prettyDnf uncovered),
            P.hang "uncovered expanded:" (NC.prettyDnf (Set.fromList uncoveredExpanded))
          ]
      shouldDebug = False
      doDebug = case shouldDebug of
        True -> trace (P.toPlainUnbroken debugOutput)
        False -> id
  doDebug (pure (redundant, inaccessible, sols))
