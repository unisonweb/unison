-- Pattern match coverage checking following the algorithm described
-- in "Lower Your Guards".
-- https://simon.peytonjones.org/assets/pdfs/lower-your-guards.pdf
module Unison.PatternMatchCoverage where

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

checkMatch ::
  forall vt v loc m.
  (Pmc vt v loc m) =>
  loc ->
  Type.Type vt loc ->
  [Term.MatchCase loc (Term.Term' vt v loc)] ->
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
