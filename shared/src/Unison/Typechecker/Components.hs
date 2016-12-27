module Unison.Typechecker.Components (components, minimize, minimize') where

import Data.Maybe
import Unison.Term (Term)
import Unison.Var (Var)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import qualified Data.Graph as Graph
import qualified Data.Graph.SCC as SCC
import qualified Data.Map as Map

-- | Order bindings by dependencies and group into components.
-- Each component consists of > 1 bindings, each of which depends
-- transitively on all other bindings in the component.
--
-- 1-element components may or may not depend on themselves.
--
-- The order is such that a component at index i will not depend
-- on components and indexes > i. But a component at index i does not
-- _necessarily_ depend on any components at earlier indices.
--
-- Example:
--
--   let rec
--     ping n = pong (n + 1);
--     pong n = ping (n + 1);
--     g = id 42;
--     y = id "hi"
--     id x = x;
--   in ping g
--
-- `components` would produce `[[ping,pong], [id], [g], [y]]`
-- Notice that `id` comes before `g` and `y` in the output, since
-- both `g` and `y` depend on `id`.
--
-- Uses Tarjan's algorithm:
--   https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
components :: Var v => [(v, Term v)] -> [[(v, Term v)]]
components bs =
  let
    varIds = Map.fromList (map fst bs `zip` [(0::Int)..])
    varId v = fromJust $ Map.lookup v varIds -- something horribly wrong if this bombs
    -- use ints as keys for graph to preserve original source order as much as possible
    (graph, toNode, _) =
      Graph.graphFromEdges [ ((v,b), varId v, deps b) | (v,b) <- bs ]
    toBindingGroup vertices = map (_1 . toNode) vertices
    _1 (a,_,_) = a
    vars = Set.fromList (map fst bs)
    deps b = varId <$> Set.toList (Set.intersection vars (ABT.freeVars b))
  in
    toBindingGroup . Graph.flattenSCC <$> SCC.sccList graph

-- | Algorithm for minimizing cycles of a `let rec`. This can
-- improve generalization during typechecking and may also be more
-- efficient for execution.
--
-- For instance:
--
-- minimize (let rec id x = x; g = id 42; y = id "hi" in g)
-- ==>
-- Just (let id x = x; g = id 42; y = id "hi" in g)
--
-- Gets rid of the let rec and replaces it with an ordinary `let`, such
-- that `id` is suitably generalized.
minimize :: Var v => Term v -> Maybe (Term v)
minimize (Term.LetRecNamed' bs e) = case components bs of
  [_single] -> Nothing
  cs -> Just $ foldr mklet e cs where
    mklet [(hdv,hdb)] e
      | Set.member hdv (ABT.freeVars hdb) = Term.letRec [(hdv,hdb)] e
      | otherwise                         = Term.let1 [(hdv,hdb)] e
    mklet cycle e = Term.letRec cycle e
minimize _ = Nothing

minimize' :: Var v => Term v -> Term v
minimize' term = fromMaybe term (minimize term)
