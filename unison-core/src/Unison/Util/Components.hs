module Unison.Util.Components where

import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.Prelude

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
components :: Ord v => (t -> Set v) -> [(v, t)] -> [[(v, t)]]
components freeVars bs =
  let varIds =
        Map.fromList (map fst bs `zip` reverse [(1 :: Int) .. length bs])
      -- something horribly wrong if this bombs
      msg = error "Components.components bug"
      varId v = fromMaybe msg $ Map.lookup v varIds

      -- use ints as keys for graph to preserve original source order as much as
      -- possible
      graph = [((v, b), varId v, deps b) | (v, b) <- bs]
      vars = Set.fromList (map fst bs)
      deps b = varId <$> Set.toList (Set.intersection vars (freeVars b))
   in Graph.flattenSCC <$> Graph.stronglyConnComp graph
