module Unison.Typechecker.Components (components, minimize, minimize') where

import           Control.Arrow ((&&&))
import           Data.Bifunctor (first)
import           Data.Function (on)
import qualified Data.Graph as Graph
import           Data.List (groupBy, sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import           Unison.Term (AnnotatedTerm')
import qualified Unison.Term as Term
import           Unison.Var (Var)

components
  :: Var v => [(v, ABT.Term f v a)] -> [[(v, ABT.Term f v a)]]
components = components' ABT.freeVars

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
components' :: Var v => (t -> Set v) -> [(v, t)] -> [[(v, t)]]
components' freeVars bs =
  let varIds =
        Map.fromList (map fst bs `zip` reverse [(1 :: Int) .. length bs])
      -- something horribly wrong if this bombs
      varId v = fromJust $ Map.lookup v varIds

      -- use ints as keys for graph to preserve original source order as much as
      -- possible
      graph = [ ((v, b), varId v, deps b) | (v, b) <- bs ]
      vars  = Set.fromList (map fst bs)
      deps b = varId <$> Set.toList (Set.intersection vars (freeVars b))
  in  Graph.flattenSCC <$> Graph.stronglyConnComp graph

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
--
-- Fails on the left if there are duplicate definitions.
minimize
  :: Var v
  => AnnotatedTerm' vt v a
  -> Either (NonEmpty (v, [a])) (Maybe (AnnotatedTerm' vt v a))
minimize (Term.LetRecNamedAnnotatedTop' isTop ann bs e) =
  let bindings = first snd <$> bs
      group    = map (fst . head &&& map (ABT.annotation . snd)) . groupBy ((==) `on` fst) . sortBy
        (compare `on` fst)
      grouped = group bindings
      dupes   = filter ((> 1) . length . snd) grouped
  in  if not $ null dupes
        then Left $ Nel.fromList dupes
        else
          let cs             = components bindings
              varAnnotations = Map.fromList ((\((a, v), _) -> (v, a)) <$> bs)
              annotationFor v = fromJust $ Map.lookup v varAnnotations
              annotatedVar v = (annotationFor v, v)
              -- When introducing a nested let/let rec, we use the annotation
              -- of the variable that starts off that let/let rec
              mklet [(hdv, hdb)] e
                | Set.member hdv (ABT.freeVars hdb) = Term.letRec isTop
                  (annotationFor hdv)
                  [(annotatedVar hdv, hdb)]
                  e
                | otherwise = Term.let1 isTop [(annotatedVar hdv, hdb)] e
              mklet cycle@((hdv, _) : _) e = Term.letRec isTop
                (annotationFor hdv)
                (first annotatedVar <$> cycle)
                e
              mklet [] e = e
          in
            -- The outer annotation is going to be meaningful, so we make
            -- sure to preserve it, whereas the annotations at intermediate Abs
            -- nodes aren't necessarily meaningful
              Right . Just . ABT.annotate ann . foldr mklet e $ cs
minimize _ = Right Nothing

minimize'
  :: Var v => AnnotatedTerm' vt v a -> Either (NonEmpty (v,[a])) (AnnotatedTerm' vt v a)
minimize' term = fromMaybe term <$> minimize term
