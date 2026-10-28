# Change log

## 0.7

* #294: Change the argument order of `bfs*`, `dfs*` and `reachable` algorithms.
* #293: Fix the `ToGraph` instance of symmetric relations.

## 0.6.1

* Drop dependency on `mtl`.

## 0.6

* #276: Add `Monoid` and `Semigroup` instances.
* #278: Stop supporting GHC 8.0 and GHC 8.2.
* #274, #277: Expand the API and add algorithms for bipartite graphs, drop the
              `Undirected` component in `Bipartite.Undirected.AdjacencyMap`.
* #273: Add attribute quoting style to `Export.Dot`.
* #259: Allow newer QuickCheck.
* #257: Add `IsString` instances.
* #226: Expand the API of `Bipartite.Undirected.AdjacencyMap`.

## 0.5

* #217, #224, #227, #234, #235: Add new BFS, DFS, topological sort, and SCC
                                algorithms for adjacency maps.
* #228, #247, #254: Improve algebraic graph fusion.
* #207, #218, #255: Add `Bipartite.Undirected.AdjacencyMap`.
* #220, #237, #255: Add `Algebra.Graph.Undirected`.
* #203, #215, #223: Add `Acyclic.AdjacencyMap`.
* #202, #209, #211: Add `induceJust` and `induceJust1`.
* #172, #245: Stop supporting GHC 7.8 and GHC 7.10.
* #208: Add `fromNonEmpty` to `NonEmpty.AdjacencyMap`.
* #208: Add `fromAdjacencyMap` to `AdjacencyIntMap`.
* #208: Drop `Internal` modules for `AdjacencyIntMap`, `AdjacencyMap`,
        `Labelled.AdjacencyMap`, `NonEmpty.AdjacencyMap`, `Relation` and
        `Relation.Symmetric`.
* #206: Add `Algebra.Graph.AdjacencyMap.box`.
* #205: Drop dependencies on `base-compat` and `base-orphans`.
* #205: Remove `Algebra.Graph.Fold`.
* #151: Remove `ToGraph.size`. Demote `ToGraph.adjacencyMap`,
        `ToGraph.adjacencyIntMap`, `ToGraph.adjacencyMapTranspose` and
        `ToGraph.adjacencyIntMapTranspose` to functions.
* #204: Derive `Generic` and `NFData` for `Algebra.Graph` and `Algebra.Graph.Labelled`.

## 0.4

* #174: Add `Symmetric.Relation`.
* #143: Allow newer QuickCheck.
* #171: Implement sparsification for King-Launchbury graph representation.
* #178: Derive `Generic` for adjacency maps.

## 0.3

* #129: Add a testsuite for rewrite rules based on the `inspection-testing` library.
* #63, #148: Add relational composition of algebraic graphs.
* #139, #146: Add relational operations to adjacency maps.
* #146: Rename `preorderClosure` to `closure`.
* #146: Switch to left-to-right composition in `Relation.compose`.
* #143: Allow newer QuickCheck.
* #140, #142: Fix `Show` instances.
* #128, #130: Modify the SCC algorithm to return non-empty graph components.
* #130: Move adjacency map algorithms to separate modules.
* #130: Export `fromAdjacencySets` and `fromAdjacencyIntSets`.
* #138: Do not require `Eq` instance on the string type when exporting graphs.
* #136: Rename `Algebra.Graph.NonEmpty.NonEmptyGraph` to `Algebra.Graph.NonEmpty.Graph`.
* #136: Add `Algebra.Graph.NonEmpty.AdjacencyMap`.
* #136: Remove `vertexIntSet` from the API of basic graph data types. Also
        remove `Algebra.Graph.adjacencyMap` and `Algebra.Graph.adjacencyIntMap`.
        This functionality is still available from the type class `ToGraph`.
* #126, #131: Implement custom `Ord` instance.
* #17, #122, #125, #149: Add labelled algebraic graphs.
* #121: Drop `Foldable` and `Traversable` instances.
* #113: Add `Labelled.AdjacencyMap`.

## 0.2

* #117: Add `sparsify`.
* #115: Add `isDfsForestOf`.
* #114: Add a basic implementation of edge-labelled graphs.
* #107: Drop `starTranspose`.
* #106: Extend `ToGraph` with algorithms based on adjacency maps.
* #106: Add `isAcyclic` and `reachable`.
* #106: Rename `isTopSort` to `isTopSortOf`.
* #102: Switch the master branch to GHC 8.4.3. Add a CI instance for GHC 8.6.1.
* #101: Drop `-O2` from the `ghc-options` section of the Cabal file.
* #100: Rename `fromAdjacencyList` to `stars`.
* #79: Improve the API consistency: rename `IntAdjacencyMap` to `AdjacencyIntMap`,
       and then rename the function that extracts its adjacency map to
       `adjacencyIntMap` to avoid the clash with `AdjacencyMap.adjacencyMap`,
       which has incompatible type.
* #82, #92: Add performance regression suite.
* #76: Remove benchmarks.
* #74: Drop dependency of `Algebra.Graph` on graph type classes.
* #62: Move King-Launchbury graphs into `Data.Graph.Typed`.
* #67, #68, #69, #77, #81, #93, #94, #97, #103, #110: Various performance improvements.
* #66, #72, #96, #98: Add missing `NFData` instances.

## 0.1.1.1

* #59: Allow `base-compat-0.10`.

## 0.1.1

* #58: Update documentation.
* #57: Allow newer QuickCheck.

## 0.1.0

* Start complying with PVP.
* #48: Add `starTranspose`.
* #48: Add `foldg` to `ToGraph`.
* #15: Optimise `removeEdge`.
* #39: Factor out difference lists into `Algebra.Graph.Internal`.
* #31: Add `Algebra.Graph.NonEmpty`.
* #32: Remove smart constructor `graph`.
* #27, #55: Support GHC versions 7.8.4, 7.10.3, 8.0.2, 8.2.2, 8.4.1.
* #25: Add `NFData Graph` instance.
* General improvements to code, documentation and tests.

## 0.0.5

* Add `dfs`.
* #19: Move `GraphKL` to an internal module.
* #18: Add `dfsForestFrom`.
* #16: Add support for graph export, in particular in DOT format.
* Make API more consistent, e.g. rename `postset` to `postSet`.
* Improve documentation and tests.
