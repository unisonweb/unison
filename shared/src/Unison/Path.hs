-- |
-- Provides a typeclass for a general concept of a path into
-- a treelike structure. We have a root or empty path, paths
-- may be concatenated, and a pair of paths may be factored into
-- paths relative to their lowest common ancestor in the tree.

module Unison.Path where

-- | Satisfies:
--   * `extend root p == p` and `extend p root == p`
--   * `extend` is associative, `extend (extend p1 p2) p3 == extend p1 (extend p2 p3)`
--   * `lca root p == root` and `lca p root == root`
--   * `case factor p p2 of (r,p',p2') -> extend r p' == p && extend r p2' == p2`
class Path p where
  -- | The root or empty path
  root :: p
  -- | Concatenate two paths
  extend :: p -> p -> p
  -- | Extract the lowest common ancestor and the path from the LCA to each argument
  factor :: p -> p -> (p,(p,p))

-- | Compute the lowest common ancestor of two paths
lca :: Path p => p -> p -> p
lca p p2 = fst (factor p p2)

instance Eq a => Path [a] where
  root = []
  extend = (++)
  factor p1 p2 = (take shared p1, (drop shared p1, drop shared p2))
    where shared = length (zipWith (==) p1 p2)

instance Path () where
  root = ()
  extend _ _ = ()
  factor u _ = (u,(u,u))
