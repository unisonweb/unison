{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module U.Core.ABT.Var where

import Data.Set (Set)
import Prelude hiding (abs, cycle)
import U.Core.ABT
import qualified Data.Set as Set

-- | A class for avoiding accidental variable capture
--
--   * `Set.notMember (freshIn vs v) vs`:
--     `freshIn` returns a variable not used in the `Set`
class Ord v => Var v where
  freshIn :: Set v -> v -> v

substsInheritAnnotation
  :: (Foldable f, Functor f, Var v)
  => [(v, Term f v b)]
  -> Term f v a
  -> Term f v a
substsInheritAnnotation replacements body =
  foldr (uncurry substInheritAnnotation) body (reverse replacements)

-- Like `subst`, but the annotation of the replacement is inherited from
-- the previous annotation at each replacement point.
substInheritAnnotation :: (Foldable f, Functor f, Var v)
                       => v -> Term f v b -> Term f v a -> Term f v a
substInheritAnnotation v r =
  subst' (\ann -> const ann <$> r) v (freeVars r)

-- Slightly generalized version of `subst`, the replacement action is handled
-- by the function `replace`, which is given the annotation `a` at the point
-- of replacement. `r` should be the set of free variables contained in the
-- term returned by `replace`. See `substInheritAnnotation` for an example usage.
subst' :: (Foldable f, Functor f, Var v) => (a -> Term f v a) -> v -> Set v -> Term f v a -> Term f v a
subst' replace v r t2@(Term fvs ann body)
  | Set.notMember v fvs = t2 -- subtrees not containing the var can be skipped
  | otherwise = case body of
    Var v' | v == v' -> replace ann -- var match; perform replacement
           | otherwise -> t2 -- var did not match one being substituted; ignore
    Cycle body -> cycle ann (subst' replace v r body)
    Abs x _ | x == v -> t2 -- x shadows v; ignore subtree
    Abs x e -> abs ann x' e'
      where x' = freshIn (fvs `Set.union` r) x
            -- rename x to something that cannot be captured by `r`
            e' = if x /= x' then subst' replace v r (rename x x' e)
                 else subst' replace v r e
    Tm body -> tm ann (fmap (subst' replace v r) body)

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f, Var v) => v -> v -> Term f v a -> Term f v a
rename old new t0@(Term fvs ann t) =
  if Set.notMember old fvs then t0
  else case t of
    Var v -> if v == old then var ann new else t0
    Cycle body -> cycle ann (rename old new body)
    Abs v body ->
      -- v shadows old, so skip this subtree
      if v == old then abs ann v body

      -- the rename would capture new, freshen this Abs
      -- to make that no longer true, then proceed with
      -- renaming `old` to `new`
      else if v == new then
        let v' = freshIn (Set.fromList [new,old] <> freeVars body) v
        in abs ann v' (rename old new (rename v v' body))

      -- nothing special, just rename inside body of Abs
      else abs ann v (rename old new body)
    Tm v -> tm ann (fmap (rename old new) v)
