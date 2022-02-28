-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Unison.ABT
  ( -- * Types
    ABT(..)
  , Term(..)
  , Var(..)

  , V(..)
  , Subst(..)

  -- * Combinators & Traversals
  , fresh
  , unvar
  , freshenS
  , freshInBoth
  , visit
  , visit'
  , visitPure
  , changeVars
  , allVars
  , subterms
  , annotateBound
  , rebuildUp
  , rebuildUp'
  , reannotateUp
  , rewriteDown
  , transform
  , transformM
  , foreachSubterm
  , freeVarOccurrences
  , isFreeIn
  , occurrences
  , extraMap
  , vmap
  , vmapM
  , amap
  , rename
  , renames
  , subst
  , substs
  , substInheritAnnotation
  , substsInheritAnnotation
  , find
  , find'
  , FindAction(..)

  -- * Optics
  , baseFunctor_
  , rewriteDown_

  -- * Safe Term constructors & Patterns
  , annotate
  , annotatedVar
  , var
  , tm
  , tm'
  , abs
  , absChain
  , absChain'
  , abs'
  , absr
  , unabs
  , cycle
  , cycle'
  , cycler
  , pattern Abs'
  , pattern Abs''
  , pattern AbsN'
  , pattern Var'
  , pattern Cycle'
  , pattern CycleA'
  , pattern Tm'

    -- * Algorithms
  , components
  , orderedComponents
  ) where

import Control.Lens (Lens', lens, use, (%%~), (.=))
import Control.Monad.State (MonadState)
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.List hiding (cycle, find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude.Extras (Eq1 (..), Ord1 (..), Show1 (..))
import Unison.Prelude
import qualified Unison.Util.Components as Components
import Prelude hiding (abs, cycle)

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r)
  deriving (Functor, Foldable, Traversable, Generic)


-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term { freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a) }
  deriving Generic

abt_ :: Lens' (Term f v a) (ABT f v (Term f v a))
abt_ = lens out setter
  where
    setter (Term fv a _) abt = Term fv a abt

-- a.k.a. baseFunctor_ :: Traversal' (Term f v a) (f _)
baseFunctor_ ::
  Applicative m =>
  (f (Term f v a) -> m (f (Term f v a))) ->
  Term f v a ->
  m (Term f v a)
baseFunctor_ f t =
  t & abt_ %%~ \case
    Tm fx -> Tm <$> f (fx)
    x -> pure x


-- deriving instance (Data a, Data v, Typeable f, Data (f (Term f v a)), Ord v) => Data (Term f v a)

-- | A class for variables.
--
--   * `Set.notMember (freshIn vs v) vs`:
--     `freshIn` returns a variable not used in the `Set`
class Ord v => Var v where
  freshIn :: Set v -> v -> v

data V v = Free v | Bound v deriving (Eq,Ord,Show,Functor)

unvar :: V v -> v
unvar (Free v) = v
unvar (Bound v) = v

instance Var v => Var (V v) where
  freshIn s v = freshIn (Set.map unvar s) <$> v

wrap :: (Functor f, Foldable f, Var v) => v -> Term f (V v) a -> (V v, Term f (V v) a)
wrap v t =
  if Set.member (Free v) (freeVars t)
  then let v' = fresh t (Bound v) in (v', rename (Bound v) v' t)
  else (Bound v, t)

wrap' :: (Functor f, Foldable f, Var v)
      => v -> Term f (V v) a -> (V v -> Term f (V v) a -> c) -> c
wrap' v t f = uncurry f (wrap v t)

-- Annotate the tree with the set of bound variables at each node.
annotateBound :: (Ord v, Foldable f, Functor f) => Term f v a -> Term f v (a, Set v)
annotateBound = go Set.empty where
  go bound t = let a = (annotation t, bound) in case out t of
    Var v -> annotatedVar a v
    Cycle body -> cycle' a (go bound body)
    Abs x body -> abs' a x (go (Set.insert x bound) body)
    Tm body -> tm' a (go bound <$> body)

-- | `True` if `v` is a member of the set of free variables of `t`
isFreeIn :: Ord v => v -> Term f v a -> Bool
isFreeIn v t = Set.member v (freeVars t)

-- | Replace the annotation with the given argument.
annotate :: a -> Term f v a -> Term f v a
annotate a (Term fvs _ out) = Term fvs a out

vmap :: (Functor f, Foldable f, Ord v2) => (v -> v2) -> Term f v a -> Term f v2 a
vmap f (Term _ a out) = case out of
  Var v -> annotatedVar a (f v)
  Tm fa -> tm' a (fmap (vmap f) fa)
  Cycle r -> cycle' a (vmap f r)
  Abs v body -> abs' a (f v) (vmap f body)

vmapM :: (Applicative m, Traversable f, Foldable f, Ord v2) => (v -> m v2) -> Term f v a -> m (Term f v2 a)
vmapM f (Term _ a out) = case out of
  Var v -> annotatedVar a <$> f v
  Tm fa -> tm' a <$> traverse (vmapM f) fa
  Cycle r -> cycle' a <$> vmapM f r
  Abs v body -> abs' a <$> f v <*> vmapM f body

amap :: (Functor f, Foldable f, Ord v) => (a -> a2) -> Term f v a -> Term f v a2
amap = amap' . const

amap' :: (Functor f, Foldable f, Ord v) => (Term f v a -> a -> a2) -> Term f v a -> Term f v a2
amap' f t@(Term _ a out) = case out of
  Var v -> annotatedVar (f t a) v
  Tm fa -> tm' (f t a) (fmap (amap' f) fa)
  Cycle r -> cycle' (f t a) (amap' f r)
  Abs v body -> abs' (f t a) v (amap' f body)

-- | Modifies the annotations in this tree
instance Functor f => Functor (Term f v) where
  fmap f (Term fvs a sub) = Term fvs (f a) (fmap (fmap f) sub)

extraMap :: Functor g => (forall k . f k -> g k) -> Term f v a -> Term g v a
extraMap p (Term fvs a sub) = Term fvs a (go p sub) where
  go :: Functor g => (forall k . f k -> g k) -> ABT f v (Term f v a) -> ABT g v (Term g v a)
  go p = \case
    Var v -> Var v
    Cycle r -> Cycle (extraMap p r)
    Abs v r -> Abs v (extraMap p r)
    Tm x -> Tm (fmap (extraMap p) (p x))

pattern Var' v <- Term _ _ (Var v)
pattern Cycle' vs t <- Term _ _ (Cycle (AbsN' vs (Tm' t)))
pattern Abs'' v body <- Term _ _ (Abs v body)
pattern Abs' subst <- (unabs1 -> Just subst)
pattern AbsN' vs body <- (unabs -> (vs, body))
{-# COMPLETE AbsN' #-}
pattern Tm' f <- Term _ _ (Tm f)
pattern CycleA' a avs t <- Term _ a (Cycle (AbsNA' avs t))
pattern AbsNA' avs body <- (unabsA -> (avs, body))
{-# COMPLETE Var', Cycle', Abs'', Tm' #-}

unabsA :: Term f v a -> ([(a,v)], Term f v a)
unabsA (Term _ a (Abs hd body)) =
  let (tl, body') = unabsA body in ((a,hd) : tl, body')
unabsA t = ([], t)

var :: v -> Term f v ()
var = annotatedVar ()

annotatedVar :: a -> v -> Term f v a
annotatedVar a v = Term (Set.singleton v) a (Var v)

abs :: Ord v => v -> Term f v () -> Term f v ()
abs = abs' ()

abs' :: Ord v => a -> v -> Term f v a -> Term f v a
abs' a v body = Term (Set.delete v (freeVars body)) a (Abs v body)

absr :: (Functor f, Foldable f, Var v) => v -> Term f (V v) () -> Term f (V v) ()
absr = absr' ()

-- | Rebuild an `abs`, renaming `v` to avoid capturing any `Free v` in `body`.
absr' :: (Functor f, Foldable f, Var v) => a -> v -> Term f (V v) a -> Term f (V v) a
absr' a v body = wrap' v body $ \v body -> abs' a v body

absChain :: Ord v => [v] -> Term f v () -> Term f v ()
absChain vs t = foldr abs t vs

absChain' :: Ord v => [(a, v)] -> Term f v a -> Term f v a
absChain' vs t = foldr (\(a,v) t -> abs' a v t) t vs

tm :: (Foldable f, Ord v) => f (Term f v ()) -> Term f v ()
tm = tm' ()

tm' :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm' a t =
  Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)

cycle :: Term f v () -> Term f v ()
cycle = cycle' ()

cycle' :: a -> Term f v a -> Term f v a
cycle' a t = Term (freeVars t) a (Cycle t)

cycler' :: (Functor f, Foldable f, Var v) => a -> [v] -> Term f (V v) a -> Term f (V v) a
cycler' a vs t = cycle' a $ foldr (absr' a) t vs

cycler :: (Functor f, Foldable f, Var v) => [v] -> Term f (V v) () -> Term f (V v) ()
cycler = cycler' ()

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f, Var v) => v -> v -> Term f v a -> Term f v a
rename old new t0@(Term fvs ann t) =
  if Set.notMember old fvs then t0
  else case t of
    Var v -> if v == old then annotatedVar ann new else t0
    Cycle body -> cycle' ann (rename old new body)
    Abs v body ->
      -- v shadows old, so skip this subtree
      if v == old then abs' ann v body

      -- the rename would capture new, freshen this Abs
      -- to make that no longer true, then proceed with
      -- renaming `old` to `new`
      else if v == new then
        let v' = freshIn (Set.fromList [new,old] <> freeVars body) v
        in abs' ann v' (rename old new (rename v v' body))

      -- nothing special, just rename inside body of Abs
      else abs' ann v (rename old new body)
    Tm v -> tm' ann (fmap (rename old new) v)

renames
  :: (Foldable f, Functor f, Var v)
  => Map v v -> Term f v a -> Term f v a
renames rn0 t0@(Term fvs ann t)
  | Map.null rn = t0
  | Var v <- t
  , Just u <- Map.lookup v rn
  = annotatedVar ann u
  | Cycle body <- t
  = cycle' ann (renames rn body)
  | Abs v t <- t
  -- rename iterated variables all at once to avoid a capture issue
  , AbsNA' (unzip -> (as,vs)) body <- t
  , (rn, us) <- mangle (freeVars body) rn (v:vs)
  , not $ Map.null rn
  = absChain' (zip (ann:as) us) (renames rn body)
  | Tm body <- t
  = tm' ann (renames rn <$> body)
  | otherwise = t0
  where
  rn = Map.restrictKeys rn0 fvs
  mangle fvs m vs
    = let avs = Set.fromList vs <> fvs
      in mapAccumL (mangle1 avs) m vs
  mangle1 avs m v
    | any (==v) vs
    , u <- freshIn (avs <> Set.fromList vs) v
    = (Map.insert v u m, u)
    | otherwise = (Map.delete v m, v)
    where
    vs = toList m

-- Note: this does not do capture-avoiding renaming. It actually
-- renames bound variables using the map as well.
changeVars :: (Foldable f, Functor f, Var v) => Map v v -> Term f v a -> Term f v a
changeVars m t = case out t of
  Abs v body -> case Map.lookup v m of
    Nothing -> abs' (annotation t) v (changeVars m body)
    Just v' -> abs' (annotation t) v' (changeVars m body)
  Cycle body -> cycle' (annotation t) (changeVars m body)
  Var v -> case Map.lookup v m of
    Nothing -> t
    Just v -> annotatedVar (annotation t) v
  Tm v -> tm' (annotation t) (changeVars m <$> v)

-- | Produce a variable which is free in both terms
freshInBoth :: Var v => Term f v a -> Term f v a -> v -> v
freshInBoth t1 t2 = freshIn $ Set.union (freeVars t1) (freeVars t2)

fresh :: Var v => Term f v a -> v -> v
fresh t = freshIn (freeVars t)

allVars :: Foldable f => Term f v a -> [v]
allVars t = case out t of
  Var v -> [v]
  Cycle body -> allVars body
  Abs v body -> v : allVars body
  Tm v -> Foldable.toList v >>= allVars

-- | Freshens the given variable wrt. the set of used variables
-- tracked by state. Adds the result to the set of used variables.
freshenS :: (Var v, MonadState (Set v) m) => v -> m v
freshenS = freshenS' id

-- | A more general version of `freshenS` that uses a lens
-- to focus on used variables inside state.
freshenS' :: (Var v, MonadState s m) => Lens' s (Set v) -> v -> m v
freshenS' uvLens v = do
  usedVars <- use uvLens
  let v' = freshIn usedVars v
  uvLens .= Set.insert v' usedVars
  pure v'

-- | `subst v e body` substitutes `e` for `v` in `body`, avoiding capture by
-- renaming abstractions in `body`
subst
  :: (Foldable f, Functor f, Var v)
  => v
  -> Term f v a
  -> Term f v a
  -> Term f v a
subst v r = subst' (const r) v (freeVars r)

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
    Cycle body -> cycle' ann (subst' replace v r body)
    Abs x _ | x == v -> t2 -- x shadows v; ignore subtree
    Abs x e -> abs' ann x' e'
      where x' = freshIn (fvs `Set.union` r) x
            -- rename x to something that cannot be captured by `r`
            e' = if x /= x' then subst' replace v r (rename x x' e)
                 else subst' replace v r e
    Tm body -> tm' ann (fmap (subst' replace v r) body)

-- Like `subst`, but the annotation of the replacement is inherited from
-- the previous annotation at each replacement point.
substInheritAnnotation :: (Foldable f, Functor f, Var v)
                       => v -> Term f v b -> Term f v a -> Term f v a
substInheritAnnotation v r =
  subst' (\ann -> const ann <$> r) v (freeVars r)

substsInheritAnnotation
  :: (Foldable f, Functor f, Var v)
  => [(v, Term f v b)]
  -> Term f v a
  -> Term f v a
substsInheritAnnotation replacements body =
  foldr (uncurry substInheritAnnotation) body (reverse replacements)

-- | `substs [(t1,v1), (t2,v2), ...] body` performs multiple simultaneous
-- substitutions, avoiding capture
substs
  :: (Foldable f, Functor f, Var v)
  => [(v, Term f v a)]
  -> Term f v a
  -> Term f v a
substs replacements body = foldr (uncurry subst) body (reverse replacements)

-- Count the number times the given variable appears free in the term
occurrences :: (Foldable f, Var v) => v -> Term f v a -> Int
occurrences v t | not (v `isFreeIn` t) = 0
occurrences v t = case out t of
  Var v2 -> if v == v2 then 1 else 0
  Cycle t -> occurrences v t
  Abs v2 t -> if v == v2 then 0 else occurrences v t
  Tm t -> foldl' (\s t -> s + occurrences v t) 0 $ Foldable.toList t

rebuildUp :: (Ord v, Foldable f, Functor f)
          => (f (Term f v a) -> f (Term f v a))
          -> Term f v a
          -> Term f v a
rebuildUp f (Term _ ann body) = case body of
  Var v -> annotatedVar ann v
  Cycle body -> cycle' ann (rebuildUp f body)
  Abs x e -> abs' ann x (rebuildUp f e)
  Tm body -> tm' ann (f $ fmap (rebuildUp f) body)

rebuildUp' :: (Ord v, Foldable f, Functor f)
          => (Term f v a -> Term f v a)
          -> Term f v a
          -> Term f v a
rebuildUp' f (Term _ ann body) = case body of
  Var v -> f (annotatedVar ann v)
  Cycle body -> f $ cycle' ann (rebuildUp' f body)
  Abs x e -> f $ abs' ann x (rebuildUp' f e)
  Tm body -> f $ tm' ann (fmap (rebuildUp' f) body)

freeVarOccurrences :: (Traversable f, Ord v) => Set v -> Term f v a -> [(v, a)]
freeVarOccurrences except t =
  [ (v, a) | (v,a) <- go $ annotateBound t, not (Set.member v except) ]
  where
  go e = case out e of
    Var v -> if Set.member v (snd $ annotation e)
             then []
             else [(v, fst $ annotation e)]
    Cycle body -> go body
    Abs _ body -> go body
    Tm body -> foldMap go body

foreachSubterm
  :: (Traversable f, Applicative g)
  => (Term f v a -> g b)
  -> Term f v a
  -> g [b]
foreachSubterm f e = case out e of
  Var   _    -> pure <$> f e
  Cycle body -> (:) <$> f e <*> foreachSubterm f body
  Abs _ body -> (:) <$> f e <*> foreachSubterm f body
  Tm body ->
    (:)
      <$> f e
      <*> (join . Foldable.toList <$> traverse (foreachSubterm f) body)

subterms :: (Traversable f) => Term f v a -> [Term f v a]
subterms t = runIdentity $ foreachSubterm pure t

-- subterms_ :: (Traversable f) => Fold (Term f v a) (Term f v a)
-- subterms_ = folding subterms

-- | `visit f t` applies an effectful function to each subtree of
-- `t` and sequences the results. When `f` returns `Nothing`, `visit`
-- descends into the children of the current subtree. When `f` returns
-- `Just t2`, `visit` replaces the current subtree with `t2`. Thus:
-- `visit (const Nothing) t == pure t` and
-- `visit (const (Just (pure t2))) t == pure t2`
visit
  :: (Traversable f, Applicative g, Ord v)
  => (Term f v a -> Maybe (g (Term f v a)))
  -> Term f v a
  -> g (Term f v a)
visit f t = flip fromMaybe (f t) $ case out t of
  Var   _    -> pure t
  Cycle body -> cycle' (annotation t) <$> visit f body
  Abs x e    -> abs' (annotation t) x <$> visit f e
  Tm body    -> tm' (annotation t) <$> traverse (visit f) body

-- subTermsSetter_ :: (Traversable f, Ord v) => Setter' (Term f v a) (Term f v a)
-- subTermsSetter_ f tm = visit (Just . f) tm

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' :: (Traversable f, Monad g, Ord v)
       => (f (Term f v a) -> g (f (Term f v a)))
       -> Term f v a
       -> g (Term f v a)
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle' (annotation t) <$> visit' f body
  Abs x e -> abs' (annotation t) x <$> visit' f e
  Tm body -> f body >>= (fmap (tm' (annotation t)) . traverse (visit' f))

-- | `visit` specialized to the `Identity` effect.
visitPure :: (Traversable f, Ord v)
      => (Term f v a -> Maybe (Term f v a)) -> Term f v a -> Term f v a
visitPure f = runIdentity . visit (fmap pure . f)

rewriteDown :: (Traversable f, Ord v)
            => (Term f v a -> Term f v a)
            -> Term f v a
            -> Term f v a
rewriteDown f tm = runIdentity $ rewriteDown_ (Identity . f) tm


-- | Setter' (Term f v a) (Term f v a)
rewriteDown_ :: (Traversable f, Monad m, Ord v)
            => (Term f v a -> m (Term f v a))
            -> Term f v a
            -> m (Term f v a)
rewriteDown_ f t = do
  t' <- f t
  case out t' of
    Var v -> pure (annotatedVar (annotation t') v)
    Cycle body -> cycle' (annotation t') <$> rewriteDown_ f body
    Abs x e -> abs' (annotation t') x <$> rewriteDown_ f e
    Tm body -> tm' (annotation t') <$> (traverse (rewriteDown_ f) body)

data Subst f v a =
  Subst { freshen :: forall m v' . Monad m => (v -> m v') -> m v'
        , bind :: Term f v a -> Term f v a
        , bindInheritAnnotation :: forall b . Term f v b -> Term f v a
        , variable :: v }

unabs1 :: forall a f v. (Foldable f, Functor f, Var v) => Term f v a -> Maybe (Subst f v a)
unabs1 (Term _ _ (Abs v body)) = Just (Subst freshen bind bindInheritAnnotation v) where
  freshen :: (v -> t) -> t
  freshen f = f v
  bind :: Term f v a -> Term f v a
  bind x = subst v x body
  bindInheritAnnotation :: Term f v b -> Term f v a
  bindInheritAnnotation x = substInheritAnnotation v x body
unabs1 _ = Nothing

unabs :: Term f v a -> ([v], Term f v a)
unabs (Term _ _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)

transform :: (Ord v, Foldable g, Functor f)
          => (forall a. f a -> g a) -> Term f v a -> Term g v a
transform f tm = case out tm of
  Var v -> annotatedVar (annotation tm) v
  Abs v body -> abs' (annotation tm) v (transform f body)
  Tm subterms ->
    let subterms' = fmap (transform f) subterms
    in tm' (annotation tm) (f subterms')
  Cycle body -> cycle' (annotation tm) (transform f body)

transformM :: (Ord v, Monad m, Traversable g)
          => (forall a. f a -> m (g a)) -> Term f v a -> m (Term g v a)
transformM f t = case out t of
  Var v -> pure $ annotatedVar (annotation t) v
  Abs v body -> abs' (annotation t) v <$> (transformM f body)
  Tm subterms -> tm' (annotation t) <$> (traverse (transformM f) =<< f subterms)
  Cycle body -> cycle' (annotation t) <$> (transformM f body)

-- Rebuild the tree annotations upward, starting from the leaves,
-- using the Monoid to choose the annotation at intermediate nodes
reannotateUp :: (Ord v, Foldable f, Functor f, Monoid b)
  => (Term f v a -> b)
  -> Term f v a
  -> Term f v (a, b)
reannotateUp g t = case out t of
  Var v -> annotatedVar (annotation t, g t) v
  Cycle body ->
    let body' = reannotateUp g body
    in cycle' (annotation t, snd (annotation body')) body'
  Abs v body ->
    let body' = reannotateUp g body
    in abs' (annotation t, snd (annotation body')) v body'
  Tm body ->
    let
      body' = reannotateUp g <$> body
      ann = g t <> foldMap (snd . annotation) body'
    in tm' (annotation t, ann) body'

-- Find all subterms that match a predicate.  Prune the search for speed.
-- (Some patterns of pruning can cut the complexity of the search.)
data FindAction x = Found x | Prune | Continue
  deriving (Show, Functor)
find :: (Ord v, Foldable f, Functor f)
  => (Term f v a -> FindAction x)
  -> Term f v a
  -> [x]
find p t = case p t of
    Found x -> x : go
    Prune -> []
    Continue -> go
  where go = case out t of
          Var _ -> []
          Cycle body -> Unison.ABT.find p body
          Abs _ body -> Unison.ABT.find p body
          Tm body -> Foldable.concat (Unison.ABT.find p <$> body)

find' :: (Ord v, Foldable f, Functor f)
  => (Term f v a -> Bool)
  -> Term f v a
  -> [Term f v a]
find' p = Unison.ABT.find (\t -> if p t then Found t else Continue)

instance (Foldable f, Functor f, Eq1 f, Var v) => Eq (Term f v a) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 == t2 = go (out t1) (out t2) where
    go (Var v) (Var v2) | v == v2 = True
    go (Cycle t1) (Cycle t2) = t1 == t2
    go (Abs v1 body1) (Abs v2 body2) =
      if v1 == v2 then body1 == body2
      else let v3 = freshInBoth body1 body2 v1
           in rename v1 v3 body1 == rename v2 v3 body2
    go (Tm f1) (Tm f2) = f1 ==# f2
    go _ _ = False

instance (Foldable f, Functor f, Ord1 f, Var v) => Ord (Term f v a) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 `compare` t2 = go (out t1) (out t2) where
    go (Var v) (Var v2) = v `compare` v2
    go (Cycle t1) (Cycle t2) = t1 `compare` t2
    go (Abs v1 body1) (Abs v2 body2) =
      if v1 == v2 then body1 `compare` body2
      else let v3 = freshInBoth body1 body2 v1
           in rename v1 v3 body1 `compare` rename v2 v3 body2
    go (Tm f1) (Tm f2) = compare1 f1 f2
    go t1 t2 = tag t1 `compare` tag t2
    tag (Var _) = 0 :: Word
    tag (Tm _) = 1
    tag (Abs _ _) = 2
    tag (Cycle _) = 3

components :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
components = Components.components freeVars

-- Converts to strongly connected components while preserving the
-- order of definitions. Satisfies `join (orderedComponents bs) == bs`.
orderedComponents' :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
orderedComponents' tms = go [] Set.empty tms
  where
  go [] _ [] = []
  go [] deps (hd:rem) = go [hd] (deps <> freeVars (snd hd)) rem
  go cur deps rem = case findIndex isDep rem of
    Nothing -> reverse cur : let (hd,tl) = splitAt 1 rem
                             in go hd (depsFor hd) tl
    Just i  -> go (reverse newMembers ++ cur) deps' (drop (i+1) rem)
               where deps' = deps <> depsFor newMembers
                     newMembers = take (i+1) rem
    where
    depsFor = foldMap (freeVars . snd)
    isDep (v, _) = Set.member v deps

-- Like `orderedComponents'`, but further break up cycles and move
-- cyclic subcycles before other components in the same cycle.
-- Tweak suggested by @aryairani.
--
-- Example: given `[[x],[ping,r,s,pong]]`, where `ping` and `pong`
-- are mutually recursive but `r` and `s` are uninvolved, this produces:
-- `[[x], [ping,pong], [r], [s]]`.
orderedComponents :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
orderedComponents bs0 = tweak =<< orderedComponents' bs0 where
  tweak :: Var v => [(v,Term f v a)] -> [[(v,Term f v a)]]
  tweak bs@(_:_:_) = case takeWhile isCyclic (components bs) of
    [] -> [bs]
    cycles -> cycles <> orderedComponents rest
      where
      rest = [ (v,b) | (v,b) <- bs, Set.notMember v cycleVars ]
      cycleVars = Set.fromList (fst <$> join cycles)
  tweak bs = [bs] -- any cycle with < 2 bindings is left alone
  isCyclic [(v,b)] = Set.member v (freeVars b)
  isCyclic bs      = length bs > 1

instance (Show1 f, Show v) => Show (Term f v a) where
  -- annotations not shown
  showsPrec p (Term _ _ out) = case out of
    Var v -> showParen (p>=9) $ \x -> "Var " ++ show v ++ x
    Cycle body -> ("Cycle " ++) . showsPrec p body
    Abs v body -> showParen True $ (show v ++) . showString ". " . showsPrec p body
    Tm f -> showsPrec1 p f
