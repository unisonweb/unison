{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.ABT
  ( -- * Types
    ABT (..),
    Term (..),
    Var (..),
    V (..),
    Subst (..),

    -- * Combinators & Traversals
    fresh,
    unvar,
    freshenS,
    freshInBoth,
    visit,
    visit',
    visitPure,
    changeVars,
    allVars,
    subterms,
    annotateBound,
    rebuildUp,
    rebuildUp',
    reannotateUp,
    rewriteDown,
    transform,
    transformM,
    foreachSubterm,
    freeVarOccurrences,
    isFreeIn,
    occurrences,
    extraMap,
    vmap,
    vmapM,
    amap,
    rename,
    renames,
    subst,
    substs,
    substInheritAnnotation,
    substsInheritAnnotation,
    find,
    find',
    FindAction (..),

    -- * Optics
    baseFunctor_,
    rewriteDown_,

    -- * Safe Term constructors & Patterns
    annotate,
    annotatedVar,
    var,
    tm,
    tm',
    abs,
    absChain,
    absChain',
    abs',
    absr,
    unabs,
    cycle,
    cycle',
    cycler,
    pattern Abs',
    pattern Abs'',
    pattern AbsN',
    pattern Var',
    pattern Cycle',
    pattern CycleA',
    pattern Tm',

    -- * Algorithms
    components,
    orderedComponents,
  )
where

import Control.Lens (Lens', lens, use, (%%~), (.=))
import Control.Monad.State (MonadState)
import qualified Data.Foldable as Foldable
import Data.List hiding (cycle, find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import U.Core.ABT
  ( ABT (..),
    Term (..),
    foreachSubterm,
    freshInBoth,
    rename,
    subst',
    substInheritAnnotation,
    substsInheritAnnotation,
    subterms,
    transform,
    transformM,
    unabs,
    visit',
    visit,
    visitPure,
    vmap,
    pattern AbsN',
    pattern Tm',
    pattern Var',
  )
import qualified U.Core.ABT
import U.Core.ABT.Var (Var (freshIn))
import Unison.Prelude
import qualified Unison.Util.Components as Components
import Prelude hiding (abs, cycle)

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

data V v = Free v | Bound v deriving (Eq, Ord, Show, Functor)

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

wrap' ::
  (Functor f, Foldable f, Var v) =>
  v ->
  Term f (V v) a ->
  (V v -> Term f (V v) a -> c) ->
  c
wrap' v t f = uncurry f (wrap v t)

-- Annotate the tree with the set of bound variables at each node.
annotateBound :: (Ord v, Foldable f, Functor f) => Term f v a -> Term f v (a, Set v)
annotateBound = go Set.empty
  where
    go bound t =
      let a = (annotation t, bound)
       in case out t of
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

-- amap :: (Functor f, Foldable f) => (a -> a') -> Term f v a -> Term f v a'
-- amap f (Term fv a out) = Term fv (f a) $ case out of
--   Var v -> Var v
--   Tm fa -> Tm (amap f <$> fa)
--   Cycle r -> Cycle (amap f r)
--   Abs v body -> Abs v (amap f body)

extraMap :: Functor g => (forall k. f k -> g k) -> Term f v a -> Term g v a
extraMap p (Term fvs a sub) = Term fvs a (go p sub)
  where
    go :: Functor g => (forall k. f k -> g k) -> ABT f v (Term f v a) -> ABT g v (Term g v a)
    go p = \case
      Var v -> Var v
      Cycle r -> Cycle (extraMap p r)
      Abs v r -> Abs v (extraMap p r)
      Tm x -> Tm (fmap (extraMap p) (p x))

pattern Cycle' vs t <- Term _ _ (Cycle (AbsN' vs (Tm' t)))

pattern Abs'' v body <- Term _ _ (Abs v body)

pattern Abs' subst <- (unabs1 -> Just subst)

pattern CycleA' a avs t <- Term _ a (Cycle (AbsNA' avs t))

pattern AbsNA' avs body <- (unabsA -> (avs, body))

{-# COMPLETE Var', Cycle', Abs'', Tm' #-}

unabsA :: Term f v a -> ([(a, v)], Term f v a)
unabsA (Term _ a (Abs hd body)) =
  let (tl, body') = unabsA body in ((a, hd) : tl, body')
unabsA t = ([], t)

var :: v -> Term f v ()
var = annotatedVar ()

annotatedVar :: a -> v -> Term f v a
annotatedVar = U.Core.ABT.var

abs :: Ord v => v -> Term f v () -> Term f v ()
abs = abs' ()

abs' :: Ord v => a -> v -> Term f v a -> Term f v a
abs' = U.Core.ABT.abs

absr :: (Functor f, Foldable f, Var v) => v -> Term f (V v) () -> Term f (V v) ()
absr = absr' ()

-- | Rebuild an `abs`, renaming `v` to avoid capturing any `Free v` in `body`.
absr' :: (Functor f, Foldable f, Var v) => a -> v -> Term f (V v) a -> Term f (V v) a
absr' a v body = wrap' v body $ \v body -> abs' a v body

absChain :: Ord v => [v] -> Term f v () -> Term f v ()
absChain vs t = foldr abs t vs

absChain' :: Ord v => [(a, v)] -> Term f v a -> Term f v a
absChain' vs t = foldr (\(a, v) t -> abs' a v t) t vs

tm :: (Foldable f, Ord v) => f (Term f v ()) -> Term f v ()
tm = tm' ()

tm' :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm' = U.Core.ABT.tm

cycle :: Term f v () -> Term f v ()
cycle = cycle' ()

cycle' :: a -> Term f v a -> Term f v a
cycle' = U.Core.ABT.cycle

cycler' :: (Functor f, Foldable f, Var v) => a -> [v] -> Term f (V v) a -> Term f (V v) a
cycler' a vs t = cycle' a $ foldr (absr' a) t vs

cycler :: (Functor f, Foldable f, Var v) => [v] -> Term f (V v) () -> Term f (V v) ()
cycler = cycler' ()

renames ::
  (Foldable f, Functor f, Var v) =>
  Map v v ->
  Term f v a ->
  Term f v a
renames rn0 t0@(Term fvs ann t)
  | Map.null rn = t0
  | Var v <- t,
    Just u <- Map.lookup v rn =
      annotatedVar ann u
  | Cycle body <- t =
      cycle' ann (renames rn body)
  | Abs v t <- t,
    -- rename iterated variables all at once to avoid a capture issue
    AbsNA' (unzip -> (as, vs)) body <- t,
    (rn, us) <- mangle (freeVars body) rn (v : vs),
    not $ Map.null rn =
      absChain' (zip (ann : as) us) (renames rn body)
  | Tm body <- t =
      tm' ann (renames rn <$> body)
  | otherwise = t0
  where
    rn = Map.restrictKeys rn0 fvs
    mangle fvs m vs =
      let avs = Set.fromList vs <> fvs
       in mapAccumL (mangle1 avs) m vs
    mangle1 avs m v
      | any (== v) vs,
        u <- freshIn (avs <> Set.fromList vs) v =
          (Map.insert v u m, u)
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
subst ::
  (Foldable f, Functor f, Var v) =>
  v ->
  Term f v a ->
  Term f v a ->
  Term f v a
subst v r = subst' (const r) v (freeVars r)

-- | `substs [(t1,v1), (t2,v2), ...] body` performs multiple simultaneous
-- substitutions, avoiding capture
substs ::
  (Foldable f, Functor f, Var v) =>
  [(v, Term f v a)] ->
  Term f v a ->
  Term f v a
substs replacements body = foldr (uncurry subst) body (reverse replacements)

-- Count the number times the given variable appears free in the term
occurrences :: (Foldable f, Var v) => v -> Term f v a -> Int
occurrences v t | not (v `isFreeIn` t) = 0
occurrences v t = case out t of
  Var v2 -> if v == v2 then 1 else 0
  Cycle t -> occurrences v t
  Abs v2 t -> if v == v2 then 0 else occurrences v t
  Tm t -> foldl' (\s t -> s + occurrences v t) 0 $ Foldable.toList t

rebuildUp ::
  (Ord v, Foldable f, Functor f) =>
  (f (Term f v a) -> f (Term f v a)) ->
  Term f v a ->
  Term f v a
rebuildUp f (Term _ ann body) = case body of
  Var v -> annotatedVar ann v
  Cycle body -> cycle' ann (rebuildUp f body)
  Abs x e -> abs' ann x (rebuildUp f e)
  Tm body -> tm' ann (f $ fmap (rebuildUp f) body)

rebuildUp' ::
  (Ord v, Foldable f, Functor f) =>
  (Term f v a -> Term f v a) ->
  Term f v a ->
  Term f v a
rebuildUp' f (Term _ ann body) = case body of
  Var v -> f (annotatedVar ann v)
  Cycle body -> f $ cycle' ann (rebuildUp' f body)
  Abs x e -> f $ abs' ann x (rebuildUp' f e)
  Tm body -> f $ tm' ann (fmap (rebuildUp' f) body)

freeVarOccurrences :: (Traversable f, Ord v) => Set v -> Term f v a -> [(v, a)]
freeVarOccurrences except t =
  [(v, a) | (v, a) <- go $ annotateBound t, not (Set.member v except)]
  where
    go e = case out e of
      Var v ->
        if Set.member v (snd $ annotation e)
          then []
          else [(v, fst $ annotation e)]
      Cycle body -> go body
      Abs _ body -> go body
      Tm body -> foldMap go body

-- subterms_ :: (Traversable f) => Fold (Term f v a) (Term f v a)
-- subterms_ = folding subterms

-- subTermsSetter_ :: (Traversable f, Ord v) => Setter' (Term f v a) (Term f v a)
-- subTermsSetter_ f tm = visit (Just . f) tm

rewriteDown ::
  (Traversable f, Ord v) =>
  (Term f v a -> Term f v a) ->
  Term f v a ->
  Term f v a
rewriteDown f tm = runIdentity $ rewriteDown_ (Identity . f) tm

-- | Setter' (Term f v a) (Term f v a)
rewriteDown_ ::
  (Traversable f, Monad m, Ord v) =>
  (Term f v a -> m (Term f v a)) ->
  Term f v a ->
  m (Term f v a)
rewriteDown_ f t = do
  t' <- f t
  case out t' of
    Var v -> pure (annotatedVar (annotation t') v)
    Cycle body -> cycle' (annotation t') <$> rewriteDown_ f body
    Abs x e -> abs' (annotation t') x <$> rewriteDown_ f e
    Tm body -> tm' (annotation t') <$> (traverse (rewriteDown_ f) body)

data Subst f v a = Subst
  { freshen :: forall m v'. Monad m => (v -> m v') -> m v',
    bind :: Term f v a -> Term f v a,
    bindInheritAnnotation :: forall b. Term f v b -> Term f v a,
    variable :: v
  }

unabs1 :: forall a f v. (Foldable f, Functor f, Var v) => Term f v a -> Maybe (Subst f v a)
unabs1 (Term _ _ (Abs v body)) = Just (Subst freshen bind bindInheritAnnotation v)
  where
    freshen :: (v -> t) -> t
    freshen f = f v
    bind :: Term f v a -> Term f v a
    bind x = subst v x body
    bindInheritAnnotation :: Term f v b -> Term f v a
    bindInheritAnnotation x = substInheritAnnotation v x body
unabs1 _ = Nothing

-- Rebuild the tree annotations upward, starting from the leaves,
-- using the Monoid to choose the annotation at intermediate nodes
reannotateUp ::
  (Ord v, Foldable f, Functor f, Monoid b) =>
  (Term f v a -> b) ->
  Term f v a ->
  Term f v (a, b)
reannotateUp g t = case out t of
  Var v -> annotatedVar (annotation t, g t) v
  Cycle body ->
    let body' = reannotateUp g body
     in cycle' (annotation t, snd (annotation body')) body'
  Abs v body ->
    let body' = reannotateUp g body
     in abs' (annotation t, snd (annotation body')) v body'
  Tm body ->
    let body' = reannotateUp g <$> body
        ann = g t <> foldMap (snd . annotation) body'
     in tm' (annotation t, ann) body'

-- Find all subterms that match a predicate.  Prune the search for speed.
-- (Some patterns of pruning can cut the complexity of the search.)
data FindAction x = Found x | Prune | Continue
  deriving (Show, Functor)

find ::
  (Ord v, Foldable f, Functor f) =>
  (Term f v a -> FindAction x) ->
  Term f v a ->
  [x]
find p t = case p t of
  Found x -> x : go
  Prune -> []
  Continue -> go
  where
    go = case out t of
      Var _ -> []
      Cycle body -> Unison.ABT.find p body
      Abs _ body -> Unison.ABT.find p body
      Tm body -> Foldable.concat (Unison.ABT.find p <$> body)

find' ::
  (Ord v, Foldable f, Functor f) =>
  (Term f v a -> Bool) ->
  Term f v a ->
  [Term f v a]
find' p = Unison.ABT.find (\t -> if p t then Found t else Continue)

components :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
components = Components.components freeVars

-- Converts to strongly connected components while preserving the
-- order of definitions. Satisfies `join (orderedComponents bs) == bs`.
orderedComponents' :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
orderedComponents' tms = go [] Set.empty tms
  where
    go [] _ [] = []
    go [] deps (hd : rem) = go [hd] (deps <> freeVars (snd hd)) rem
    go cur deps rem = case findIndex isDep rem of
      Nothing ->
        reverse cur :
        let (hd, tl) = splitAt 1 rem
         in go hd (depsFor hd) tl
      Just i -> go (reverse newMembers ++ cur) deps' (drop (i + 1) rem)
        where
          deps' = deps <> depsFor newMembers
          newMembers = take (i + 1) rem
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
orderedComponents bs0 = tweak =<< orderedComponents' bs0
  where
    tweak :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
    tweak bs@(_ : _ : _) = case takeWhile isCyclic (components bs) of
      [] -> [bs]
      cycles -> cycles <> orderedComponents rest
        where
          rest = [(v, b) | (v, b) <- bs, Set.notMember v cycleVars]
          cycleVars = Set.fromList (fst <$> join cycles)
    tweak bs = [bs] -- any cycle with < 2 bindings is left alone
    isCyclic [(v, b)] = Set.member v (freeVars b)
    isCyclic bs = length bs > 1
