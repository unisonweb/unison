-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.ABT where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity (runIdentity)
import Data.List hiding (cycle)
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable
import Data.Vector ((!))
import Prelude hiding (abs,cycle)
import Prelude.Extras (Eq1(..), Show1(..))
import Unison.Hashable (Accumulate,Hashable1)
import Unison.Var (Var)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Unison.Hashable as Hashable
import qualified Unison.Var as Var

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r) deriving (Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term { freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a) }

data V v = Free v | Bound v deriving (Eq,Ord,Show,Functor)

unvar :: V v -> v
unvar (Free v) = v
unvar (Bound v) = v

instance Var v => Var (V v) where
  rename n2 = fmap (Var.rename n2)
  named txt = Bound (Var.named txt)
  name v = Var.name (unvar v)
  qualifiedName v = Var.qualifiedName (unvar v)
  freshIn s v = Var.freshIn (Set.map unvar s) <$> v
  freshenId id v = Var.freshenId id <$> v
  clear v = Var.clear <$> v

data Path s t a b m = Path { focus :: s -> Maybe (a, b -> Maybe t, m) }

here :: Monoid m => Path s t s t m
here = Path $ \s -> Just (s, Just, mempty)

instance Semigroup (Path s t a b m) where
  (<>) = mappend

instance Monoid (Path s t a b m) where
  mempty = Path (const Nothing)
  mappend (Path p1) (Path p2) = Path p3 where
    p3 s = p1 s <|> p2 s

type Path' f g m = forall a v . Var v => Path (Term f v a) (Term f (V v) a) (Term g v a) (Term g (V v) a) m

compose :: Monoid m => Path s t a b m -> Path a b a' b' m -> Path s t a' b' m
compose (Path p1) (Path p2) = Path p3 where
  p3 s = do
    (get1,set1,m1) <- p1 s
    (get2,set2,m2) <- p2 get1
    pure (get2, \i -> set2 i >>= set1, m1 `mappend` m2)

at :: Path s t a b m -> s -> Maybe a
at p s = (\(a,_,_) -> a) <$> focus p s

modify' :: Path s t a b m -> (m -> a -> b) -> s -> Maybe t
modify' p f s = focus p s >>= \(get,set,m) -> set (f m get)

wrap :: (Functor f, Foldable f, Var v) => v -> Term f (V v) a -> (V v, Term f (V v) a)
wrap v t =
  if Set.member (Free v) (freeVars t)
  then let v' = fresh t (Bound v) in (v', rename (Bound v) v' t)
  else (Bound v, t)

wrap' :: (Functor f, Foldable f, Var v)
      => v -> Term f (V v) a -> (V v -> Term f (V v) a -> c) -> c
wrap' v t f = uncurry f (wrap v t)

-- | Return the list of all variables bound by this ABT
bound' :: Foldable f => Term f v a -> [v]
bound' t = case out t of
  Abs v t -> v : bound' t
  Cycle t -> bound' t
  Tm f -> Foldable.toList f >>= bound'
  _ -> []

-- | Return the list of all variables bound by this ABT
bound :: (Ord v, Foldable f) => Term f v a -> Set v
bound t = Set.fromList (bound' t)

-- | `True` if the term has no free variables, `False` otherwise
isClosed :: Term f v a -> Bool
isClosed t = Set.null (freeVars t)

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

-- | Modifies the annotations in this tree
instance Functor f => Functor (Term f v) where
  fmap f (Term fvs a sub) = Term fvs (f a) (fmap (fmap f) sub)

pattern Var' v <- Term _ _ (Var v)
pattern Cycle' vs t <- Term _ _ (Cycle (AbsN' vs t))
-- pattern Abs' v body <- Term _ _ (Abs v body)
pattern Abs' subst <- (unabs1 -> Just subst)
pattern AbsN' vs body <- (unabs -> (vs, body))
pattern Tm' f <- Term _ _ (Tm f)
pattern CycleA' a avs t <- Term _ a (Cycle (AbsNA' avs t))
pattern AbsNA' avs body <- (unabsA -> (avs, body))

unabsA :: Term f v a -> ([(a,v)], Term f v a)
unabsA (Term _ a (Abs hd body)) =
  let (tl, body') = unabsA body in ((a,hd) : tl, body')
unabsA t = ([], t)

v' :: Var v => Text -> v
v' = Var.named

var :: v -> Term f v ()
var = annotatedVar ()

var' :: Var v => Text -> Term f v ()
var' v = var (Var.named v)

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

into :: (Foldable f, Ord v) => ABT f v (Term f v ()) -> Term f v ()
into = into' ()

into' :: (Foldable f, Ord v) => a -> ABT f v (Term f v a) -> Term f v a
into' a abt = case abt of
  Var x -> annotatedVar a x
  Cycle t -> cycle' a t
  Abs v r -> abs' a v r
  Tm t -> tm' a t

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f, Var v) => v -> v -> Term f v a -> Term f v a
rename old new t0@(Term _ ann t) = case t of
  Var v -> if v == old then annotatedVar ann new else t0
  Cycle body -> cycle' ann (rename old new body)
  Abs v body -> if v == old then abs' ann v body
                else abs' ann v (rename old new body)
  Tm v -> tm' ann (fmap (rename old new) v)

-- | Produce a variable which is free in both terms
freshInBoth :: Var v => Term f v a -> Term f v a -> v -> v
freshInBoth t1 t2 = fresh t2 . fresh t1

fresh :: Var v => Term f v a -> v -> v
fresh t = fresh' (freeVars t)

fresh' :: Var v => Set v -> v -> v
fresh' used = Var.freshIn used

freshes :: Var v => Term f v a -> [v] -> [v]
freshes t = Var.freshes (freeVars t)

freshes' :: Var v => Set v -> [v] -> [v]
freshes' = Var.freshes

freshNamed' :: Var v => Set v -> Text -> v
freshNamed' used n = fresh' used (v' n)

-- | `subst v e body` substitutes `e` for `v` in `body`, avoiding capture by
-- renaming abstractions in `body`
subst :: (Foldable f, Functor f, Var v) => v -> Term f v a -> Term f v a -> Term f v a
subst v r t2 = subst' (const r) v (freeVars r) t2

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
      where x' = fresh t2 (fresh' r x)
            -- rename x to something that cannot be captured by `r`
            e' = if x /= x' then subst' replace v r (rename x x' e)
                 else subst' replace v r e
    Tm body -> tm' ann (fmap (subst' replace v r) body)

-- Like `subst`, but the annotation of the replacement is inherited from
-- the previous annotation at each replacement point.
substInheritAnnotation :: (Foldable f, Functor f, Var v)
                       => v -> Term f v b -> Term f v a -> Term f v a
substInheritAnnotation v r t =
  subst' (\ann -> const ann <$> r) v (freeVars r) t

substsInheritAnnotation
  :: (Foldable f, Functor f, Var v)
  => [(v, Term f v b)] -> Term f v a -> Term f v a
substsInheritAnnotation replacements body = foldr f body (reverse replacements) where
  f (v, t) body = substInheritAnnotation v t body

-- | `substs [(t1,v1), (t2,v2), ...] body` performs multiple simultaneous
-- substitutions, avoiding capture
substs :: (Foldable f, Functor f, Var v)
       => [(v, Term f v a)] -> Term f v a -> Term f v a
substs replacements body = foldr f body (reverse replacements) where
  f (v, t) body = subst v t body

rebuildUp :: (Ord v, Foldable f, Functor f)
          => (f (Term f v a) -> f (Term f v a))
          -> Term f v a
          -> Term f v a
rebuildUp f (Term _ ann body) = case body of
  Var v -> annotatedVar ann v
  Cycle body -> cycle' ann (rebuildUp f body)
  Abs x e -> abs' ann x (rebuildUp f e)
  Tm body -> tm' ann (f $ fmap (rebuildUp f) body)

-- Annotate the tree with the set of bound variables at each node.
annotateBound :: (Ord v, Foldable f, Functor f) => Term f v a -> Term f v (a, Set v)
annotateBound t = go Set.empty t where
  go bound t = let a = (annotation t, bound) in case out t of
    Var v -> annotatedVar a v
    Cycle body -> cycle' a (go bound body)
    Abs x body -> abs' a x (go (Set.insert x bound) body)
    Tm body -> tm' a (go bound <$> body)

freeVarAnnotations :: (Traversable f, Ord v) => Term f v a -> [(v, a)]
freeVarAnnotations t =
  join . runIdentity $ foreachSubterm f (annotateBound t) where
    f t@(Var' v)
      | Set.notMember v (snd . annotation $ t) = pure [(v, fst . annotation $ t)]
    f _ = pure []


foreachSubterm
  :: (Traversable f, Applicative g, Ord v)
  => (Term f v a -> g b)
  -> Term f v a
  -> g [b]
foreachSubterm f e = case out e of
  Var _ -> pure <$> f e
  Cycle body -> liftA2 (:) (f e) (foreachSubterm f body)
  Abs _ body -> liftA2 (:) (f e) (foreachSubterm f body)
  Tm body -> liftA2 (:) (f e) (join . Foldable.toList <$> (sequenceA $ foreachSubterm f <$> body))

-- | `visit f t` applies an effectful function to each subtree of
-- `t` and sequences the results. When `f` returns `Nothing`, `visit`
-- descends into the children of the current subtree. When `f` returns
-- `Just t2`, `visit` replaces the current subtree with `t2`. Thus:
-- `visit (const Nothing) t == pure t` and
-- `visit (const (Just (pure t2))) t == pure t2`
visit :: (Traversable f, Applicative g, Ord v)
      => (Term f v a -> Maybe (g (Term f v a)))
      -> Term f v a
      -> g (Term f v a)
visit f t = case f t of
  Just gt -> gt
  Nothing -> case out t of
    Var _ -> pure t
    Cycle body -> cycle' (annotation t) <$> visit f body
    Abs x e -> abs' (annotation t) x <$> visit f e
    Tm body -> tm' (annotation t) <$> traverse (visit f) body

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' :: (Traversable f, Applicative g, Monad g, Ord v)
       => (f (Term f v a) -> g (f (Term f v a)))
       -> Term f v a
       -> g (Term f v a)
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle' (annotation t) <$> visit' f body
  Abs x e -> abs' (annotation t) x <$> visit' f e
  Tm body -> f body >>= \body -> tm' (annotation t) <$> traverse (visit' f) body

-- | `visit` specialized to the `Identity` effect.
visitPure :: (Traversable f, Ord v)
      => (Term f v a -> Maybe (Term f v a)) -> Term f v a -> Term f v a
visitPure f = runIdentity . visit (fmap pure . f)

data Subst f v a =
  Subst { freshen :: forall m v' . Monad m => (v -> m v') -> m v'
        , bind :: Term f v a -> Term f v a
        , bindInheritAnnotation :: forall b . Term f v b -> Term f v a
        , variable :: v }

unabs1 :: (Foldable f, Functor f, Var v) => Term f v a -> Maybe (Subst f v a)
unabs1 (Term _ _ (Abs v body)) = Just (Subst freshen bind bindInheritAnnotation v) where
  freshen f = f v
  bind x = subst v x body
  bindInheritAnnotation x = substInheritAnnotation v x body
unabs1 _ = Nothing

unabs :: Term f v a -> ([v], Term f v a)
unabs (Term _ _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)

reabs :: Ord v => [v] -> Term f v () -> Term f v ()
reabs vs t = foldr abs t vs

transform :: (Ord v, Foldable g, Functor f)
          => (forall a. f a -> g a) -> Term f v a -> Term g v a
transform f tm = case (out tm) of
  Var v -> annotatedVar (annotation tm) v
  Abs v body -> abs' (annotation tm) v (transform f body)
  Tm subterms ->
    let subterms' = fmap (transform f) subterms
    in tm' (annotation tm) (f subterms')
  Cycle body -> cycle' (annotation tm) (transform f body)

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

-- | We ignore annotations in the `Term`, as these should never affect the
-- meaning of the term.
hash :: forall f v a h . (Functor f, Hashable1 f, Eq v, Var v, Ord h, Accumulate h)
     => Term f v a -> h
hash t = hash' [] t where
  hash' :: [Either [v] v] -> Term f v a -> h
  hash' env (Term _ _ t) = case t of
    Var v -> maybe die hashInt ind
      where lookup (Left cycle) = elem v cycle
            lookup (Right v') = v == v'
            ind = findIndex lookup env
            hashInt :: Int -> h
            hashInt i = Hashable.accumulate [Hashable.UInt64 $ fromIntegral i]
            die = error $ "unknown var in environment: " ++ show (Var.name v)
    Cycle (AbsN' vs t) -> hash' (Left vs : env) t
    Cycle t -> hash' env t
    Abs v t -> hash' (Right v : env) t
    Tm t -> Hashable.hash1 (hashCycle env) (hash' env) $ t

  hashCycle :: [Either [v] v] -> [Term f v a] -> ([h], Term f v a -> h)
  hashCycle env@(Left cycle : envTl) ts | length cycle == length ts =
    let
      permute p xs = case Vector.fromList xs of xs -> map (xs !) p
      hashed = map (\(i,t) -> ((i,t), hash' env t)) (zip [0..] ts)
      pt = map fst (sortBy (comparing snd) hashed)
      (p,ts') = unzip pt
    in case map Right (permute p cycle) ++ envTl of
      env -> (map (hash' env) ts', hash' env)
  hashCycle env ts = (map (hash' env) ts, hash' env)

-- | Use the `hash` function to efficiently remove duplicates from the list, preserving order.
distinct :: forall f v h a proxy . (Functor f, Hashable1 f, Eq v, Var v, Ord h, Accumulate h)
         => proxy h
         -> [Term f v a] -> [Term f v a]
distinct _ ts = map fst (sortBy (comparing snd) m)
  where m = Map.elems (Map.fromList (hashes `zip` (ts `zip` [0 :: Int .. 1])))
        hashes = map hash ts :: [h]

-- | Use the `hash` function to remove elements from `t1s` that exist in `t2s`, preserving order.
subtract :: forall f v h a proxy . (Functor f, Hashable1 f, Eq v, Var v, Ord h, Accumulate h)
         => proxy h
         -> [Term f v a] -> [Term f v a] -> [Term f v a]
subtract _ t1s t2s =
  let skips = Set.fromList (map hash t2s :: [h])
  in filter (\t -> Set.notMember (hash t) skips) t1s

instance (Show1 f, Var v) => Show (Term f v a) where
  -- annotations not shown
  showsPrec p (Term _ _ out) = case out of
    Var v -> (show v ++)
    Cycle body -> ("Cycle " ++) . showsPrec p body
    Abs v body -> showParen True $ (Text.unpack (Var.shortName v) ++) . showString ". " . showsPrec p body
    Tm f -> showsPrec1 p f
