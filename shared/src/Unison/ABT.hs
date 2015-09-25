-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.ABT where

import Data.Aeson (ToJSON(..),FromJSON(..))
import Data.List hiding (cycle)
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable
import Data.Vector ((!))
import Prelude hiding (abs,cycle)
import Prelude.Extras (Eq1(..), Show1(..))
import Unison.Hashable (Hashable,Hashable1)
import Unison.Var (Var)
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Unison.Hashable as Hashable
import qualified Unison.JSON as J
import qualified Unison.Var as Var

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r) deriving (Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Individual variables are annotated with a value of
-- type `v`.
data Term f v a = Term { freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a) }

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
pattern Abs' v body <- Term _ _ (Abs v body)
pattern AbsN' vs body <- (unabs -> (vs, body))
pattern Tm' f <- Term _ _ (Tm f)

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

tm :: (Foldable f, Ord v) => f (Term f v ()) -> Term f v ()
tm = tm' ()

tm' :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm' a t =
  Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)

cycle :: Term f v () -> Term f v ()
cycle = cycle' ()

cycle' :: a -> Term f v a -> Term f v a
cycle' a t = Term (freeVars t) a (Cycle t)

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

-- | `subst t x body` substitutes `t` for `x` in `body`, avoiding capture
subst :: (Foldable f, Functor f, Var v) => Term f v a -> v -> Term f v a -> Term f v a
subst t x body = replace t match body where
  match (Var' v) = x == v
  match _ = False

-- | `substs [(t1,v1), (t2,v2), ...] body` performs multiple simultaneous
-- substitutions, avoiding capture
substs :: (Foldable f, Functor f, Var v) => [(v, Term f v a)] -> Term f v a -> Term f v a
substs replacements body = foldr f body replacements where
  f (v, t) body = subst t v body

-- | `replace t f body` substitutes `t` for all maximal (outermost)
-- subterms matching the predicate `f` in `body`, avoiding capture.
replace :: (Foldable f, Functor f, Var v)
        => Term f v a
        -> (Term f v a -> Bool)
        -> Term f v a
        -> Term f v a
replace t f body | f body = t
replace t f t2@(Term _ ann body) = case body of
  Var v -> annotatedVar ann v
  Cycle body -> cycle' ann (replace t f body)
  Abs x e -> abs' ann x' e'
    where x' = freshInBoth t t2 x
          -- rename x to something that cannot be captured by `t`
          e' = if x /= x' then replace t f (rename x x' e)
               else replace t f e
  Tm body -> tm' ann (fmap (replace t f) body)

-- | `visit f t` applies an effectful function to each subtree of
-- `t` and sequences the results. When `f` returns `Nothing`, `visit`
-- descends into the children of the current subtree. When `f` returns
-- `Just t2`, `visit` replaces the current subtree with `t2`. Thus:
-- `visit (const Nothing) t == pure t` and
-- `visit (const (Just (pure t2))) t == pure t2`
visit :: (Traversable f, Applicative g, Ord v)
      => (Term f v () -> Maybe (g (Term f v ())))
      -> Term f v ()
      -> g (Term f v ())
visit f t = case f t of
  Just gt -> gt
  Nothing -> case out t of
    Var _ -> pure t
    Cycle body -> cycle <$> visit f body
    Abs x e -> abs x <$> visit f e
    Tm body -> tm <$> traverse (visit f) body

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' :: (Traversable f, Applicative g, Monad g, Ord v)
       => (f (Term f v ()) -> g (f (Term f v ())))
       -> Term f v ()
       -> g (Term f v ())
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle <$> visit' f body
  Abs x e -> abs x <$> visit' f e
  Tm body -> f body >>= \body -> tm <$> traverse (visit' f) body

-- | A single step 'focusing' action, returns the subtree and a function
-- to replace that subtree
type Focus1 f a = f a -> Maybe (a, a -> f a)

-- | Extract the subterm at a given path
at :: (Foldable f, Ord v) => [Focus1 f (Term f v a)] -> Term f v a -> Maybe (Term f v a)
at path t = fst <$> focus path t

-- | Modify the subterm a path points to
modify :: (Foldable f, Ord v)
       => (Term f v a -> Term f v a)
       -> [Focus1 f (Term f v a)]
       -> Term f v a
       -> Maybe (Term f v a)
modify f path t = (\(t,replace) -> replace (f t)) <$> focus path t

-- | Focus on a subterm, obtaining the subtree and a function to replace that subtree
focus :: (Foldable f, Ord v)
      => [Focus1 f (Term f v a)]
      -> Term f v a
      -> Maybe (Term f v a, Term f v a -> Term f v a)
focus [] t = Just (t, id)
focus path@(hd:tl) (Term _ ann t) = case t of
  Var _ -> Nothing
  Cycle t ->
    let f (t,replace) = (t, cycle' ann . replace)
    in f <$> focus path t
  Abs v t ->
    let f (t,replace) = (t, abs' ann v . replace)
    in f <$> focus path t
  Tm ft -> do
    (sub,hreplace) <- hd ft
    (t,replace) <- focus tl sub
    pure (t, tm' ann . hreplace . replace)

-- | Returns the longest prefix of the path which points to a subterm
-- in which `v` is not bound.
introducedAt :: Ord v => v -> [Focus1 f (Term f v ())] -> Term f v () -> Maybe [Focus1 f (Term f v ())]
introducedAt v path t = f =<< boundAlong path t where
  f bs = case dropWhile (\vs -> not (Set.member v vs)) (reverse bs) of
    [] -> if elem v (fst (unabs t)) then Just [] else Nothing
    p -> Just (take (length p) path)

-- | Returns the set of variables in scope at the given path, if valid
boundAt :: Ord v => [Focus1 f (Term f v ())] -> Term f v () -> Maybe (Set v)
boundAt path t = f =<< boundAlong path t where
  f [] = Nothing
  f vs = Just (last vs)

-- | Returns the set of variables in scope at the given path,
-- or the empty set if path is invalid
boundAt' :: Ord v => [Focus1 f (Term f v ())] -> Term f v () -> Set v
boundAt' path t = fromMaybe Set.empty (boundAt path t)

-- | For each element of the input path, the set of variables in scope
boundAlong :: Ord v => [Focus1 f (Term f v ())] -> Term f v () -> Maybe [Set v]
boundAlong path t = go Set.empty path t where
  go _ [] _ = Just []
  go env path@(hd:tl) t = case out t of
    Var _ -> Nothing
    Cycle t -> go env path t
    Abs v t -> let !env' = Set.insert v env in go env' path t
    Tm ft -> do
      (t,_) <- hd ft
      tl <- go env tl t
      pure (env : tl)

unabs :: Term f v a -> ([v], Term f v a)
unabs (Term _ _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)

reabs :: Ord v => [v] -> Term f v () -> Term f v ()
reabs vs t = foldr abs t vs

instance (Foldable f, Functor f, Eq1 f, Eq a, Var v) => Eq (Term f v a) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 == t2 = annotation t1 == annotation t2 && go (out t1) (out t2) where
    go (Var v) (Var v2) | v == v2 = True
    go (Cycle t1) (Cycle t2) = t1 == t2
    go (Abs v1 body1) (Abs v2 body2) =
      if v1 == v2 then body1 == body2
      else let v3 = freshInBoth body1 body2 v1
           in rename v1 v3 body1 == rename v2 v3 body2
    go (Tm f1) (Tm f2) = f1 ==# f2
    go _ _ = False

instance (J.ToJSON1 f, ToJSON v, ToJSON a) => ToJSON (Term f v a) where
  toJSON (Term _ a e) =
    let
      body = case e of
        Var v -> J.array [J.text "Var", toJSON v]
        Cycle body -> J.array [J.text "Cycle", toJSON body]
        Abs v body -> J.array [J.text "Abs", J.array [toJSON v, toJSON body]]
        Tm v -> J.array [J.text "Tm", J.toJSON1 v]
    in
      J.array [toJSON a, body]

instance (Foldable f, J.FromJSON1 f, FromJSON v, Ord v, FromJSON a) => FromJSON (Term f v a) where
  parseJSON j = do
    ann <- J.at 0 Aeson.parseJSON j
    J.at 1 (\j -> do {
      t <- J.at0 (Aeson.withText "ABT.tag" pure) j;
      case t of
        _ | t == "Var"   -> annotatedVar ann <$> J.at 1 Aeson.parseJSON j
        _ | t == "Cycle" -> cycle' ann <$> J.at 1 Aeson.parseJSON j
        _ | t == "Abs"   -> J.at 1 (\j -> abs' ann <$> J.at 0 Aeson.parseJSON j <*> J.at 1 Aeson.parseJSON j) j
        _ | t == "Tm"    -> tm' ann <$> J.at 1 J.parseJSON1 j
        _                -> fail ("unknown tag: " ++ Text.unpack t)
    }) j

-- | We ignore annotations in the `Term`, as these should never affect the
-- meaning of the term.
hash :: forall f v a h . (Functor f, Hashable1 f, Eq v, Var v, Ord h, Hashable h)
     => Term f v a -> h
hash t = hash' [] t where
  hash' :: [Either [v] v] -> Term f v a -> h
  hash' env (Term _ _ t) = case t of
    Var v -> maybe die hashInt ind
      where lookup (Left cycle) = elem v cycle
            lookup (Right v') = v == v'
            ind = findIndex lookup env
            -- env not likely to be very big, prefer to encode in one byte if possible
            hashInt :: Int -> h
            hashInt i = Hashable.hash [Hashable.VarInt i]
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
distinct :: forall f v h a proxy . (Functor f, Hashable1 f, Eq v, Var v, Ord h, Hashable h)
         => proxy h
         -> [Term f v a] -> [Term f v a]
distinct _ ts = map fst (sortBy (comparing snd) m)
  where m = Map.elems (Map.fromList (hashes `zip` (ts `zip` [0 :: Int .. 1])))
        hashes = map hash ts :: [h]

-- | Use the `hash` function to remove elements from `t1s` that exist in `t2s`, preserving order.
subtract :: forall f v h a proxy . (Functor f, Hashable1 f, Eq v, Var v, Ord h, Hashable h)
         => proxy h
         -> [Term f v a] -> [Term f v a] -> [Term f v a]
subtract _ t1s t2s =
  let skips = Set.fromList (map hash t2s :: [h])
  in filter (\t -> Set.notMember (hash t) skips) t1s

instance (Show1 f, Var v) => Show (Term f v a) where
  -- annotations not shown
  showsPrec p (Term _ _ out) = case out of
    Var v -> showsPrec 0 (Var.shortName v)
    Cycle body -> showsPrec p body
    Abs v body -> showParen True $ showsPrec 0 (Var.shortName v) . showString ". " . showsPrec p body
    Tm f -> showsPrec1 p f
