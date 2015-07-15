-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.ABT where

import Data.Aeson (ToJSON(..),FromJSON(..))
import Data.List hiding (cycle)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable
import Prelude hiding (abs,cycle)
import Prelude.Extras (Eq1(..), Show1(..))
import Unison.Symbol (Symbol)
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.JSON as J
import qualified Unison.Symbol as Symbol

type V = Symbol

data ABT f r
  = Var V
  | Cycle r
  | Abs V r
  | Tm (f r) deriving (Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`.
data Term f a = Term { freeVars :: Set V, annotation :: a, out :: ABT f (Term f a) }

-- | `True` if the term has no free variables, `False` otherwise
isClosed :: Term f a -> Bool
isClosed t = Set.null (freeVars t)

-- | `True` if `v` is a member of the set of free variables of `t`
isFreeIn :: V -> Term f a -> Bool
isFreeIn v t = Set.member v (freeVars t)

-- | Replace the annotation with the given argument.
annotate :: a -> Term f a -> Term f a
annotate a (Term fvs _ out) = Term fvs a out

-- | Modifies the annotations in this tree
instance Functor f => Functor (Term f) where
  fmap f (Term fvs a sub) = Term fvs (f a) (fmap (fmap f) sub)

pattern Var' v <- Term _ _ (Var v)
pattern Cycle' vs t <- Term _ _ (Cycle (AbsN' vs t))
pattern Abs' v body <- Term _ _ (Abs v body)
pattern AbsN' vs body <- (unabs -> (vs, body))
pattern Tm' f <- Term _ _ (Tm f)

v' :: Text -> V
v' = Symbol.prefix

var :: V -> Term f ()
var = annotatedVar ()

var' :: Text -> Term f ()
var' v = var (Symbol.prefix v)

annotatedVar :: a -> V -> Term f a
annotatedVar a v = Term (Set.singleton v) a (Var v)

abs :: V -> Term f () -> Term f ()
abs = abs' ()

abs' :: a -> V -> Term f a -> Term f a
abs' a v body = Term (Set.delete v (freeVars body)) a (Abs v body)

tm :: Foldable f => f (Term f ()) -> Term f ()
tm = tm' ()

tm' :: Foldable f => a -> f (Term f a) -> Term f a
tm' a t =
  Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)

cycle :: Term f () -> Term f ()
cycle = cycle' ()

cycle' :: a -> Term f a -> Term f a
cycle' a t = Term (freeVars t) a (Cycle t)

into :: Foldable f => ABT f (Term f ()) -> Term f ()
into = into' ()

into' :: Foldable f => a -> ABT f (Term f a) -> Term f a
into' a abt = case abt of
  Var x -> annotatedVar a x
  Cycle t -> cycle' a t
  Abs v r -> abs' a v r
  Tm t -> tm' a t

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f) => V -> V -> Term f a -> Term f a
rename old new t0@(Term _ ann t) = case t of
  Var v -> if v == old then annotatedVar ann new else t0
  Cycle body -> cycle' ann (rename old new body)
  Abs v body -> if v == old then abs' ann v body
                else abs' ann v (rename old new body)
  Tm v -> tm' ann (fmap (rename old new) v)

-- | Produce a variable which is free in both terms
freshInBoth :: Term f a -> Term f a -> V -> V
freshInBoth t1 t2 = fresh t2 . fresh t1

fresh :: Term f a -> V -> V
fresh t = fresh' (freeVars t)

fresh' :: Set V -> V -> V
fresh' used = Symbol.freshIn used

freshes :: Term f a -> [V] -> [V]
freshes t = freshes' (freeVars t)

freshes' :: Set V -> [V] -> [V]
freshes' _ [] = []
freshes' used (h:t) =
  let h' = fresh' used h
  in h' : freshes' (Set.insert h' used) t

freshNamed' :: Set V -> Text -> V
freshNamed' used n = fresh' used (v' n)

-- | `subst t x body` substitutes `t` for `x` in `body`, avoiding capture
subst :: (Foldable f, Functor f) => Term f a -> V -> Term f a -> Term f a
subst t x body = replace t match body where
  match (Var' v) = x == v
  match _ = False

-- | `substs [(t1,v1), (t2,v2), ...] body` performs multiple simultaneous
-- substitutions, avoiding capture
substs :: (Foldable f, Functor f) => [(V, Term f a)] -> Term f a -> Term f a
substs replacements body = foldr f body replacements where
  f (v, t) body = subst t v body

-- | `replace t f body` substitutes `t` for all maximal (outermost)
-- subterms matching the predicate `f` in `body`, avoiding capture.
replace :: (Foldable f, Functor f)
        => Term f a
        -> (Term f a -> Bool)
        -> Term f a
        -> Term f a
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
visit :: (Traversable f, Applicative g) => (Term f () -> Maybe (g (Term f ()))) -> Term f () -> g (Term f ())
visit f t = case f t of
  Just gt -> gt
  Nothing -> case out t of
    Var _ -> pure t
    Cycle body -> cycle <$> visit f body
    Abs x e -> abs x <$> visit f e
    Tm body -> tm <$> traverse (visit f) body

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' :: (Traversable f, Applicative g, Monad g)
       => (f (Term f ()) -> g (f (Term f ()))) -> Term f () -> g (Term f ())
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle <$> visit' f body
  Abs x e -> abs x <$> visit' f e
  Tm body -> f body >>= \body -> tm <$> traverse (visit' f) body

-- | A single step 'focusing' action, returns the subtree and a function
-- to replace that subtree
type Focus1 f a = f a -> Maybe (a, a -> f a)

-- | Extract the subterm at a given path
at :: Foldable f => [Focus1 f (Term f a)] ->  Term f a -> Maybe (Term f a)
at path t = fst <$> focus path t

-- | Modify the subterm a path points to
modify :: Foldable f
       => (Term f a -> Term f a)
       -> [Focus1 f (Term f a)]
       -> Term f a
       -> Maybe (Term f a)
modify f path t = (\(t,replace) -> replace (f t)) <$> focus path t

-- | Focus on a subterm, obtaining the subtree and a function to replace that subtree
focus :: Foldable f
      => [Focus1 f (Term f a)]
      -> Term f a
      -> Maybe (Term f a, Term f a -> Term f a)
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
introducedAt :: V -> [Focus1 f (Term f ())] -> Term f () -> Maybe [Focus1 f (Term f ())]
introducedAt v path t = f =<< boundAlong path t where
  f bs = case dropWhile (\vs -> not (Set.member v vs)) (reverse bs) of
    [] -> if elem v (fst (unabs t)) then Just [] else Nothing
    p -> Just (take (length p) path)

-- | Returns the set of variables in scope at the given path, if valid
boundAt :: [Focus1 f (Term f ())] -> Term f () -> Maybe (Set V)
boundAt path t = f =<< boundAlong path t where
  f [] = Nothing
  f vs = Just (last vs)

-- | Returns the set of variables in scope at the given path,
-- or the empty set if path is invalid
boundAt' :: [Focus1 f (Term f ())] -> Term f () -> Set V
boundAt' path t = fromMaybe Set.empty (boundAt path t)

-- | For each element of the input path, the set of variables in scope
boundAlong :: [Focus1 f (Term f ())] -> Term f () -> Maybe [Set V]
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

unabs :: Term f a -> ([V], Term f a)
unabs (Term _ _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)

reabs :: [V] -> Term f () -> Term f ()
reabs vs t = foldr abs t vs

instance (Foldable f, Functor f, Eq1 f, Eq a) => Eq (Term f a) where
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

instance (J.ToJSON1 f, ToJSON a) => ToJSON (Term f a) where
  toJSON (Term _ a e) =
    let
      body = case e of
        Var v -> J.array [J.text "Var", toJSON v]
        Cycle body -> J.array [J.text "Cycle", toJSON body]
        Abs v body -> J.array [J.text "Abs", J.array [toJSON v, toJSON body]]
        Tm v -> J.array [J.text "Tm", J.toJSON1 v]
    in
      J.array [toJSON a, body]

instance (Foldable f, J.FromJSON1 f, FromJSON a) => FromJSON (Term f a) where
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

instance Show1 f => Show (Term f a) where
  -- annotations not shown
  showsPrec p (Term _ _ out) = case out of
    Var v -> showsPrec 0 v
    Cycle body -> showsPrec p body
    Abs v body -> showParen True $ showsPrec 0 v . showString ". " . showsPrec p body
    Tm f -> showsPrec1 p f
