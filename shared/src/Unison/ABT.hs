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
import Data.Functor.Classes (Eq1(..),Show1(..))
import Data.List hiding (cycle)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable
import Prelude hiding (abs,cycle)
import Unison.Symbol (Symbol)
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.JSON as J
import qualified Unison.Symbol as Symbol

type V = Symbol

data ABT f a
  = Var V
  | Cycle a
  | Abs V a
  | Tm (f a) deriving (Functor, Foldable, Traversable)

data Term f = Term { freeVars :: Set V, out :: ABT f (Term f) }

-- | `True` if the term has no free variables, `False` otherwise
isClosed :: Term f -> Bool
isClosed t = Set.null (freeVars t)

-- | `True` if `v` is a member of the set of free variables of `t`
isFreeIn :: V -> Term f -> Bool
isFreeIn v t = Set.member v (freeVars t)

pattern Var' v <- Term _ (Var v)
pattern Cycle' vs t <- Term _ (Cycle (AbsN' vs t))
pattern Abs' v body <- Term _ (Abs v body)
pattern AbsN' vs body <- (unabs -> (vs, body))
pattern Tm' f <- Term _ (Tm f)

v' :: Text -> V
v' = Symbol.prefix

var :: V -> Term f
var v = Term (Set.singleton v) (Var v)

var' :: Text -> Term f
var' v = var (Symbol.prefix v)

abs :: V -> Term f -> Term f
abs v body = Term (Set.delete v (freeVars body)) (Abs v body)

tm :: Foldable f => f (Term f) -> Term f
tm t = Term (Set.unions (fmap freeVars (Foldable.toList t)))
            (Tm t)

cycle :: Term f -> Term f
cycle t = Term (freeVars t) (Cycle t)

into :: Foldable f => ABT f (Term f) -> Term f
into abt = case abt of
  Var x -> var x
  Cycle t -> cycle t
  Abs v a -> abs v a
  Tm t -> tm t

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f) => V -> V -> Term f -> Term f
rename old new t0@(Term _ t) = case t of
  Var v -> if v == old then var new else t0
  Cycle body -> cycle (rename old new body)
  Abs v body -> if v == old then abs v body
                else abs v (rename old new body)
  Tm v -> tm (fmap (rename old new) v)

-- | Produce a variable which is free in both terms
freshInBoth :: Term f -> Term f -> V -> V
freshInBoth t1 t2 = fresh t2 . fresh t1

fresh :: Term f -> V -> V
fresh t = fresh' (freeVars t)

fresh' :: Set V -> V -> V
fresh' used = Symbol.freshIn used

freshes :: Term f -> [V] -> [V]
freshes t = freshes' (freeVars t)

freshes' :: Set V -> [V] -> [V]
freshes' _ [] = []
freshes' used (h:t) =
  let h' = fresh' used h
  in h' : freshes' (Set.insert h' used) t

freshNamed' :: Set V -> Text -> V
freshNamed' used n = fresh' used (v' n)

-- | `subst t x body` substitutes `t` for `x` in `body`, avoiding capture
subst :: (Foldable f, Functor f) => Term f -> V -> Term f -> Term f
subst t x body = replace t match body where
  match (Var' v) = x == v
  match _ = False

-- | `substs [(t1,v1), (t2,v2), ...] body` performs multiple simultaneous
-- substitutions, avoiding capture
substs :: (Foldable f, Functor f) => [(V, Term f)] -> Term f -> Term f
substs replacements body = foldr f body replacements where
  f (v, t) body = subst t v body

-- | `rewrite t f body` substitutes `t` for all maximal (outermost)
-- subterms matching the predicate `f` in `body`, avoiding capture.
replace :: (Foldable f, Functor f) => Term f -> (Term f -> Bool) -> Term f -> Term f
replace t f body = go t f body where
  go t f body | f body = t
  go t f body = case out body of
    Var v -> var v
    Cycle body -> cycle (go t f body)
    Abs x e -> abs x' e'
      where x' = freshInBoth t body x
            -- rename x to something that cannot be captured by `t`
            e' = if x /= x' then go t f (rename x x' e)
                 else go t f e
    Tm body -> tm (fmap (go t f) body)

-- | `visit f t` applies an effectful function to each subtree of
-- `t` and sequences the results. When `f` returns `Nothing`, `visit`
-- descends into the children of the current subtree. When `f` returns
-- `Just t2`, `visit` replaces the current subtree with `t2`. Thus:
-- `visit (const Nothing) t == pure t` and
-- `visit (const (Just (pure t2))) t == pure t2`
visit :: (Traversable f, Applicative g) => (Term f -> Maybe (g (Term f))) -> Term f -> g (Term f)
visit f t = case f t of
  Just gt -> gt
  Nothing -> case out t of
    Var _ -> pure t
    Cycle body -> cycle <$> visit f body
    Abs x e -> abs x <$> visit f e
    Tm body -> tm <$> traverse (visit f) body

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' :: (Traversable f, Applicative g, Monad g)
       => (f (Term f) -> g (f (Term f))) -> Term f -> g (Term f)
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle <$> visit' f body
  Abs x e -> abs x <$> visit' f e
  Tm body -> f body >>= \body -> tm <$> traverse (visit' f) body

-- | A single step 'focusing' action, returns the subtree and a function
-- to replace that subtree
type Focus1 f a = f a -> Maybe (a, a -> f a)

-- | Extract the subterm at a given path
at :: Foldable f => [Focus1 f (Term f)] -> Term f -> Maybe (Term f)
at path t = fst <$> focus path t

-- | Modify the subterm a path points to
modify :: Foldable f
       => (Term f -> Term f) -> [Focus1 f (Term f)] -> Term f -> Maybe (Term f)
modify f path t = (\(t,replace) -> replace (f t)) <$> focus path t

-- | Focus on a subterm, obtaining the subtree and a function to replace that subtree
focus :: Foldable f
      => [Focus1 f (Term f)] -> Term f -> Maybe (Term f, Term f -> Term f)
focus [] t = Just (t, id)
focus path@(hd:tl) t = case out t of
  Var _ -> Nothing
  Cycle t ->
    let f (t,replace) = (t, cycle . replace)
    in f <$> focus path t
  Abs v t ->
    let f (t,replace) = (t, abs v . replace)
    in f <$> focus path t
  Tm ft -> do
    (sub,hreplace) <- hd ft
    (t,replace) <- focus tl sub
    pure (t, tm . hreplace . replace)

-- | Returns the longest prefix of the path which points to a subterm
-- in which `v` is not bound.
introducedAt :: V -> [Focus1 f (Term f)] -> Term f -> Maybe [Focus1 f (Term f)]
introducedAt v path t = f =<< boundAlong path t where
  f bs = case dropWhile (\vs -> not (Set.member v vs)) (reverse bs) of
    [] -> if elem v (fst (unabs t)) then Just [] else Nothing
    p -> Just (take (length p) path)

-- | Returns the set of variables in scope at the given path, if valid
boundAt :: [Focus1 f (Term f)] -> Term f -> Maybe (Set V)
boundAt path t = f =<< boundAlong path t where
  f [] = Nothing
  f vs = Just (last vs)

-- | Returns the set of variables in scope at the given path,
-- or the empty set if path is invalid
boundAt' :: [Focus1 f (Term f)] -> Term f -> Set V
boundAt' path t = fromMaybe Set.empty (boundAt path t)

-- | For each element of the input path, the set of variables in scope
boundAlong :: [Focus1 f (Term f)] -> Term f -> Maybe [Set V]
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

unabs :: Term f -> ([V], Term f)
unabs (Term _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)

reabs :: [V] -> Term f -> Term f
reabs vs t = foldr abs t vs

instance (Foldable f, Functor f, Eq1 f) => Eq (Term f) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 == t2 = go (out t1) (out t2) where
    go (Var v) (Var v2) | v == v2 = True
    go (Cycle t1) (Cycle t2) = t1 == t2
    go (Abs v1 body1) (Abs v2 body2) =
      if v1 == v2 then body1 == body2
      else let v3 = freshInBoth body1 body2 v1
           in rename v1 v3 body1 == rename v2 v3 body2
    go (Tm f1) (Tm f2) = eq1 f1 f2
    go _ _ = False

instance J.ToJSON1 f => ToJSON (Term f) where
  toJSON (Term _ e) = case e of
    Var v -> J.array [J.text "Var", toJSON v]
    Cycle body -> J.array [J.text "Cycle", toJSON body]
    Abs v body -> J.array [J.text "Abs", J.array [toJSON v, toJSON body]]
    Tm v -> J.array [J.text "Tm", J.toJSON1 v]

instance (Foldable f, J.FromJSON1 f) => FromJSON (Term f) where
  parseJSON j = do
    t <- J.at0 (Aeson.withText "ABT.tag" pure) j
    case t of
      _ | t == "Var"   -> var <$> J.at 1 Aeson.parseJSON j
      _ | t == "Cycle" -> cycle <$> J.at 1 Aeson.parseJSON j
      _ | t == "Abs"   -> J.at 1 (\j -> abs <$> J.at 0 Aeson.parseJSON j <*> J.at 1 Aeson.parseJSON j) j
      _ | t == "Tm"    -> tm <$> J.at 1 J.parseJSON1 j
      _                -> fail ("unknown tag: " ++ Text.unpack t)

instance Show1 f => Show (Term f) where
  showsPrec p (Term _ out) = case out of
    Var v -> showsPrec 0 v
    Cycle body -> showsPrec p body
    Abs v body -> showParen True $ showsPrec 0 v . showString ". " . showsPrec p body
    Tm f -> showsPrec1 p f
