-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.ABT (ABT(..),abs,freevars,into,out,rename,subst,tm,Term,V) where

import Control.Applicative
import Data.Aeson
import Data.Foldable (Foldable)
import Data.Functor.Classes
import Data.Set (Set)
import Prelude hiding (abs)
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
  | Abs V a
  | Tm (f a) deriving Functor

data Term f = Term { freevars :: Set V, out :: ABT f (Term f) }

var :: V -> Term f
var v = Term (Set.singleton v) (Var v)

abs :: V -> Term f -> Term f
abs v body = Term (Set.delete v (freevars body)) (Abs v body)

tm :: Foldable f => f (Term f) -> Term f
tm t = Term (Set.unions (fmap freevars (Foldable.toList t)))
            (Tm t)

into :: Foldable f => ABT f (Term f) -> Term f
into abt = case abt of
  Var x -> var x
  Abs v a -> abs v a
  Tm t -> tm t

fresh :: (V -> Bool) -> V -> V
fresh used v | used v = fresh used (Symbol.freshen v)
fresh _  v = v

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f) => V -> V -> Term f -> Term f
rename old new (Term _ t) = case t of
  Var v -> if v == old then var new else var old
  Abs v body -> if v == old then abs v body
                else abs v (rename old new body)
  Tm v -> tm (fmap (rename old new) v)

-- | Produce a variable which is free in both terms
freshInBoth :: Term f -> Term f -> V -> V
freshInBoth t1 t2 x = fresh (memberOf (freevars t1) (freevars t2)) x
  where memberOf s1 s2 v = Set.member v s1 || Set.member v s2

-- | `subst t x body` substitutes `t` for `x` in `body`, avoiding capture
subst :: (Foldable f, Functor f) => Term f -> V -> Term f -> Term f
subst t x body = case out body of
  Var v | x == v -> t
  Var v -> var v
  Abs x e -> abs x' e'
    where x' = freshInBoth t body x
          -- rename x to something that cannot be captured
          e' = if x /= x' then subst t x (rename x x' e)
               else e
  Tm body -> tm (fmap (subst t x) body)

instance (Foldable f, Functor f, Eq1 f) => Eq (Term f) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 == t2 = go (out t1) (out t2) where
    go (Var v) (Var v2) | v == v2 = True
    go (Abs v1 body1) (Abs v2 body2) =
      if v1 == v2 then body1 == body2
      else let v3 = freshInBoth body1 body2 v1
           in rename v1 v3 body1 == rename v2 v3 body2
    go (Tm f1) (Tm f2) = eq1 f1 f2
    go _ _ = False

instance J.ToJSON1 f => ToJSON (Term f) where
  toJSON (Term _ e) = case e of
    Var v -> J.array [J.text "Var", toJSON v]
    Abs v body -> J.array [J.text "Abs", toJSON v, toJSON body]
    Tm v -> J.array [J.text "Tm", J.toJSON1 v]

instance (Foldable f, J.FromJSON1 f) => FromJSON (Term f) where
  parseJSON j = do
    t <- J.at0 (Aeson.withText "ABT.tag" pure) j
    case t of
      _ | t == "Var" -> var <$> J.at 1 Aeson.parseJSON j
      _ | t == "Abs" -> abs <$> J.at 1 Aeson.parseJSON j <*> J.at 2 Aeson.parseJSON j
      _ | t == "Tm"  -> tm <$> J.at 1 J.parseJSON1 j
      _              -> fail ("unknown tag: " ++ Text.unpack t)

-- todo: binary encoder/decoder can work similarly

-- hash :: IntTagged f => Term f ->

