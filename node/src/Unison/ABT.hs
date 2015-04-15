-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.ABT (ABT(..),abs,freevars,into,out,rename,subst,tm,Term,V) where

import Control.Applicative
import Data.Aeson
import Data.Foldable (Foldable)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable
import Data.Vector ((!?))
import Prelude hiding (abs)
import Unison.Symbol (Symbol)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
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

-- | `subst t x body` substitutes `t` for `x` in `body`, avoiding capture
subst :: (Foldable f, Functor f) => Term f -> V -> Term f -> Term f
subst t x body = case out body of
  Var v | x == v -> t
  Var v -> var v
  Abs x e -> abs x' e'
    where memberOf s1 s2 v = Set.member v s1 || Set.member v s2
          x' = fresh (memberOf (freevars t) (freevars body)) x
          -- rename x to something that cannot be captured
          e' = if x /= x' then subst t x (rename x x' e)
               else e
  Tm body -> tm (fmap (subst t x) body)

-- | Subject to `toIntTagged fa fromIntTagged == Just fa`, which
-- ensures that the `toList` plus the tag is all the information needed
-- to reconstruct any `f a`.
class Foldable f => IntTagged f where
  intTag :: f a -> Int
  fromIntTagged :: Int -> [a] -> Maybe (f a)

-- | Subject to `toTextTagged fa fromTextTagged == Just fa`, which
-- ensures that the `toList` plus the tag is all the information needed
-- to reconstruct any `f a`.
class Foldable f => TextTagged f where
  textTag :: f a -> Text
  fromTextTagged :: Text -> [a] -> Maybe (f a)

toIntTagged :: IntTagged f => f a -> (Int -> [a] -> r) -> r
toIntTagged fa k = k (intTag fa) (Foldable.toList fa)

toTextTagged :: TextTagged f => f a -> (Text -> [a] -> r) -> r
toTextTagged fa k = k (textTag fa) (Foldable.toList fa)

instance TextTagged f => ToJSON (Term f) where
  toJSON (Term _ e) = case e of
    Var v -> array [text "Var", toJSON v]
    Abs v body -> array [text "Abs", toJSON v, toJSON body]
    Tm v -> array [text "Tm", toTextTagged v (\tag xs -> array (text tag : map toJSON xs))]

instance TextTagged f => FromJSON (Term f) where
  parseJSON j = do
    t <- at0 (J.withText "ABT.tag" pure) j
    case t of
      _ | t == "Var" -> at 1 (\j -> var <$> parseJSON j) j
      _ | t == "Abs" -> abs <$> at 1 parseJSON j <*> at 2 parseJSON j
      _ | t == "Tm"  -> tm <$> do
        tag <- at0 (J.withText "ABT.f.tag" pure) j
        args <- Vector.drop 1 <$> at 1 (J.withArray "ABT.f.args" pure) j
        parsedArgs <- traverse parseJSON args
        maybe (fail (msg t)) pure (fromTextTagged tag (Vector.toList parsedArgs))
      _ -> fail (msg t)
    where msg t = "unknown tag: " ++ Text.unpack t

-- todo: binary encoder/decoder can work similarly, just using IntTagged

text :: Text -> Value
text t = toJSON t

array :: [Value] -> Value
array = Aeson.Array . Vector.fromList

-- | Run the parser on the nth (0-based) subtree, assuming the input is an array
at :: Int -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
at ind parse j = J.withArray "at" k j where
  k vs = maybe z parse (vs !? 0) where z = fail ("invalid index: " ++ show ind)

-- | Run the parser on the 0th subtree, assuming the input is an array
at0 :: (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
at0 = at 0
