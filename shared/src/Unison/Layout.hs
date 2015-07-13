{-# LANGUAGE DeriveFunctor #-}

module Unison.Layout where

import Unison.Path (Path)
import qualified Unison.Path as Path

data L e r
  = Embed e
  | Empty
  | Linebreak
  | Nest e r
  | Append r r

-- A `Doc` without the nondeterminism. All layout decisions have been fixed.
data Layout p e = Layout p (L e (Layout p e))

mapPath :: (p -> p2) -> Layout p e -> Layout p2 e
mapPath f (Layout p l) = Layout (f p) $ case l of
  Empty -> Empty
  Linebreak -> Linebreak
  Embed e -> Embed e
  Append a b -> Append (mapPath f a) (mapPath f b)

linebreak :: Path p => Layout p e
linebreak = Layout Path.root Linebreak

empty :: Path p => Layout p e
empty = Layout Path.root Empty

embed :: Path p => e -> Layout p e
embed e = Layout Path.root (Embed e)

-- | Indent the layout by the given element
nest :: Path p => e -> Layout p e -> Layout p e
nest e (Layout p d) = Layout p (Nest e (Layout Path.root d))

append :: Path p => Layout p e -> Layout p e -> Layout p e
append (Layout p1 l1) (Layout p2 l2) =
  case Path.factor p1 p2 of
    (lca, (p1, p2)) -> Layout lca (Layout p1 l1 `Append` Layout p2 l2)

reroot :: Path p => p -> Layout p e -> Layout p e
reroot p (Layout _ l) = Layout p l

instance Path p => Monoid (Layout p e) where
  mempty = empty
  mappend = append
