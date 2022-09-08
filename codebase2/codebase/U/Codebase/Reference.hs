{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}

module U.Codebase.Reference where

import Control.Lens (Bifunctor (..), Lens, Prism, Traversal, lens, prism)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Text (Text)
import Data.Word (Word64)
import U.Codebase.ShortHash (ShortHash)
import qualified U.Codebase.ShortHash as SH
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash

-- | This is the canonical representation of Reference
type Reference = Reference' Text Hash

type Id = Id' Hash

data Reference' t h
  = ReferenceBuiltin t
  | ReferenceDerived (Id' h)
  deriving (Eq, Ord, Show)

_ReferenceDerived :: Prism (Reference' t h) (Reference' t h') (Id' h) (Id' h')
_ReferenceDerived = prism embed project
  where
    embed (Id h pos) = ReferenceDerived (Id h pos)
    project (ReferenceDerived id') = Right id'
    project (ReferenceBuiltin t) = Left (ReferenceBuiltin t)

pattern Derived :: h -> Pos -> Reference' t h
pattern Derived h i = ReferenceDerived (Id h i)

{-# COMPLETE ReferenceBuiltin, Derived #-}

type Pos = Word64

data Id' h = Id h Pos
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

t_ :: Traversal (Reference' t h) (Reference' t' h) t t'
t_ f = \case
  ReferenceBuiltin t -> ReferenceBuiltin <$> f t
  ReferenceDerived id -> pure (ReferenceDerived id)

h_ :: Traversal (Reference' t h) (Reference' t h') h h'
h_ f = \case
  ReferenceBuiltin t -> pure (ReferenceBuiltin t)
  Derived h i -> Derived <$> f h <*> pure i

idH :: Lens (Id' h) (Id' h') h h'
idH = lens (\(Id h _w) -> h) (\(Id _h w) h -> Id h w)

isBuiltin :: Reference -> Bool
isBuiltin (ReferenceBuiltin _) = True
isBuiltin _ = False

toShortHash :: Reference -> ShortHash
toShortHash (ReferenceBuiltin b) = SH.Builtin b
toShortHash (ReferenceDerived (Id h 0)) = SH.ShortHash (Hash.toBase32HexText h) Nothing Nothing
toShortHash (ReferenceDerived (Id h i)) = SH.ShortHash (Hash.toBase32HexText h) (Just i) Nothing

instance Bifunctor Reference' where
  bimap f _ (ReferenceBuiltin t) = ReferenceBuiltin (f t)
  bimap _ g (ReferenceDerived id) = ReferenceDerived (g <$> id)

instance Bifoldable Reference' where
  bifoldMap f _ (ReferenceBuiltin t) = f t
  bifoldMap _ g (ReferenceDerived id) = foldMap g id

instance Bitraversable Reference' where
  bitraverse f _ (ReferenceBuiltin t) = ReferenceBuiltin <$> f t
  bitraverse _ g (ReferenceDerived id) = ReferenceDerived <$> traverse g id
