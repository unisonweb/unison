module U.Codebase.Reference
  ( Reference,
    RReference,
    TermReference,
    TermRReference,
    TypeReference,
    TypeRReference,
    Reference' (..),
    pattern Derived,
    Id,
    Id' (..),
    Pos,
    _ReferenceDerived,
    t_,
    h_,
    idH,
    isBuiltin,
    toShortHash,
  )
where

import Control.Lens (Lens, Prism, Traversal, lens, prism)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import U.Codebase.ShortHash (ShortHash)
import U.Codebase.ShortHash qualified as SH
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Prelude

-- | This is the canonical representation of Reference
type Reference = Reference' Text Hash

-- | A possibly-self (R = "recursive") reference.
type RReference = Reference' Text (Maybe Hash)

-- | A term reference.
type TermReference = Reference

-- | A possibly-self term reference.
type TermRReference = RReference

-- | A type reference.
type TypeReference = Reference

-- | A possibly-self type reference.
type TypeRReference = RReference

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
isBuiltin = \case
  ReferenceBuiltin {} -> True
  ReferenceDerived {} -> False

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
