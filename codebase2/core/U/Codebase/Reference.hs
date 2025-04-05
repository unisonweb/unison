module U.Codebase.Reference
  ( Reference,
    RReference,
    TermReference,
    TermRReference,
    TermReferenceId,
    TypeReference,
    TypeRReference,
    TypeReferenceId,
    Reference' (..),
    TermReference',
    TypeReference',
    ReferenceType (..),
    pattern Derived,
    Id,
    Id' (..),
    Pos,
    _ReferenceDerived,
    _RReferenceReference,
    t_,
    h_,
    idH,
    idPos,
    idToHash,
    idToShortHash,
    isBuiltin,
    toShortHash,
    toId,
    unsafeId,
    component,
  )
where

import Control.Lens (Lens, Lens', Prism, Prism', Traversal, lens, preview, prism)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Text qualified as Text
import Unison.Hash (Hash)
import Unison.Hash qualified as H
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH

-- | This is the canonical representation of Reference
type Reference = Reference' Text Hash

-- | A possibly-self (R = "recursive") reference.
type RReference = Reference' Text (Maybe Hash)

-- | A term reference.
type TermReference = Reference

-- | A possibly-self term reference.
type TermRReference = RReference

-- | A type declaration reference.
type TypeReference = Reference

-- | A possibly-self type declaration reference.
type TypeRReference = RReference

type Id = Id' Hash

-- | A term reference id.
type TermReferenceId = Id

-- | A type declaration reference id.
type TypeReferenceId = Id

data ReferenceType = RtTerm | RtType deriving (Eq, Ord, Show)

-- | Either a builtin or a user defined (hashed) top-level declaration. Used for both terms and types.
data Reference' t h
  = ReferenceBuiltin t
  | ReferenceDerived (Id' h)
  deriving stock (Eq, Generic, Functor, Ord, Show)

-- | A type declaration reference.
type TermReference' t h = Reference' t h

-- | A term declaration reference.
type TypeReference' t h = Reference' t h

_RReferenceReference :: Prism' (Reference' t (Maybe h)) (Reference' t h)
_RReferenceReference = prism embed project
  where
    embed = \case
      ReferenceBuiltin x -> ReferenceBuiltin x
      ReferenceDerived (Id h p) -> ReferenceDerived (Id (Just h) p)

    project = \case
      ReferenceBuiltin x -> Right (ReferenceBuiltin x)
      ReferenceDerived (Id mh p) -> case mh of
        Nothing -> Left (ReferenceDerived (Id mh p))
        Just h -> Right (ReferenceDerived (Id h p))

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

-- | @Pos@ is a position into a cycle, as cycles are hashed together.
data Id' h = Id h Pos
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

t_ :: Prism (Reference' t h) (Reference' t' h) t t'
t_ = prism ReferenceBuiltin \case
  ReferenceBuiltin t -> Right t
  ReferenceDerived id -> Left (ReferenceDerived id)

h_ :: Traversal (Reference' t h) (Reference' t h') h h'
h_ f = \case
  ReferenceBuiltin t -> pure (ReferenceBuiltin t)
  Derived h i -> Derived <$> f h <*> pure i

idPos :: Lens' (Id' h) Pos
idPos = lens (\(Id _h w) -> w) (\(Id h _w) w -> Id h w)

idH :: Lens (Id' h) (Id' h') h h'
idH = lens (\(Id h _w) -> h) (\(Id _h w) h -> Id h w)

idToHash :: Id -> Hash
idToHash (Id h _) = h

idToShortHash :: Id -> ShortHash
idToShortHash = toShortHash . ReferenceDerived

isBuiltin :: Reference -> Bool
isBuiltin (ReferenceBuiltin _) = True
isBuiltin _ = False

toId :: Reference -> Maybe Id
toId =
  preview _ReferenceDerived

toShortHash :: Reference -> ShortHash
toShortHash = \case
  ReferenceBuiltin b -> SH.Builtin b
  ReferenceDerived (Id h 0) -> SH.ShortHash (Hash.toBase32HexText h) Nothing Nothing
  ReferenceDerived (Id h i) -> SH.ShortHash (Hash.toBase32HexText h) (Just i) Nothing

unsafeId :: Reference -> Id
unsafeId = \case
  ReferenceBuiltin b -> error $ "Tried to get the hash of builtin " <> Text.unpack b <> "."
  ReferenceDerived x -> x

instance Bifunctor Reference' where
  bimap f _ (ReferenceBuiltin t) = ReferenceBuiltin (f t)
  bimap _ g (ReferenceDerived id) = ReferenceDerived (g <$> id)

instance Bifoldable Reference' where
  bifoldMap f _ (ReferenceBuiltin t) = f t
  bifoldMap _ g (ReferenceDerived id) = foldMap g id

instance Bitraversable Reference' where
  bitraverse f _ (ReferenceBuiltin t) = ReferenceBuiltin <$> f t
  bitraverse _ g (ReferenceDerived id) = ReferenceDerived <$> traverse g id

component :: H.Hash -> [k] -> [(k, Id)]
component h ks =
  let
   in [(k, (Id h i)) | (k, i) <- ks `zip` [0 ..]]
