{-# LANGUAGE DataKinds #-}

module U.Codebase.Referent where

import Control.Lens (Prism)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Generics.Sum (_Ctor)
import U.Codebase.Decl (ConstructorId)
import U.Codebase.Reference (Reference, Reference')
import U.Codebase.Reference qualified as Reference
import U.Codebase.ShortHash (ShortHash)
import U.Codebase.ShortHash qualified as SH
import Unison.Hash (Hash)
import Unison.Prelude

data ConstructorType
  = DataConstructor
  | EffectConstructor
  deriving (Show, Eq, Ord)

type Referent = Referent' Reference Reference.Id

type ReferentH = Referent' (Reference' Text (Maybe Hash)) Reference.Id

data Referent' termRef typeRef
  = Ref termRef
  | Con typeRef ConstructorId
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

_Ref :: Prism (Referent' tmr tyr) (Referent' tmr' tyr) tmr tmr'
_Ref = _Ctor @"Ref"

_Con :: Prism (Referent' tmr tyr) (Referent' tmr tyr') (tyr, ConstructorId) (tyr', ConstructorId)
_Con = _Ctor @"Con"

toReference :: Referent -> Reference
toReference = \case
  Ref termRef -> termRef
  Con typeRef _ -> Reference.ReferenceDerived typeRef

toTypeReference :: Referent -> Maybe Reference.Id
toTypeReference = \case
  Ref {} -> Nothing
  Con r _ -> Just r

toTermReference :: Referent' termRef typeRef -> Maybe termRef
toTermReference = \case
  Ref termRef -> Just termRef
  Con _ _ -> Nothing

type Id = Id' Hash Hash

data Id' hTm hTp
  = RefId (Reference.Id' hTm)
  | ConId (Reference.Id' hTp) ConstructorId
  deriving (Eq, Ord, Show)

instance Bifunctor Referent' where
  bimap f g = \case
    Ref r -> Ref (f r)
    Con r i -> Con (g r) i

instance Bifoldable Referent' where
  bifoldMap f g = \case
    Ref r -> f r
    Con r _ -> g r

instance Bitraversable Referent' where
  bitraverse f g = \case
    Ref r -> Ref <$> f r
    Con r c -> flip Con c <$> g r

instance Bifunctor Id' where
  bimap f g = \case
    RefId r -> RefId (fmap f r)
    ConId r j -> ConId (fmap g r) j

instance Bifoldable Id' where
  bifoldMap f g = \case
    RefId r -> foldMap f r
    ConId r _ -> foldMap g r

instance Bitraversable Id' where
  bitraverse f g = \case
    RefId r -> RefId <$> traverse f r
    ConId r c -> flip ConId c <$> traverse g r

toShortHash :: Referent -> ShortHash
toShortHash = \case
  Ref r -> Reference.toShortHash r
  Con r conId ->
    case Reference.idToShortHash r of
      SH.ShortHash prefix cycle Nothing -> SH.ShortHash prefix cycle (Just conId)
      _ -> error $ "unexpected output calling idToShortHash on " ++ show r
