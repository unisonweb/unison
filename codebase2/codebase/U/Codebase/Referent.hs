{-# LANGUAGE DataKinds #-}

module U.Codebase.Referent where

import Control.Lens (Prism, Prism', Traversal, preview, prism, review)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Generics.Sum (_Ctor)
import U.Codebase.Reference (Reference, Reference'(..), TermRReference, TermReference, TypeReference)
import U.Codebase.Reference qualified as Reference
import Unison.Core.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH

type Referent = Referent' TermReference TypeReference

type ReferentH = Referent' TermRReference TypeReference

data Referent' termRef typeRef
  = Ref termRef
  | Con typeRef ConstructorId
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

refs_ :: Traversal (Referent' ref ref) (Referent' ref' ref') ref ref'
refs_ f r = bitraverse f f r

termRef_ :: Traversal (Referent' termRef typeRef) (Referent' termRef' typeRef) termRef termRef'
termRef_ f = bitraverse f pure

typeRef_ :: Traversal (Referent' termRef typeRef) (Referent' termRef typeRef') typeRef typeRef'
typeRef_ f = bitraverse pure f

_Ref :: Prism (Referent' tmr tyr) (Referent' tmr' tyr) tmr tmr'
_Ref = _Ctor @"Ref"

_Con :: Prism (Referent' tmr tyr) (Referent' tmr tyr') (tyr, ConstructorId) (tyr', ConstructorId)
_Con = _Ctor @"Con"

_ReferentHReferent :: Prism' ReferentH Referent
_ReferentHReferent = prism embed project
  where
    embed = \case
      Ref termRef -> Ref (review Reference._RReferenceReference termRef)
      Con r cid -> Con r cid

    project = \case
      Con r cid -> Right (Con r cid)
      Ref r -> case preview Reference._RReferenceReference r of
        Nothing -> Left (Ref r)
        Just r -> Right (Ref r)

toReference :: Referent -> Reference
toReference = \case
  Ref termRef -> termRef
  Con typeRef _ -> typeRef

toReferenceId :: Referent -> Maybe Reference.Id
toReferenceId = \case
  Ref termRef -> Reference.toId termRef
  Con typeRef _ -> Reference.toId typeRef

fromTermReferenceId :: Reference.TermReferenceId -> Referent
fromTermReferenceId = Ref . ReferenceDerived

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
    case Reference.toShortHash r of
      SH.Builtin b -> SH.Builtin b
      SH.ShortHash prefix cycle _cid -> SH.ShortHash prefix cycle (Just conId)
