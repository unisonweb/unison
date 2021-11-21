{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}

module U.Codebase.Referent where

import U.Codebase.Reference (Reference, Reference')
import qualified U.Codebase.Reference as Reference
import U.Util.Hash (Hash)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import U.Codebase.Decl (ConstructorId)
import Control.Lens (Prism, Traversal)
import Data.Generics.Sum (_Ctor)
import Unison.Prelude

type Referent = Referent' Reference Reference
type ReferentH = Referent' (Reference' Text (Maybe Hash)) (Reference' Text Hash)

data Referent' termRef typeRef
  = Ref termRef
  | Con typeRef ConstructorId
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

refs_ :: Traversal (Referent' ref ref) (Referent' ref' ref') ref ref'
refs_ f r = bitraverse f f r

typeRef_ :: Traversal (Referent' typeRef termRef) (Referent' typeRef' termRef) typeRef typeRef'
typeRef_ f = bitraverse f pure

termRef_ :: Traversal (Referent' typeRef termRef) (Referent' typeRef termRef') termRef termRef'
termRef_ f = bitraverse pure f

_Ref :: Prism (Referent' tmr tyr) (Referent' tmr' tyr) tmr tmr'
_Ref = _Ctor @"Ref"

_Con :: Prism (Referent' tmr tyr) (Referent' tmr tyr') (tyr, ConstructorId) (tyr', ConstructorId)
_Con = _Ctor @"Con"

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
