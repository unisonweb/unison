{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module U.Codebase.Referent where

import Data.Text (Text)
import U.Codebase.Reference (Reference, Reference')
import qualified U.Codebase.Reference as Reference
import U.Util.Hash (Hash)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import U.Codebase.Decl (ConstructorId)

type Referent = GReferent Reference Reference
type ReferentH = GReferent (Reference' Text (Maybe Hash)) (Reference' Text Hash)

data GReferent rTm rTp
  = Ref rTm
  | Con rTp ConstructorId
  deriving (Eq, Ord, Show)

type Id = Id' Hash Hash
data Id' hTm hTp
  = RefId (Reference.Id' hTm)
  | ConId (Reference.Id' hTp) ConstructorId
  deriving (Eq, Ord, Show)

instance Bifunctor GReferent where
  bimap f g = \case
    Ref r -> Ref (f r)
    Con r i -> Con (g r) i

instance Bifoldable GReferent where
  bifoldMap f g = \case
    Ref r -> f r
    Con r _ -> g r

instance Bitraversable GReferent where
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
