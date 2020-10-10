{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module U.Codebase.Referent where

import Data.Text (Text)
import U.Codebase.Reference (Reference, Reference')
import qualified U.Codebase.Reference as Reference
import U.Util.Hash (Hash)
import U.Util.Hashable (Hashable (..))
import Data.Word (Word64)
import qualified U.Util.Hashable as Hashable

type Referent = Referent' Reference Reference
type ReferentH = Referent' (Reference' Text (Maybe Hash)) (Reference' Text Hash)

type ConstructorIndex = Word64

data Referent' rTm rTp
  = Ref rTm
  | Con rTp ConstructorIndex
  deriving (Eq, Ord, Show)

type Id = Id' Hash Hash
data Id' hTm hTp
  = RefId (Reference.Id' hTm)
  | ConId (Reference.Id' hTp) ConstructorIndex
  deriving (Eq, Ord, Show, Functor)

instance (Hashable rTm, Hashable rTp) => Hashable (Referent' rTm rTp) where
  tokens (Ref r) = Hashable.Tag 0 : Hashable.tokens r
  tokens (Con r i) = [Hashable.Tag 1] ++ Hashable.tokens r ++ [Hashable.Nat (fromIntegral i)]
