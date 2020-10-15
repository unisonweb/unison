{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module U.Codebase.Type where

import qualified U.Core.ABT as ABT
import U.Codebase.Reference (Reference, Reference')
import Data.Text (Text)
import U.Util.Hash (Hash)
import U.Codebase.Kind (Kind)
import Unsafe.Coerce (unsafeCoerce)
import U.Util.Hashable (Hashable, Hashable1)
import qualified U.Util.Hashable as Hashable

-- | For standalone types, like those in Term.Ann
type FT = F' Reference

-- | For potentially recursive types, like those in DataDeclaration
type FD = F' (Reference' Text (Maybe Hash))

data F' r a
  = Ref r
  | Arrow a a
  | Ann a Kind
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  | IntroOuter a -- binder like ∀, used to introduce variables that are
      -- bound by outer type signatures, to support scoped type
      -- variables
  deriving (Foldable, Functor, Eq, Ord, Traversable)

-- | Non-recursive type
type TypeT v = ABT.Term FT v ()

-- | Potentially-recursive type
type TypeD v = ABT.Term FD v ()

type TypeR r v = ABT.Term (F' r) v ()

rmap :: Ord v => (r -> r') -> ABT.Term (F' r) v a -> ABT.Term (F' r') v a
rmap f = ABT.transform \case
  Ref r -> Ref (f r)
  x -> unsafeCoerce x

rtraverse :: (Monad g, Ord v) => (r -> g r') -> ABT.Term (F' r) v a -> g (ABT.Term (F' r') v a)
rtraverse g = ABT.transformM \case
  Ref r -> Ref <$> g r
  x -> pure $ unsafeCoerce x

instance Hashable r => Hashable1 (F' r) where
  hash1 hashCycle hash e =
    let
      (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
      -- Note: start each layer with leading `0` byte, to avoid collisions with
      -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
    in Hashable.accumulate $ tag 0 : case e of
      Ref r -> [tag 0, Hashable.accumulateToken r]
      Arrow a b -> [tag 1, hashed (hash a), hashed (hash b) ]
      App a b -> [tag 2, hashed (hash a), hashed (hash b) ]
      Ann a k -> [tag 3, hashed (hash a), Hashable.accumulateToken k ]
      -- Example:
      --   a) {Remote, Abort} (() -> {Remote} ()) should hash the same as
      --   b) {Abort, Remote} (() -> {Remote} ()) but should hash differently from
      --   c) {Remote, Abort} (() -> {Abort} ())
      Effects es -> let
        (hs, _) = hashCycle es
        in tag 4 : map hashed hs
      Effect e t -> [tag 5, hashed (hash e), hashed (hash t)]
      Forall a -> [tag 6, hashed (hash a)]
      IntroOuter a -> [tag 7, hashed (hash a)]
