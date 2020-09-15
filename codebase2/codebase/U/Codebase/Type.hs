{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Type where

import qualified U.Core.ABT as ABT
import U.Codebase.Reference (Reference, Reference')
import Data.Text (Text)
import U.Util.Hash (Hash)
import U.Codebase.Kind (Kind)
import Unsafe.Coerce (unsafeCoerce)

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

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type TypeT v = ABT.Term FT v ()

type TypeD v = ABT.Term FD v ()

type TypeR r v = ABT.Term (F' r) v ()

rmap :: (r -> r') -> ABT.Term (F' r) v a -> ABT.Term (F' r') v a
rmap f = ABT.extraMap $ \case
  Ref r -> Ref (f r)
  x -> unsafeCoerce x