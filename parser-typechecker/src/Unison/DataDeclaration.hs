{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}

module Unison.DataDeclaration where

import Unison.Type (Type)
import GHC.Generics

newtype Product v = Product [v]
  deriving (Functor, Show, Traversable, Foldable, Generic)

data DataDeclaration' name v = Constructors [(name, Product v)]
  deriving (Functor, Show, Traversable, Foldable, Generic)

type DataDeclaration v = DataDeclaration' v (Type v)
