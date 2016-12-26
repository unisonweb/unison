{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Unison.DataDeclaration where

import Unison.Type (Type)

newtype Product v = Product [v] deriving (Functor, Traversable, Foldable)

data DataDeclaration' name v = Constructors [(name, Product v)] deriving (Functor, Traversable, Foldable)

type DataDeclaration v = DataDeclaration' v (Type v)
