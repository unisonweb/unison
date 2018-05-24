{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}

module Unison.DataDeclaration where

import Unison.Type (Type)
import GHC.Generics

data DataDeclaration v = DataDeclaration {
  bound :: [v],
  constructors :: [(v, [Type v])]
} deriving (Show)
