{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}

module Unison.DataDeclaration where

import Unison.Type (Type)
import GHC.Generics
import Data.Aeson

newtype Product v = Product [v] deriving (Functor, Traversable, Foldable, Generic)

data DataDeclaration' name v = Constructors [(name, Product v)] deriving (Functor, Traversable, Foldable, Generic)

type DataDeclaration v = DataDeclaration' v (Type v)

instance FromJSON v => FromJSON (Product v)
instance ToJSON v => ToJSON (Product v)
instance (ToJSON name, ToJSON v) => ToJSON (DataDeclaration' name v)
instance (FromJSON name, FromJSON v) => FromJSON (DataDeclaration' name v)
