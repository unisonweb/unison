{-# Language DeriveGeneric #-}

module Unison.Pattern where

import GHC.Generics
import Unison.Literal (Literal)
import Data.Text (Text)
import Data.Aeson

data Pattern
  = Wildcard Text
  | Var
  | Literal Literal
  | Constructor !Int [Pattern] deriving (Generic,Eq,Show)

instance ToJSON Pattern -- yay! autoderived JSON instances
instance FromJSON Pattern
