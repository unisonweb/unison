{-# Language DeriveGeneric #-}

module Unison.Pattern where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Unison.Literal (Literal)
import Unison.Reference (Reference)

data Pattern
  = Wildcard !Text -- added Text just for comment / annotation
  | Var
  | Literal !Literal
  | Constructor !Reference !Int [Pattern] deriving (Generic,Eq,Show)

instance ToJSON Pattern -- yay! autoderived JSON instances
instance FromJSON Pattern
