module Unison.EffectDeclaration where

import Unison.Type (Type)

data EffectDeclaration v = EffectDeclaration {
  bound :: [v],
  constructors :: [(v, Type v)]
} deriving (Show)
