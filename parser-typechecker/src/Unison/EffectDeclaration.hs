module Unison.EffectDeclaration where

import Unison.Type (Type)

newtype EffectDeclaration v = EffectDeclaration [(v, Type v)]
  deriving (Show)
