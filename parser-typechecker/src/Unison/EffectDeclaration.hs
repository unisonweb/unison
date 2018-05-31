module Unison.EffectDeclaration where

import  Unison.Type (Type)
import  Unison.DataDeclaration

newtype EffectDeclaration v = EffectDeclaration {
  toDataDecl :: DataDeclaration v
} deriving (Show)

mkEffectDecl :: [v] -> [(v, Type v)] -> EffectDeclaration v
mkEffectDecl = (EffectDeclaration .) . DataDeclaration
