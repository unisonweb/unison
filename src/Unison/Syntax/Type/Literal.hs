module Unison.Syntax.Type.Literal where

import Unison.Syntax.Hash as H

-- | Type literals
data Literal
  = Hash H.Hash
  | Number
  | String
  | Vector
  deriving (Eq,Ord,Show)
