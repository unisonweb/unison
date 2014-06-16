module Unison.Language.Type.Literal where

import Unison.Syntax.Hash as H

-- | Type literals
data Literal
  = Hash H.Hash
  | Number
  | String
  | Vector
