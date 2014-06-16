module Unison.Syntax.Term.Literal where

import Data.Text
import Data.Vector.Unboxed as V
import Unison.Syntax.Hash as H

data Literal
  = Hash H.Hash
  | Number Double
  | String Text
  | Vector (V.Vector Double)
  deriving (Eq,Ord,Show)
