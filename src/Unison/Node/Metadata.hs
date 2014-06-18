module Unison.Node.Metadata where

import Data.Text

data Metadata k
  = Term Names [Names] k k -- ^ @Term names paramNames description typ
  | Type Names [Names] k k -- ^ @Type names paramNames description kind
  deriving (Eq,Ord,Show,Read)

data Names = Names [Text] deriving (Eq,Ord,Show,Read)

-- data Examples k = Examples [(k, k)]
