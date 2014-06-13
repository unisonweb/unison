module Unison.Node.Metadata where

import Data.Text

data Metadata k
  = Term Names Names k k -- ^ @Term names paramNames description typ
  | Type Names Names k k -- ^ @Type names paramNames description kind

data Names = Names [Text]

-- data Examples k = Examples [(k, k)]
