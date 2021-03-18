
module Unison.Runtime.Exception where

import Control.Exception
import Data.String (fromString)
import Data.Text

import Unison.Runtime.Stack
import Unison.Util.Pretty as P

data RuntimeExn
  = PE (P.Pretty P.ColorText)
  | BU Text Closure
  deriving (Show)
instance Exception RuntimeExn

die :: String -> IO a
die = throwIO . PE . P.lit . fromString

