module Unison.Runtime.Exception where

import Control.Exception
import Data.String (fromString)
import Data.Text
import GHC.Stack
import Unison.Reference (Reference)
import Unison.Runtime.Stack
import Unison.Util.Pretty as P

data RuntimeExn
  = PE CallStack (P.Pretty P.ColorText)
  | BU [(Reference, Int)] Text Closure
  deriving (Show)

instance Exception RuntimeExn

die :: (HasCallStack) => String -> IO a
die = throwIO . PE callStack . P.lit . fromString

exn :: (HasCallStack) => String -> a
exn = throw . PE callStack . P.lit . fromString
