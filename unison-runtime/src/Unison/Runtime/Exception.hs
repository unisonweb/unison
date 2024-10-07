module Unison.Runtime.Exception2 where

import Control.Exception
import Data.String (fromString)
import Data.Text
import GHC.Stack
import Unison.Reference (Reference)
import Unison.Runtime.Stack2
import Unison.Util.Pretty as P

data RuntimeExn
  = PE CallStack (P.Pretty P.ColorText)
  | BU [(Reference, Int)] Text Closure
  deriving (Show)

instance Exception RuntimeExn

die :: (HasCallStack) => String -> IO a
die = throwIO . PE callStack . P.lit . fromString

dieP :: (HasCallStack) => P.Pretty P.ColorText -> IO a
dieP = throwIO . PE callStack

exn :: (HasCallStack) => String -> a
exn = throw . PE callStack . P.lit . fromString
