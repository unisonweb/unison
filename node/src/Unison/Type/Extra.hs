{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Type.Extra where

import Data.Bytes.Serial
import Unison.Kind.Extra ()
import Unison.Reference.Extra ()
import Unison.Type

instance Serial Literal
instance Serial1 F
