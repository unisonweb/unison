{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Distance.Extra where

import Unison.Distance
import Data.Bytes.Serial (Serial)
instance Serial Distance
