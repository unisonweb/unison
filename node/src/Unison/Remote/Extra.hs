{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Remote.Extra where

import Unison.Remote
import Data.Bytes.Serial

instance Serial t => Serial (Remote t)
instance Serial1 Remote
instance Serial1 Step
instance Serial1 Local
instance Serial t => Serial (Step t)
instance Serial t => Serial (Local t)
instance Serial Timeout
instance Serial Base64
instance Serial Node
instance Serial Channel where
