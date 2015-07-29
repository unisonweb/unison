{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.TermEdit.Extra where

import Data.Bytes.Serial
import Unison.Symbol.Extra ()
import Unison.TermEdit

instance Serial v => Serial (Action v)
