{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.TermEdit.Extra where

import Data.Bytes.Serial
import Unison.TermEdit

instance Serial Action
