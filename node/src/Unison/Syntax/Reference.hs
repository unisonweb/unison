{-# LANGUAGE TemplateHaskell #-}

module Unison.Syntax.Reference where

import Data.Aeson.TH
import qualified Data.Text as Text
import qualified Unison.Syntax.Hash as H

data Reference = Builtin Text.Text | Derived H.Hash deriving (Eq,Ord,Show)

deriveJSON defaultOptions ''Reference
