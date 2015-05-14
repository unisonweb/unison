{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Kind where

import GHC.Generics
import Data.Aeson.TH

data Kind = Star | Constraint | Arrow Kind Kind deriving (Eq,Ord,Read,Show,Generic)

deriveJSON defaultOptions ''Kind
