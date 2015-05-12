{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Kind where

import GHC.Generics
import Data.Aeson.TH
import Data.Bytes.Serial

data Kind = Star | Constraint | Arrow Kind Kind deriving (Eq,Ord,Read,Show,Generic)

instance Serial Kind
deriveJSON defaultOptions ''Kind
