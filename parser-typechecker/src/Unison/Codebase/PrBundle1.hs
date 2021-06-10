{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.PrBundle1 where

import Data.Aeson (FromJSON, ToJSON)
import Unison.Prelude (Generic, Text)

data Header = Header {version :: Int, baseHash :: Text} deriving (Generic)

instance ToJSON Header

instance FromJSON Header
