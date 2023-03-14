{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Core.Orphans.Sqlite () where

import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))

deriving via Text instance ToField ProjectName

deriving via Text instance FromField ProjectName

deriving via Text instance ToField ProjectBranchName

deriving via Text instance FromField ProjectBranchName
