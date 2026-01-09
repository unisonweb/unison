-- Types common to multiple versions of Sync
module Unison.SyncCommon.Types
  ( BranchRef (..),
  )
where

import Codec.Serialise (Serialise (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Prelude (From (..))
import Unison.Server.Orphans ()

newtype BranchRef = BranchRef {unBranchRef :: Text}
  deriving (Serialise, Eq, Show, Ord, ToJSON, FromJSON) via Text

instance From (ProjectAndBranch ProjectName ProjectBranchName) BranchRef where
  from pab = BranchRef $ from pab
