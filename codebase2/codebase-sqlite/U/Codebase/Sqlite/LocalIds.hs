module U.Codebase.Sqlite.LocalIds where

import Data.Vector (Vector)
import U.Codebase.Sqlite.DbId

-- |A mapping between index ids that are local to an object and the ids in the database
data LocalIds = LocalIds
  { textLookup :: Vector TextId,
    objectLookup :: Vector ObjectId
  }

