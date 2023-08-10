module Unison.Server.QueryResult where

import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Server.SearchResult qualified as SR

data QueryResult = QueryResult
  { misses :: [HQ.HashQualified Name],
    hits :: [SR.SearchResult]
  }
