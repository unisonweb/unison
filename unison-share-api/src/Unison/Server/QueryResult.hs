module Unison.Server.QueryResult where

import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Server.SearchResult as SR

data QueryResult = QueryResult
  { misses :: [HQ.HashQualified Name],
    hits :: [SR.SearchResult]
  }
