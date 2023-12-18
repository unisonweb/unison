module Unison.Server.NameSearch.FromNames where

import Unison.HashQualified' qualified as HQ'
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Server.NameSearch
import Unison.Server.SearchResult qualified as SR

-- | Make a type search, given a short hash length and names to search in.
makeTypeSearch :: (Applicative m) => Int -> NamesWithHistory -> Search m Reference
makeTypeSearch len names =
  Search
    { lookupNames = \ref -> pure $ NamesWithHistory.typeName len ref names,
      lookupRelativeHQRefs' = \searchType n -> pure $ NamesWithHistory.lookupRelativeHQType' searchType n names,
      matchesNamedRef = HQ'.matchesNamedReference,
      makeResult = \hqname r names -> pure $ SR.typeResult hqname r names
    }

-- | Make a term search, given a short hash length and names to search in.
makeTermSearch :: (Applicative m) => Int -> NamesWithHistory -> Search m Referent
makeTermSearch len names =
  Search
    { lookupNames = \ref -> pure $ NamesWithHistory.termName len ref names,
      lookupRelativeHQRefs' = \searchType n -> pure $ NamesWithHistory.lookupRelativeHQTerm' searchType n names,
      matchesNamedRef = HQ'.matchesNamedReferent,
      makeResult = \hqname r names -> pure $ SR.termResult hqname r names
    }

makeNameSearch :: (Applicative m) => Int -> NamesWithHistory -> NameSearch m
makeNameSearch hashLength names =
  NameSearch
    { typeSearch = makeTypeSearch hashLength names,
      termSearch = makeTermSearch hashLength names
    }
