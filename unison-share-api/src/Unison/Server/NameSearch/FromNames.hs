module Unison.Server.NameSearch.FromNames where

import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as Names
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Server.NameSearch
import Unison.Server.SearchResult qualified as SR

-- | Make a type search, given a short hash length and names to search in.
makeTypeSearch :: (Applicative m) => Int -> Names -> Search m Reference
makeTypeSearch len names =
  Search
    { lookupNames = \ref -> pure $ Names.typeName len ref names,
      lookupRelativeHQRefs' = \searchType n -> pure $ Names.lookupRelativeHQType' searchType n names,
      matchesNamedRef = HQ'.matchesNamedReference,
      makeResult = \hqname r names -> pure $ SR.typeResult hqname r names
    }

-- | Make a term search, given a short hash length and names to search in.
makeTermSearch :: (Applicative m) => Int -> Names -> Search m Referent
makeTermSearch len names =
  Search
    { lookupNames = \ref -> pure $ Names.termName len ref names,
      lookupRelativeHQRefs' = \searchType n -> pure $ Names.lookupRelativeHQTerm' searchType n names,
      matchesNamedRef = HQ'.matchesNamedReferent,
      makeResult = \hqname r names -> pure $ SR.termResult hqname r names
    }

makeNameSearch :: (Applicative m) => Int -> Names -> NameSearch m
makeNameSearch hashLength names =
  NameSearch
    { typeSearch = makeTypeSearch hashLength names,
      termSearch = makeTermSearch hashLength names
    }
