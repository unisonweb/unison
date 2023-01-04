module Unison.Server.NameSearch where

import Control.Lens
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Server.SearchResult as SR

-- | A @Search r@ is a small bag of functions that is used to power a search for @r@s.
--
-- Construct a 'Search' with 'makeTypeSearch' or 'makeTermSearch', and eliminate it with 'applySearch'.
data Search m r = Search
  { lookupNames :: r -> m (Set (HQ'.HashQualified Name)),
    lookupRelativeHQRefs' :: HQ'.HashQualified Name -> m (Set r),
    makeResult :: HQ.HashQualified Name -> r -> Set (HQ'.HashQualified Name) -> m SR.SearchResult,
    matchesNamedRef :: Name -> r -> HQ'.HashQualified Name -> Bool
  }

data NameSearch m = NameSearch
  { typeSearch :: Search m Reference,
    termSearch :: Search m Referent
  }

-- | Make a type search, given a short hash length and names to search in.
makeTypeSearch :: Applicative m => Int -> NamesWithHistory -> Search m Reference
makeTypeSearch len names =
  Search
    { lookupNames = \ref -> pure $ NamesWithHistory.typeName len ref names,
      lookupRelativeHQRefs' = pure . (`NamesWithHistory.lookupRelativeHQType'` names),
      matchesNamedRef = HQ'.matchesNamedReference,
      makeResult = \hqname r names -> pure $ SR.typeResult hqname r names
    }

-- | Make a term search, given a short hash length and names to search in.
makeTermSearch :: Applicative m => Int -> NamesWithHistory -> Search m Referent
makeTermSearch len names =
  Search
    { lookupNames = \ref -> pure $ NamesWithHistory.termName len ref names,
      lookupRelativeHQRefs' = pure . (`NamesWithHistory.lookupRelativeHQTerm'` names),
      matchesNamedRef = HQ'.matchesNamedReferent,
      makeResult = \hqname r names -> pure $ SR.termResult hqname r names
    }

makeNameSearch :: Applicative m => Int -> NamesWithHistory -> NameSearch m
makeNameSearch hashLength names =
  NameSearch
    { typeSearch = makeTypeSearch hashLength names,
      termSearch = makeTermSearch hashLength names
    }

-- | Interpret a 'Search' as a function from name to search results.
applySearch :: (Show r, Monad m) => Search m r -> HQ'.HashQualified Name -> m [SR.SearchResult]
applySearch Search {lookupNames, lookupRelativeHQRefs', makeResult, matchesNamedRef} query = do
  refs <- (lookupRelativeHQRefs' query)
  -- a bunch of references will match a HQ ref.
  for (toList refs) \ref -> do
    let -- Precondition: the input set is non-empty
        prioritize :: Set (HQ'.HashQualified Name) -> (HQ'.HashQualified Name, Set (HQ'.HashQualified Name))
        prioritize =
          Set.toList
            >>> sortOn (\n -> matchesNamedRef (HQ'.toName n) ref query)
            >>> List.uncons
            >>> fromMaybe (error (reportBug "E839404" ("query = " ++ show query ++ ", ref = " ++ show ref)))
            >>> over _2 Set.fromList
    names <- lookupNames ref
    let (primaryName, aliases) =
          -- The precondition of `prioritize` should hold here because we are passing in the set of names that are
          -- related to this ref, which is itself one of the refs that the query name was related to! (Hence it should
          -- be non-empty).
          prioritize names
    makeResult (HQ'.toHQ primaryName) ref aliases
