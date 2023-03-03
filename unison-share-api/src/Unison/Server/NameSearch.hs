module Unison.Server.NameSearch where

import Control.Lens
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
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
