module Unison.Server.NameSearch.Sqlite where

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

scopedNameSearch :: Path -> NameSearch Sqlite.Transaction
scopedNameSearch path =
  NameSearch {typeSearch, termSearch}
    where
      typeSearch =
        Search {
        lookupNames = lookupNamesForTypes,
        lookupRelativeHQRefs' = lookupRelativeHQRefsForTypes,
        makeResult = \hqname r names -> pure $ SR.typeResult hqname r names,
        matchesNamedRef = HQ'.matchesNamedReference
               }
      termSearch = Search {
          lookupNames = lookupNamesForTerms,
          lookupRelativeHQRefs' = lookupRelativeHQRefsForTerms,
          makeResult = \hqname r names -> pure $ SR.termResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReferent
                          }

      pathText :: Text
      pathText = (Path.toText path)
      lookupNamesForTypes :: Reference -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
      lookupNamesForTypes ref = Ops.typeNamesWithinNamespace pathText (Cv.reference1to2 ref)
      lookupNamesForTerms :: Referent -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
      lookupNamesForTerms ref = Ops.termNamesWithinNamespace pathText (Cv.referent1to2 ref)
      lookupRelativeHQRefsForTypes :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
      lookupRelativeHQRefsForTypes name = _
      lookupRelativeHQRefsForTerms :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
      lookupRelativeHQRefsForTerms name = _


