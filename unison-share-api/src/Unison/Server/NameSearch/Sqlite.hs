module Unison.Server.NameSearch.Sqlite where

import Control.Lens
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified U.Codebase.Sqlite.NamedRef as NamedRef
import qualified U.Codebase.Sqlite.Operations as Ops
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Server.NameSearch (NameSearch (..), Search (..))
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Debug as Debug

scopedNameSearch :: Path -> NameSearch Sqlite.Transaction
scopedNameSearch path =
  NameSearch {typeSearch, termSearch}
  where
    typeSearch =
      Search
        { lookupNames = lookupNamesForTypes,
          lookupRelativeHQRefs' = lookupRelativeHQRefsForTypes,
          makeResult = \hqname r names -> pure $ SR.typeResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReference
        }
    termSearch =
      Search
        { lookupNames = lookupNamesForTerms,
          lookupRelativeHQRefs' = lookupRelativeHQRefsForTerms,
          makeResult = \hqname r names -> pure $ SR.termResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReferent
        }

    pathText :: Text
    pathText = (Path.toText path)
    lookupNamesForTypes :: Reference -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTypes ref = track "lookupNamesForTypes" do
      names <- Ops.typeNamesWithinNamespace pathText (Cv.reference1to2 ref)
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Reference.toShortHash ref))
        & Set.fromList
        & pure
    lookupNamesForTerms :: Referent -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTerms ref = track "lookupNamesForTerms" do
      names <- Ops.termNamesWithinNamespace pathText (Cv.referent1to2 ref)
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Referent.toShortHash ref))
        & Set.fromList
        & pure
    lookupRelativeHQRefsForTypes :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
    lookupRelativeHQRefsForTypes hqName =  track "lookupRelativeHQRefsForTypes" do
      namedRefs <- case hqName of
        HQ'.NameOnly name -> do
          Ops.typeNamesBySuffix pathText (coerce $ Name.reverseSegments name)
        HQ'.HashQualified name sh -> do
          let sh2 = (either (error . Text.unpack) id $ Cv.shorthash1to2 sh)
          Ops.typeNamesByShortHash pathText sh2 (Just . coerce $ Name.reverseSegments name)
      namedRefs
        & fmap (Cv.reference2to1 . NamedRef.ref)
        & Set.fromList
        & pure
    lookupRelativeHQRefsForTerms :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
    lookupRelativeHQRefsForTerms hqName = track "lookupRelativeHQRefsForTerms" do
      namedRefs <- case hqName of
        HQ'.NameOnly name -> do
          Ops.termNamesBySuffix pathText (coerce $ Name.reverseSegments name)
        HQ'.HashQualified name sh -> do
          let sh2 = (either (error . Text.unpack) id $ Cv.shorthash1to2 sh)
          Ops.termNamesByShortHash pathText sh2 (Just . coerce $ Name.reverseSegments name)
      namedRefs
        & fmap
          ( \(NamedRef.ref -> (ref, mayCT)) ->
              Cv.referent2to1UsingCT (fromMaybe (error "Required constructor type for constructor but it was null") mayCT) ref
          )
        & Set.fromList
        & pure
    reversedSegmentsToName :: NamedRef.ReversedSegments -> Name
    reversedSegmentsToName = Name.fromReverseSegments . coerce

    track :: String -> Sqlite.Transaction a -> Sqlite.Transaction a
    track name m = do
      Debug.debugLogM Debug.Server $ "Starting " <> name
      r <- m
      Debug.debugLogM Debug.Server $ "Finished " <> name
      pure r
