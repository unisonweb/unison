module Unison.Server.NameSearch.Sqlite
  ( resolveShortHash,
    typeReferencesByShortHash,
    termReferentsByShortHash,
    NameSearch (..),
    scopedNameSearch,
  )
where

import Control.Lens
import qualified Data.Set as Set
import qualified Data.Text as Text
import U.Codebase.HashTags (BranchHash)
import qualified U.Codebase.Sqlite.NamedRef as NamedRef
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified Unison.Builtin as Builtin
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Debug as Debug
import qualified Unison.HashQualified' as HQ'
import qualified Unison.LabeledDependency as LD
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
import qualified Unison.ShortHash as SH
import qualified Unison.Sqlite as Sqlite

scopedNameSearch :: BranchHash -> Path -> NameSearch Sqlite.Transaction
scopedNameSearch rootHash path =
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
    pathText = Path.toText path
    lookupNamesForTypes :: Reference -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTypes ref = track "lookupNamesForTypes" do
      names <- Ops.typeNamesWithinNamespace rootHash pathText (Cv.reference1to2 ref)
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Reference.toShortHash ref))
        & Set.fromList
        & pure
    lookupNamesForTerms :: Referent -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTerms ref = track "lookupNamesForTerms" do
      names <- Ops.termNamesWithinNamespace rootHash pathText (Cv.referent1to2 ref)
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Referent.toShortHash ref))
        & Set.fromList
        & pure
    lookupRelativeHQRefsForTypes :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
    lookupRelativeHQRefsForTypes hqName = track "lookupRelativeHQRefsForTypes" do
      namedRefs <- case hqName of
        HQ'.NameOnly name -> do
          Ops.typeNamesBySuffix rootHash pathText (coerce $ Name.reverseSegments name)
        HQ'.HashQualified name sh -> do
          let sh2 = either (error . Text.unpack) id $ Cv.shorthash1to2 sh
          Ops.typeNamesByShortHash rootHash pathText sh2 (Just . coerce $ Name.reverseSegments name)
      namedRefs
        & fmap (Cv.reference2to1 . NamedRef.ref)
        & Set.fromList
        & pure
    lookupRelativeHQRefsForTerms :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
    lookupRelativeHQRefsForTerms hqName = track "lookupRelativeHQRefsForTerms" do
      namedRefs <- case hqName of
        HQ'.NameOnly name -> do
          Ops.termNamesBySuffix rootHash pathText (coerce $ Name.reverseSegments name)
        HQ'.HashQualified name sh -> do
          let sh2 = either (error . Text.unpack) id $ Cv.shorthash1to2 sh
          Ops.termNamesByShortHash rootHash pathText sh2 (Just . coerce $ Name.reverseSegments name)
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

-- | Look up types in the codebase by short hash, and include builtins.
typeReferencesByShortHash :: SH.ShortHash -> Sqlite.Transaction (Set Reference)
typeReferencesByShortHash sh = do
  fromCodebase <- Codebase.typeReferencesByPrefix sh
  let fromBuiltins =
        Set.filter
          (\r -> sh == Reference.toShortHash r)
          Builtin.intrinsicTypeReferences
  pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)

-- | Look up terms in the codebase by short hash, and include builtins.
termReferentsByShortHash :: Codebase m v a -> SH.ShortHash -> Sqlite.Transaction (Set Referent)
termReferentsByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferentsByPrefix codebase sh
  let fromBuiltins =
        Set.map Referent.Ref $
          Set.filter
            (\r -> sh == Reference.toShortHash r)
            Builtin.intrinsicTermReferences
  pure (fromBuiltins <> Set.mapMonotonic (over Referent.reference_ Reference.DerivedId) fromCodebase)

-- | Resolves a shorthash into any possible matches.
resolveShortHash :: Codebase m v a -> SH.ShortHash -> Sqlite.Transaction (Set LD.LabeledDependency)
resolveShortHash codebase sh = do
  terms <- Set.map LD.TermReferent <$> termReferentsByShortHash codebase sh
  types <- Set.map LD.TypeReference <$> typeReferencesByShortHash sh
  pure $ terms <> types
