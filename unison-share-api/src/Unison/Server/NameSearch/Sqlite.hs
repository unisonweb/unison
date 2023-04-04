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
import U.Codebase.HashTags (BranchHash)
import qualified U.Codebase.Sqlite.NamedRef as NamedRef
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified Unison.Builtin as Builtin
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
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
import qualified Unison.Util.Set as Set

data SearchStrategy
  = ExactMatch
  | SuffixMatch
  deriving (Show, Eq)

scopedNameSearch :: Codebase m v a -> BranchHash -> Path -> NameSearch Sqlite.Transaction
scopedNameSearch codebase rootHash path =
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
    lookupNamesForTypes ref = do
      names <- Ops.typeNamesForRefWithinNamespace rootHash pathText (Cv.reference1to2 ref) Nothing
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Reference.toShortHash ref))
        & Set.fromList
        & pure
    lookupNamesForTerms :: Referent -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTerms ref = do
      names <- Ops.termNamesForRefWithinNamespace rootHash pathText (Cv.referent1to2 ref) Nothing
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Referent.toShortHash ref))
        & Set.fromList
        & pure
    -- This is a bit messy, but the existing 'lookupRelativeHQRefs' semantics
    -- will return ONLY exact matches if any exist, otherwise it falls back on
    -- suffix search, so we maintain that behaviour here. It would probably be better
    -- to have separate functions in the Search type for each of these, and be more explicit
    -- about desired behaviour at the call-site.
    lookupRelativeHQRefsForTerms :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
    lookupRelativeHQRefsForTerms hqName = do
      exact <- hqTermSearch ExactMatch hqName
      if Set.null exact
        then do
          hqTermSearch SuffixMatch hqName
        else do
          pure exact
    lookupRelativeHQRefsForTypes :: HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
    lookupRelativeHQRefsForTypes hqName = do
      exact <- hqTypeSearch ExactMatch hqName
      if Set.null exact
        then do
          hqTypeSearch SuffixMatch hqName
        else do
          pure exact
    -- Search the codebase for matches to the given hq name.
    -- Supports either an exact match or a suffix match.
    hqTermSearch :: SearchStrategy -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
    hqTermSearch searchStrat hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          namedRefs <-
            case searchStrat of
              ExactMatch -> Ops.termRefsForExactName rootHash (coerce $ Name.reverseSegments fqn)
              SuffixMatch -> Ops.termNamesBySuffix rootHash pathText (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap
              ( \(NamedRef.ref -> (ref, mayCT)) ->
                  Cv.referent2to1UsingCT (fromMaybe (error "Required constructor type for constructor but it was null") mayCT) ref
              )
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          termRefs <- termReferentsByShortHash codebase sh
          Set.forMaybe termRefs \termRef -> do
            matches <- Ops.termNamesForRefWithinNamespace rootHash pathText (Cv.referent1to2 termRef) (Just . coerce $ Name.reverseSegments name)
            -- Return a valid ref if at least one match was found. Require that it be an exact
            -- match if specified.
            if any (\n -> coerce (Name.reverseSegments fqn) == n || searchStrat /= ExactMatch) matches
              then pure (Just termRef)
              else pure Nothing

    -- Search the codebase for matches to the given hq name.
    -- Supports either an exact match or a suffix match.
    hqTypeSearch :: SearchStrategy -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
    hqTypeSearch searchStrat hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          namedRefs <-
            case searchStrat of
              ExactMatch -> Ops.typeRefsForExactName rootHash (coerce $ Name.reverseSegments fqn)
              SuffixMatch -> Ops.typeNamesBySuffix rootHash pathText (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap (Cv.reference2to1 . NamedRef.ref)
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          typeRefs <- typeReferencesByShortHash sh
          Set.forMaybe typeRefs \typeRef -> do
            matches <- Ops.typeNamesForRefWithinNamespace rootHash pathText (Cv.reference1to2 typeRef) (Just . coerce $ Name.reverseSegments name)
            -- Return a valid ref if at least one match was found. Require that it be an exact
            -- match if specified.
            if any (\n -> coerce (Name.reverseSegments fqn) == n || searchStrat /= ExactMatch) matches
              then pure (Just typeRef)
              else pure Nothing

    reversedSegmentsToName :: NamedRef.ReversedSegments -> Name
    reversedSegmentsToName = Name.fromReverseSegments . coerce

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
