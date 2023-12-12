module Unison.Server.NameSearch.Sqlite
  ( resolveShortHash,
    typeReferencesByShortHash,
    termReferentsByShortHash,
    NameSearch (..),
    nameSearchForPerspective,
  )
where

import Control.Lens
import Data.Set qualified as Set
import U.Codebase.Sqlite.NameLookups (PathSegments (..), ReversedName (..))
import U.Codebase.Sqlite.NamedRef qualified as NamedRef
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Builtin qualified as Builtin
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.HashQualified' qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NamesWithHistory (SearchType (ExactName, IncludeSuffixes))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.NameSearch (NameSearch (..), Search (..))
import Unison.Server.SearchResult qualified as SR
import Unison.ShortHash qualified as SH
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Set qualified as Set

nameSearchForPerspective :: Codebase m v a -> Ops.NamesPerspective -> (NameSearch Sqlite.Transaction)
nameSearchForPerspective codebase namesPerspective@Ops.NamesPerspective {pathToMountedNameLookup} = do
  NameSearch {typeSearch, termSearch}
  where
    -- Some searches will provide a fully-qualified name, so we need to strip off the
    -- mount-path before we search or it will fail to find anything.
    stripMountPathPrefix :: Name -> Name
    stripMountPathPrefix name = Name.tryStripReversedPrefix name (reverse $ coerce pathToMountedNameLookup)
    typeSearch =
      Search
        { lookupNames = lookupNamesForTypes,
          lookupRelativeHQRefs' = \searchType n -> hqTypeSearch searchType . fmap stripMountPathPrefix $ n,
          makeResult = \hqname r names -> pure $ SR.typeResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReference
        }
    termSearch =
      Search
        { lookupNames = lookupNamesForTerms,
          lookupRelativeHQRefs' = \searchType n -> hqTermSearch searchType . fmap stripMountPathPrefix $ n,
          makeResult = \hqname r names -> pure $ SR.termResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReferent
        }

    lookupNamesForTypes :: Reference -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTypes ref = do
      names <- Ops.typeNamesForRefWithinNamespace namesPerspective (Cv.reference1to2 ref) Nothing
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Reference.toShortHash ref))
        & Set.fromList
        & pure
    lookupNamesForTerms :: Referent -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTerms ref = do
      names <- Ops.termNamesForRefWithinNamespace namesPerspective (Cv.referent1to2 ref) Nothing
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Referent.toShortHash ref))
        & Set.fromList
        & pure
    -- Search the codebase for matches to the given hq name.
    -- Supports either an exact match or a suffix match.
    hqTermSearch :: SearchType -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
    hqTermSearch searchStrat hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          namedRefs <-
            case searchStrat of
              ExactName -> Ops.termRefsForExactName namesPerspective (coerce $ Name.reverseSegments name)
              IncludeSuffixes -> Ops.termNamesBySuffix namesPerspective (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap
              ( \(NamedRef.ref -> (ref, mayCT)) ->
                  Cv.referent2to1UsingCT (fromMaybe (error "Required constructor type for constructor but it was null") mayCT) ref
              )
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = fullyQualifyName name
          termRefs <- termReferentsByShortHash codebase sh
          Set.forMaybe termRefs \termRef -> do
            matches <- Ops.termNamesForRefWithinNamespace namesPerspective (Cv.referent1to2 termRef) (Just . coerce $ Name.reverseSegments name)
            -- Return a valid ref if at least one match was found. Require that it be an exact
            -- match if specified.
            if any (\n -> coerce (Name.reverseSegments fqn) == n || searchStrat /= ExactName) matches
              then pure (Just termRef)
              else pure Nothing

    -- Search the codebase for matches to the given hq name.
    -- Supports either an exact match or a suffix match.
    hqTypeSearch :: SearchType -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
    hqTypeSearch searchStrat hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          let fqn = fullyQualifyName name
          namedRefs <-
            case searchStrat of
              ExactName -> Ops.typeRefsForExactName namesPerspective (coerce $ Name.reverseSegments fqn)
              IncludeSuffixes -> Ops.typeNamesBySuffix namesPerspective (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap (Cv.reference2to1 . NamedRef.ref)
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = fullyQualifyName name
          typeRefs <- typeReferencesByShortHash sh
          Set.forMaybe typeRefs \typeRef -> do
            matches <- Ops.typeNamesForRefWithinNamespace namesPerspective (Cv.reference1to2 typeRef) (Just . coerce $ Name.reverseSegments name)
            -- Return a valid ref if at least one match was found. Require that it be an exact
            -- match if specified.
            if any (\n -> coerce (Name.reverseSegments fqn) == n || searchStrat /= ExactName) matches
              then pure (Just typeRef)
              else pure Nothing

    reversedSegmentsToName :: ReversedName -> Name
    reversedSegmentsToName = Name.fromReverseSegments . coerce

    -- Fully qualify a name by prepending the current namespace perspective's path
    fullyQualifyName :: Name -> Name
    fullyQualifyName name = Path.prefixName (Path.Absolute (Path.fromList . coerce $ pathToMountedNameLookup)) name

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
