module Unison.PrettyPrintEnvDecl.Sqlite where

import U.Codebase.HashTags (BranchHash)
import U.Codebase.Sqlite.NameLookups (PathSegments (PathSegments), ReversedName (..))
import U.Codebase.Sqlite.NamedRef (NamedRef (..))
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (foldMapM)

-- | Given a set of references, return a PPE which contains names for only those references.
-- Names are limited to those within the provided perspective
ppedForReferences :: BranchHash -> Path -> Set LabeledDependency -> Sqlite.Transaction PPED.PrettyPrintEnvDecl
ppedForReferences rootHash perspective refs = do
  hashLen <- Codebase.hashLength
  (termNames, typeNames) <-
    refs & foldMapM \ref ->
      namesForReference ref

  -- Ideally we'd only suffixify the name we're actually going to use, but due to name biasing
  -- we won't know that until we actually call the pretty-printer, so
  -- we add suffixifications for every name we have for each reference.
  longestTermSuffixMatches <- forMaybe termNames \(name, ref) -> do
    result <-
      Ops.longestMatchingTermNameForSuffixification rootHash pathSegments (NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = Cv.referent1to2 ref})
        <&> fmap \(NamedRef {reversedSegments, ref = (ref, mayCt)}) ->
          let ct = fromMaybe (error "ppedForReferences: Required constructor type for constructor but it was null") mayCt
           in (Name.fromReverseSegments (coerce reversedSegments), Cv.referent2to1UsingCT ct ref)
    pure result
  longestTypeSuffixMatches <- forMaybe typeNames \(name, ref) -> do
    result <-
      Ops.longestMatchingTypeNameForSuffixification rootHash pathSegments (NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = Cv.reference1to2 ref})
        <&> fmap \(NamedRef {reversedSegments, ref}) ->
          (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
    pure result
  let allTermNamesToConsider = termNames <> longestTermSuffixMatches
  let allTypeNamesToConsider = typeNames <> longestTypeSuffixMatches
  pure . PPED.fromNamesDecl hashLen . NamesWithHistory.fromCurrentNames $ Names.fromTermsAndTypes allTermNamesToConsider allTypeNamesToConsider
  where
    pathSegments :: PathSegments
    pathSegments = coerce $ Path.toList perspective
    namesForReference :: LabeledDependency -> Sqlite.Transaction ([(Name, Referent)], [(Name, Reference)])
    namesForReference = \case
      LD.TermReferent ref -> do
        termNames <- fmap (Name.fromReverseSegments . coerce) <$> Ops.termNamesForRefWithinNamespace rootHash pathSegments (Cv.referent1to2 ref) Nothing
        pure ((,ref) <$> termNames, [])
      LD.TypeReference ref -> do
        typeNames <- fmap (Name.fromReverseSegments . coerce) <$> Ops.typeNamesForRefWithinNamespace rootHash pathSegments (Cv.reference1to2 ref) Nothing
        pure ([], (,ref) <$> typeNames)
