module Unison.PrettyPrintEnvDecl.Sqlite
  ( ppedForReferences,
  )
where

import U.Codebase.Sqlite.NameLookups (ReversedName (..))
import U.Codebase.Sqlite.NamedRef (NamedRef (..))
import U.Codebase.Sqlite.Operations (NamesPerspective)
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase qualified as Codebase
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (foldMapM)

-- | Given a set of references, return a PPE which contains names for only those references.
-- Names are limited to those within the provided perspective
ppedForReferences :: NamesPerspective -> Set LabeledDependency -> Sqlite.Transaction PPED.PrettyPrintEnvDecl
ppedForReferences namesPerspective refs = do
  hashLen <- Codebase.hashLength
  (termNames, typeNames) <-
    refs & foldMapM \ref ->
      namesForReference namesPerspective ref

  -- Ideally we'd only suffixify the name we're actually going to use, but due to name biasing
  -- we won't know that until we actually call the pretty-printer, so
  -- we add suffixifications for every name we have for each reference.
  longestTermSuffixMatches <- forMaybe termNames \(name, ref) -> do
    result <-
      Ops.longestMatchingTermNameForSuffixification namesPerspective (NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = Cv.referent1to2 ref})
        <&> fmap \(NamedRef {reversedSegments, ref = (ref, mayCt)}) ->
          let ct = fromMaybe (error "ppedForReferences: Required constructor type for constructor but it was null") mayCt
           in (Name.fromReverseSegments (coerce reversedSegments), Cv.referent2to1UsingCT ct ref)
    pure result
  longestTypeSuffixMatches <- forMaybe typeNames \(name, ref) -> do
    result <-
      Ops.longestMatchingTypeNameForSuffixification namesPerspective (NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = Cv.reference1to2 ref})
        <&> fmap \(NamedRef {reversedSegments, ref}) ->
          (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
    pure result
  let allTermNamesToConsider = termNames <> longestTermSuffixMatches
  let allTypeNamesToConsider = typeNames <> longestTypeSuffixMatches
  let names = Names.fromTermsAndTypes allTermNamesToConsider allTypeNamesToConsider
  pure (PPED.makePPED (PPE.hqNamer hashLen names) (PPE.suffixifyByHash names))
  where
    namesForReference :: Ops.NamesPerspective -> LabeledDependency -> Sqlite.Transaction ([(Name, Referent)], [(Name, Reference)])
    namesForReference namesPerspective = \case
      LD.TermReferent ref -> do
        termNames <- fmap (Name.fromReverseSegments . coerce) <$> Ops.termNamesForRefWithinNamespace namesPerspective (Cv.referent1to2 ref) Nothing
        pure ((,ref) <$> termNames, [])
      LD.TypeReference ref -> do
        typeNames <- fmap (Name.fromReverseSegments . coerce) <$> Ops.typeNamesForRefWithinNamespace namesPerspective (Cv.reference1to2 ref) Nothing
        pure ([], (,ref) <$> typeNames)
