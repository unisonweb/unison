module Unison.PrettyPrintEnvDecl.Sqlite where

import qualified Data.Set as Set
import U.Codebase.HashTags (BranchHash)
import U.Codebase.Sqlite.NamedRef (NamedRef (..))
import qualified U.Codebase.Sqlite.Operations as Ops
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import qualified Unison.Names as Names
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Names as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Sqlite as Sqlite
import Unison.Util.Monoid (foldMapM)

-- | Given a set of references, return a PPE which contains names for only those references.
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
      Ops.longestMatchingTermNameForSuffixification rootHash pathText (NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = Cv.referent1to2 ref})
        <&> fmap \(NamedRef {reversedSegments, ref = (ref, mayCt)}) ->
          let ct = fromMaybe (error "ppedForReferences: Required constructor type for constructor but it was null") mayCt
           in (Name.fromReverseSegments (coerce reversedSegments), Cv.referent2to1UsingCT ct ref)
    pure result
  longestTypeSuffixMatches <- forMaybe typeNames \(name, ref) -> do
    result <-
      Ops.longestMatchingTypeNameForSuffixification rootHash pathText (NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = Cv.reference1to2 ref})
        <&> fmap \(NamedRef {reversedSegments, ref}) ->
          (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
    pure result
  let allTermNamesToConsider = termNames <> longestTermSuffixMatches
  let allTypeNamesToConsider = typeNames <> longestTypeSuffixMatches
  pure . PPED.fromNamesDecl hashLen . NamesWithHistory.fromCurrentNames $ Names.fromTermsAndTypes allTermNamesToConsider allTypeNamesToConsider
  where
    pathText :: Text
    pathText = Path.toText perspective
    namesForReference :: LabeledDependency -> Sqlite.Transaction ([(Name, Referent)], [(Name, Reference)])
    namesForReference = \case
      LD.TermReferent ref -> do
        termNames <- fmap (Name.fromReverseSegments . coerce) <$> Ops.termNamesForRefWithinNamespace rootHash pathText (Cv.referent1to2 ref) Nothing
        pure ((,ref) <$> termNames, [])
      LD.TypeReference ref -> do
        typeNames <- fmap (Name.fromReverseSegments . coerce) <$> Ops.typeNamesForRefWithinNamespace rootHash pathText (Cv.reference1to2 ref) Nothing
        pure ([], (,ref) <$> typeNames)
