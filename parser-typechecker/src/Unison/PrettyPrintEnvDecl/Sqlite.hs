module Unison.PrettyPrintEnvDecl.Sqlite where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set
import U.Codebase.HashTags (BranchHash)
import U.Codebase.Sqlite.NamedRef (NamedRef (..))
import qualified U.Codebase.Sqlite.Operations as Ops
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.Debug as Debug
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import qualified Unison.NameSegment as NameSegment
import qualified Unison.Names as Names
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Names as PPED
import qualified Unison.PrettyPrintEnvDecl.Scanner as PPG
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Sqlite as Sqlite
import Unison.Util.Monoid (foldMapM)

prettyPrintUsingNamesIndex :: (MonadIO m) => Codebase IO v ann -> BranchHash -> Path -> PPG.PrettyPrintGrouper m a -> m a
prettyPrintUsingNamesIndex codebase rootHash perspective action = do
  let deps = PPG.collectDeps action
  pped <- liftIO . Codebase.runTransaction codebase $ ppedForReferences rootHash perspective deps
  PPG.runWithPPE pped action

-- | Given a set of references, return a PPE which contains names for only those references.
ppedForReferences :: BranchHash -> Path -> Set LabeledDependency -> Sqlite.Transaction PPED.PrettyPrintEnvDecl
ppedForReferences rootHash perspective refs = do
  hashLen <- Codebase.hashLength
  Debug.debugLogM Debug.Server "ppedForReferences: getting all names for references"
  (termNames, typeNames) <-
    refs & foldMapM \ref ->
      namesForReference ref
  Debug.debugLogM Debug.Server "ppedForReferences: computing term names for suffixification"
  let termSuffixes =
        Set.fromList (termNames <&> \(name, _ref) -> (NameSegment.toText (Name.lastSegment name)))
          -- TODO: remove this
          & Set.delete "doc"
  let typeSuffixes =
        Set.fromList (typeNames <&> \(name, _ref) -> (NameSegment.toText (Name.lastSegment name)))
          -- TODO: remove this
          & Set.delete "doc"
  Debug.debugLogM Debug.Server "ppedForReferences: computing type names for suffixification"
  longestTermSuffixMatches <- forMaybe termNames \(name, ref) -> do
    Ops.longestMatchingTermNameForSuffixification rootHash pathText (NamedRef {reversedSegments = Name.reversedSegments name, ref = Cv.referent1to2 ref})
      <&> fmap \(NamedRef {reversedSegments, ref = (ref, mayCt)}) ->
        let ct = fromMaybe (error "ppedForReferences: Required constructor type for constructor but it was null") mayCt
         in (Name.fromReverseSegments (coerce reversedSegments), Cv.referent2to1UsingCT ct ref)
  longestTypeSuffixMatches <- forMaybe typeNames \(name, ref) -> do
    Ops.longestMatchingTypeNameForSuffixification rootHash pathText (NamedRef {reversedSegments = Name.reversedSegments name, ref = Cv.reference1to2 ref})
      <&> fmap \(NamedRef {reversedSegments, ref}) ->
        (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
  let allTermNamesToConsider = termNames <> longestTermSuffixMatches
  let allTypeNamesToConsider = typeNames <> longestTypeSuffixMatches
  Debug.debugLogM Debug.Server $ "ppedForReferences built pped within " <> show perspective <> " for " <> show (Set.size refs) <> " deps, " <> show (length allTermNamesToConsider) <> " terms and " <> show (length allTypeNamesToConsider) <> " types"
  Debug.debugM Debug.Server "ppedForReferences: PPED BASE size: " $ show (length termNames + length typeNames)
  Debug.debugM Debug.Server "ppedForReferences: PPED With suffixifications size: " $ show (length allTermNamesToConsider + length allTypeNamesToConsider)
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
