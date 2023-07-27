{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.SqliteCodebase.Migrations.MetadataCheck (metadataCheck) where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Branch qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.Reference (Reference)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.DbId (BranchHashId)
import U.Codebase.Sqlite.Operations (NamesPerspective, namesPerspectiveForRootIdAndPath)
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Debug qualified as Debug
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (libSegment)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl as PPED
import Unison.PrettyPrintEnvDecl.Sqlite qualified as Sqlite
import Unison.Referent qualified as V1Referent
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.HashQualified qualified as HQ
import Unison.Util.Monoid (foldMapM)

findTermName :: Reference -> Sqlite.Transaction (Maybe Text)
findTermName ref = case ref of
  Reference.Derived h _ -> do
    let base32 = Hash.toBase32Hex h
    Sqlite.queryMaybeCol
      [Sqlite.sql|
      SELECT reversed_name FROM scoped_term_name_lookup WHERE referent_component_hash = :base32
        LIMIT 1
      |]
  _ -> pure Nothing

findTypeName :: Reference -> Sqlite.Transaction (Maybe Text)
findTypeName ref = case ref of
  Reference.Derived h _ -> do
    let base32 = Hash.toBase32Hex h
    Sqlite.queryMaybeCol
      [Sqlite.sql|
      SELECT reversed_name FROM scoped_type_name_lookup WHERE reference_component_hash = :base32
        LIMIT 1
      |]
  _ -> pure Nothing

licenseRef :: Reference
licenseRef = Reference.Derived (Hash.unsafeFromBase32HexText "7k7de5eej4eflq5h09kr5jno5dmdkepdhpd25r71nbh9a0cdpiisltoj8en3pe47ifgscuuvd19n9o1vluifeorj7gosn7b33m3vp5g") 0

data MDStuff = MDStuff
  { authors :: Set Reference,
    copyrightHolders :: Set Reference,
    licenseTypes :: Set Reference,
    unknownMDTypes :: Set Reference
  }
  deriving (Show)

instance Semigroup MDStuff where
  MDStuff a1 c1 l1 u1 <> MDStuff a2 c2 l2 u2 =
    MDStuff (a1 <> a2) (c1 <> c2) (l1 <> l2) (u1 <> u2)

instance Monoid MDStuff where
  mempty = MDStuff mempty mempty mempty mempty

depsForMDStuff :: MDStuff -> Set LabeledDependency
depsForMDStuff MDStuff {authors, copyrightHolders, unknownMDTypes} =
  Set.map (LD.TermReference . Cv.reference2to1) authors
    <> Set.map (LD.TermReference . Cv.reference2to1) copyrightHolders
    <> Set.map (LD.TypeReference . Cv.reference2to1) unknownMDTypes

printMDStuff :: NamesPerspective -> MDStuff -> Sqlite.Transaction ()
printMDStuff namesPerspective md@MDStuff {authors, unknownMDTypes, copyrightHolders, licenseTypes} = do
  ppe <- PPED.unsuffixifiedPPE <$> Sqlite.ppedForReferences namesPerspective (depsForMDStuff md)
  when (Set.size authors > 1) $ do
    termRefs <- printTermRefs ppe authors
    Debug.debugM Debug.Migration "Many authors" termRefs
  when (Set.size copyrightHolders > 1) $ do
    termRefs <- printTermRefs ppe copyrightHolders
    Debug.debugM Debug.Migration "Many copyright holders" termRefs
  when (Set.size licenseTypes > 1) $ do
    licenseRefs <- traverse (printLicenseType ppe) (Set.toList licenseTypes)
    Debug.debugM Debug.Migration "Many licenses" licenseRefs
  when (Set.size unknownMDTypes > 0) $ do
    typeRefs <- printTypeRefs ppe unknownMDTypes
    Debug.debugM Debug.Migration "Unknown types" typeRefs

printTermRefs :: PPE.PrettyPrintEnv -> Set Reference -> Sqlite.Transaction [Text]
printTermRefs ppe refs = do
  let mayNames =
        Set.toList refs
          <&> \ref -> (ref, PPE.termNameOrHashOnly ppe (V1Referent.Ref $ Cv.reference2to1 ref))
  for mayNames \case
    (ref, hq@HQ.HashOnly {}) -> fromMaybe (HQ.toText hq) <$> findTermName ref
    (_, hq) -> pure $ HQ.toText hq

printTypeRefs :: PPE.PrettyPrintEnv -> Set Reference -> Sqlite.Transaction [Text]
printTypeRefs ppe refs = do
  let mayNames =
        Set.toList refs
          <&> \ref -> (ref, PPE.typeNameOrHashOnly ppe (Cv.reference2to1 ref))
  for mayNames \case
    (ref, hq@HQ.HashOnly {}) -> fromMaybe (HQ.toText hq) <$> findTypeName ref
    (_, hq) -> pure $ HQ.toText hq

printLicenseType :: PPE.PrettyPrintEnv -> Reference.Reference' Text Hash.Hash -> Sqlite.Transaction Text
printLicenseType ppe ref =
  case lookup
    ref
    [ (license2021Ref, "license2021"),
      (license2022Ref, "license2022"),
      (license2023Ref, "license2023")
    ] of
    Just name -> pure name
    Nothing ->
      case PPE.typeNameOrHashOnly ppe (Cv.reference2to1 ref) of
        HQ.HashOnly h ->
          findTermName ref >>= \case
            Just name -> pure name
            Nothing -> pure $ HQ.toText $ HQ.HashOnly h
        hq -> pure $ HQ.toText hq

license2021Ref :: Reference.Reference' t Hash.Hash
license2021Ref = Reference.Derived (Hash.unsafeFromBase32HexText "6goouduq9me424km0c9p562gpf7pqcnmfbo6bno6vas4no8akooggdk5ithatukmj4ur007isn5m357q52dfect8laesspm7h2k1lh8") 0

license2022Ref :: Reference.Reference' t Hash.Hash
license2022Ref = Reference.Derived (Hash.unsafeFromBase32HexText "b1f6bgraj3cuol8ueh65g7bod86rpb90opptpbrj3hev0d1cqg065c6ldenh2agntsgnmjc9f9u2qfor1k0v4d69555kcnfiagrvet0") 0

license2023Ref :: Reference.Reference' t Hash.Hash
license2023Ref = Reference.Derived (Hash.unsafeFromBase32HexText "jjcjdemgv8v132on4sk9q2ov6ocgm2070kpagn50eg71fl8o4su2fn5vd4eke080bscb5l39p21m1d9m7t9pa13nbgbt9u21k4fogk8") 0

-- | Adds a table for tracking namespace statistics
-- Adds stats for all existing namespaces, even though missing stats are computed on-demand if missing.
metadataCheck :: Sqlite.Transaction ()
metadataCheck = do
  bhIds <- liveBranchHashIds
  for_ bhIds $ \bhId -> do
    branch <- Ops.expectBranchByBranchHashId bhId
    mdStuff <- allMetadataForBranch analyse branch
    np <- namesPerspectiveForRootIdAndPath bhId mempty
    printMDStuff np mdStuff
  where
    analyse :: Branch.MdValues -> Sqlite.Transaction MDStuff
    analyse (Branch.MdValues mdValues) = do
      mdValues
        & Map.toList
        & foldMapM \(value, typ) ->
          case typ of
            _
              | typ == Cv.reference1to2 IOSource.authorRef -> mempty {authors = Set.singleton value}
              | typ == Cv.reference1to2 IOSource.copyrightHolderRef -> mempty {copyrightHolders = Set.singleton value}
              | typ == Cv.reference1to2 IOSource.guidRef -> mempty
              | typ == Cv.reference1to2 IOSource.isPropagatedReference -> mempty
              | typ == Cv.reference1to2 IOSource.isTestReference -> mempty
              | typ == licenseRef -> mempty {licenseTypes = Set.singleton value}
              | otherwise -> mempty {unknownMDTypes = Set.singleton typ}
            & pure

-- | Find all causal hashes which have (at some point) been a branch head or loose code
-- root, but are not dependencies.
liveBranchHashIds :: Sqlite.Transaction [BranchHashId]
liveBranchHashIds =
  Sqlite.queryListCol
    [Sqlite.sql|
    SELECT DISTINCT root_branch_hash_id FROM (SELECT root_branch_hash_id FROM name_lookups WHERE NOT EXISTS (SELECT 1 FROM  name_lookup_mounts WHERE mounted_root_branch_hash_id = root_branch_hash_id)) AS live_roots
  |]

allMetadataForBranch :: Monoid r => (Branch.MdValues -> Sqlite.Transaction r) -> Branch.Branch Sqlite.Transaction -> Sqlite.Transaction r
allMetadataForBranch f branch = do
  let mdValuesM =
        (Branch.terms branch, Branch.types branch)
          & toListOf (beside (folded . folded) (folded . folded))
  acc <-
    mdValuesM
      & foldMapM \mdM -> mdM >>= f
  childAcc <-
    (onlyNonLib <$> Branch.nonEmptyChildren branch) >>= foldMapM \cb -> do
      Causal.value cb >>= allMetadataForBranch f
  pure $ acc <> childAcc
  where
    onlyNonLib :: (Map NameSegment b) -> (Map NameSegment b)
    onlyNonLib = Map.filterWithKey \k _ -> k /= libSegment
