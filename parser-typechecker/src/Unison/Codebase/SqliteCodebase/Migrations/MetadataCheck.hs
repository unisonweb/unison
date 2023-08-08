{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.SqliteCodebase.Migrations.MetadataCheck (metadataCheck) where

import Control.Lens
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Branch qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.Reference (Reference)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.DbId (BranchHashId)
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase (CodebasePath)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Hash qualified as Hash
import Unison.Name (libSegment)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (foldMapM)

findTermName :: Reference -> Sqlite.Transaction (Maybe Text)
findTermName ref = case ref of
  Reference.Derived h _ -> do
    let base32 = Hash.toBase32Hex h
    fmap unreverseName
      <$> Sqlite.queryMaybeCol
        [Sqlite.sql|
      SELECT reversed_name FROM scoped_term_name_lookup WHERE referent_component_hash = :base32
        LIMIT 1
      |]
  _ -> pure Nothing
  where
    unreverseName revName =
      revName
        & Text.splitOn "."
        & reverse
        & Text.intercalate "."

findTypeName :: Reference -> Sqlite.Transaction (Maybe Text)
findTypeName ref = case ref of
  Reference.Derived h _ -> do
    let base32 = Hash.toBase32Hex h
    fmap unreverseName
      <$> Sqlite.queryMaybeCol
        [Sqlite.sql|
      SELECT reversed_name FROM scoped_type_name_lookup WHERE reference_component_hash = :base32
        LIMIT 1
      |]
  _ -> pure Nothing
  where
    unreverseName revName =
      revName
        & Text.splitOn "."
        & reverse
        & Text.intercalate "."

licenseRef :: Reference
licenseRef = Reference.Derived (Hash.unsafeFromBase32HexText "7k7de5eej4eflq5h09kr5jno5dmdkepdhpd25r71nbh9a0cdpiisltoj8en3pe47ifgscuuvd19n9o1vluifeorj7gosn7b33m3vp5g") 0

data MDStuff r = MDStuff
  { authors :: Set r,
    copyrightHolders :: Set r,
    licenseTypes :: Set r,
    unknownMDTypes :: Set r
  }
  deriving (Show)

instance ToJSON r => ToJSON (MDStuff r) where
  toJSON MDStuff {..} =
    Aeson.object
      [ "authors" Aeson..= authors,
        "unknownMDTypes" Aeson..= unknownMDTypes,
        "licenseTypes" Aeson..= licenseTypes,
        "copyrightHolders" Aeson..= copyrightHolders
      ]

instance (Ord r) => Semigroup (MDStuff r) where
  MDStuff a1 c1 l1 u1 <> MDStuff a2 c2 l2 u2 =
    MDStuff (a1 <> a2) (c1 <> c2) (l1 <> l2) (u1 <> u2)

instance (Ord r) => Monoid (MDStuff r) where
  mempty = MDStuff mempty mempty mempty mempty

mdStuffNames :: MDStuff Reference -> Sqlite.Transaction (MDStuff Text)
mdStuffNames MDStuff {authors, unknownMDTypes, copyrightHolders, licenseTypes} = do
  namedAuthors <- Set.fromList <$> printTermRefs authors
  namedUnknownMDTypes <- Set.fromList <$> printTypeRefs unknownMDTypes
  namedLicenseTypes <- Set.fromList <$> traverse printLicenseType (Set.toList licenseTypes)
  namedCopyRightHolders <- Set.fromList <$> printTermRefs copyrightHolders
  pure $
    MDStuff
      { authors = namedAuthors,
        unknownMDTypes = namedUnknownMDTypes,
        licenseTypes = namedLicenseTypes,
        copyrightHolders = namedCopyRightHolders
      }

printTermRefs :: Set Reference -> Sqlite.Transaction [Text]
printTermRefs refs = do
  for (Set.toList refs) \ref -> do
    findTermName ref >>= \case
      Just name -> pure name
      Nothing -> pure . tShow $ Cv.reference2to1 ref

printTypeRefs :: Set Reference -> Sqlite.Transaction [Text]
printTypeRefs refs = do
  for (Set.toList refs) \ref -> do
    findTypeName ref >>= \case
      Just name -> pure name
      Nothing -> pure . tShow $ Cv.reference2to1 ref

printLicenseType :: Reference -> Sqlite.Transaction Text
printLicenseType ref =
  findTermName ref >>= \case
    Just name -> pure name
    Nothing -> pure . tShow $ Cv.reference2to1 ref

-- license2021Ref :: Reference.Reference' t Hash.Hash
-- license2021Ref = Reference.Derived (Hash.unsafeFromBase32HexText "6goouduq9me424km0c9p562gpf7pqcnmfbo6bno6vas4no8akooggdk5ithatukmj4ur007isn5m357q52dfect8laesspm7h2k1lh8") 0

-- license2022Ref :: Reference.Reference' t Hash.Hash
-- license2022Ref = Reference.Derived (Hash.unsafeFromBase32HexText "b1f6bgraj3cuol8ueh65g7bod86rpb90opptpbrj3hev0d1cqg065c6ldenh2agntsgnmjc9f9u2qfor1k0v4d69555kcnfiagrvet0") 0

-- license2023Ref :: Reference.Reference' t Hash.Hash
-- license2023Ref = Reference.Derived (Hash.unsafeFromBase32HexText "jjcjdemgv8v132on4sk9q2ov6ocgm2070kpagn50eg71fl8o4su2fn5vd4eke080bscb5l39p21m1d9m7t9pa13nbgbt9u21k4fogk8") 0

-- | Adds a table for tracking namespace statistics
-- Adds stats for all existing namespaces, even though missing stats are computed on-demand if missing.
metadataCheck :: CodebasePath -> Sqlite.Transaction ()
metadataCheck codebasePath = do
  bhIds <- liveBranchHashIds
  mdStuff <-
    bhIds & foldMapM \bhId -> do
      branch <- Ops.expectBranchByBranchHashId bhId
      allMetadataForBranch analyse branch
  namedMDStuff <- mdStuffNames mdStuff
  Sqlite.unsafeIO $ do
    print namedMDStuff
    dumpJSON codebasePath namedMDStuff
  where
    analyse :: Branch.MdValues -> Sqlite.Transaction (MDStuff Reference)
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

dumpJSON :: CodebasePath -> MDStuff Text -> IO ()
dumpJSON codebasePath namedMDStuff = do
  let val = Aeson.object ["codebase" Aeson..= codebasePath, "metadata" Aeson..= namedMDStuff]
  BL.appendFile "metadata.json" ("\n" <> Aeson.encode val)

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
