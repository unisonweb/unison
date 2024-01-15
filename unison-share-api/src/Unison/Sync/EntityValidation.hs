{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Module for validating hashes of entities received/sent via sync.
module Unison.Sync.EntityValidation
  ( validateEntity,
  )
where

import Data.ByteString qualified as BS
import Data.Bytes.Get (runGetS)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.HashTags
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Causal qualified as CausalFormat
import U.Codebase.Sqlite.Decode qualified as Decode
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.HashHandle qualified as HH
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.Serialization qualified as Serialization
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H
import Unison.Prelude
import Unison.Sync.Common qualified as Share
import Unison.Sync.Types qualified as Share

-- | Note: We currently only validate Namespace hashes.
-- We should add more validation as more entities are shared.
validateEntity :: Hash32 -> Share.Entity Text Hash32 Hash32 -> Maybe Share.EntityValidationError
validateEntity expectedHash32 entity = do
  case Share.entityToTempEntity id entity of
    Entity.TC (TermFormat.SyncTerm localComp) -> do
      validateTerm expectedHash32 localComp
    Entity.N (BranchFormat.SyncDiff {}) -> do
      (Just $ Share.UnsupportedEntityType expectedHash32 Share.NamespaceDiffType)
    Entity.N (BranchFormat.SyncFull localIds (BranchFormat.LocalBranchBytes bytes)) -> do
      validateBranchFull expectedHash32 localIds bytes
    Entity.C CausalFormat.SyncCausalFormat {valueHash, parents} -> do
      validateCausal expectedHash32 valueHash (toList parents)
    _ -> Nothing

validateBranchFull ::
  Hash32 ->
  BranchFormat.BranchLocalIds' Text Hash32 Hash32 (Hash32, Hash32) ->
  BS.ByteString ->
  (Maybe Share.EntityValidationError)
validateBranchFull expectedHash localIds bytes = do
  case runGetS Serialization.getLocalBranch bytes of
    Left e -> Just $ Share.InvalidByteEncoding expectedHash Share.NamespaceType (Text.pack e)
    Right localBranch -> do
      let localIds' =
            localIds
              { BranchFormat.branchDefnLookup = ComponentHash <$> BranchFormat.branchDefnLookup localIds,
                BranchFormat.branchPatchLookup = PatchHash <$> BranchFormat.branchPatchLookup localIds,
                BranchFormat.branchChildLookup =
                  BranchFormat.branchChildLookup localIds
                    <&> bimap BranchHash CausalHash
              }
      let actualHash =
            HH.hashBranchFormatFull v2HashHandle localIds' localBranch
      if actualHash == BranchHash expectedHash
        then Nothing
        else Just $ Share.EntityHashMismatch Share.NamespaceType (mismatch expectedHash (unBranchHash actualHash))

validateTerm :: Hash32 -> (TermFormat.SyncLocallyIndexedComponent' Text Hash32) -> (Maybe Share.EntityValidationError)
validateTerm expectedHash syncLocalComp = do
  case Decode.unsyncTermComponent syncLocalComp of
    Left decodeErr -> Just (Share.InvalidByteEncoding expectedHash Share.TermComponentType (tShow decodeErr))
    Right localComp -> do
      case HH.verifyTermFormatHash v2HashHandle (ComponentHash expectedHash) (TermFormat.Term localComp) of
        Nothing -> Nothing
        Just (HH.HashMismatch {expectedHash, actualHash}) -> Just . Share.EntityHashMismatch Share.TermComponentType $ mismatch (Hash32.fromHash expectedHash) (Hash32.fromHash actualHash)

validateCausal :: Hash32 -> Hash32 -> [Hash32] -> Maybe Share.EntityValidationError
validateCausal expectedHash branchHash parentHashes32 = do
  let parents = Set.fromList (Hash32.toHash <$> parentHashes32)
  let actualHash = Hash32.fromHash $ H.contentHash (H.Causal {branchHash = Hash32.toHash branchHash, parents})
  if actualHash == expectedHash
    then Nothing
    else Just $ Share.EntityHashMismatch Share.CausalType (mismatch expectedHash actualHash)

mismatch :: Hash32 -> Hash32 -> Share.HashMismatchForEntity
mismatch supplied computed = Share.HashMismatchForEntity {supplied, computed}
