{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Module for validating hashes of entities received/sent via sync.
module Unison.Sync.EntityValidation
  ( validateEntity,
  )
where

import Data.ByteString qualified as BS
import Data.Bytes.Get (runGetS)
import Data.Text qualified as Text
import GHC.IO (unsafePerformIO)
import System.Environment (lookupEnv)
import U.Codebase.HashTags
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Decode qualified as Decode
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.HashHandle qualified as HH
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.Serialization qualified as Serialization
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Prelude
import Unison.Sync.Common qualified as Share
import Unison.Sync.Types qualified as Share

validationEnvKey :: String
validationEnvKey = "UNISON_ENTITY_VALIDATION"

shouldValidateEntities :: Bool
shouldValidateEntities = unsafePerformIO $ do
  lookupEnv validationEnvKey <&> \case
    Just "true" -> True
    _ -> False
{-# NOINLINE shouldValidateEntities #-}

-- | Note: We currently only validate Namespace hashes.
-- We should add more validation as more entities are shared.
validateEntity :: Hash32 -> Share.Entity Text Hash32 Hash32 -> Maybe Share.EntityValidationError
validateEntity expectedHash32 entity
  | shouldValidateEntities = do
      case Share.entityToTempEntity id entity of
        Entity.TC (TermFormat.SyncTerm localComp) -> do
          validateTerm expectedHash localComp
        Entity.N (BranchFormat.SyncDiff {}) -> do
          (Just $ Share.UnsupportedEntityType expectedHash32 Share.NamespaceDiffType)
        Entity.N (BranchFormat.SyncFull localIds (BranchFormat.LocalBranchBytes bytes)) -> do
          validateBranchFull expectedHash localIds bytes
        _ -> Nothing
  | otherwise = Nothing
  where
    expectedHash :: Hash
    expectedHash = Hash32.toHash expectedHash32

validateBranchFull ::
  Hash ->
  BranchFormat.BranchLocalIds' Text Hash32 Hash32 (Hash32, Hash32) ->
  BS.ByteString ->
  (Maybe Share.EntityValidationError)
validateBranchFull expectedHash localIds bytes = do
  case runGetS Serialization.getLocalBranch bytes of
    Left e -> Just $ Share.InvalidByteEncoding (Hash32.fromHash expectedHash) Share.NamespaceType (Text.pack e)
    Right localBranch -> do
      let localIds' =
            localIds
              { BranchFormat.branchDefnLookup = ComponentHash . Hash32.toHash <$> BranchFormat.branchDefnLookup localIds,
                BranchFormat.branchPatchLookup = PatchHash . Hash32.toHash <$> BranchFormat.branchPatchLookup localIds,
                BranchFormat.branchChildLookup =
                  BranchFormat.branchChildLookup localIds
                    <&> bimap (BranchHash . Hash32.toHash) (CausalHash . Hash32.toHash)
              }
      let actualHash =
            HH.hashBranchFormatFull v2HashHandle localIds' localBranch
      if actualHash == BranchHash expectedHash
        then Nothing
        else Just $ Share.EntityHashMismatch Share.NamespaceType (mismatch expectedHash (unBranchHash actualHash))

validateTerm :: Hash -> (TermFormat.SyncLocallyIndexedComponent' Text Hash32) -> (Maybe Share.EntityValidationError)
validateTerm expectedHash syncLocalComp = do
  case Decode.unsyncTermComponent syncLocalComp of
    Left decodeErr -> Just (Share.InvalidByteEncoding (Hash32.fromHash expectedHash) Share.TermComponentType (tShow decodeErr))
    Right localComp -> do
      case HH.verifyTermFormatHash v2HashHandle (ComponentHash expectedHash) (TermFormat.Term localComp) of
        Nothing -> Nothing
        Just (HH.HashMismatch {expectedHash, actualHash}) -> Just . Share.EntityHashMismatch Share.TermComponentType $ mismatch expectedHash actualHash

mismatch :: Hash -> Hash -> Share.HashMismatchForEntity
mismatch supplied computed =
  Share.HashMismatchForEntity
    { supplied = Hash32.fromHash supplied,
      computed = Hash32.fromHash computed
    }
