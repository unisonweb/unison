{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module IdentifyHashMismatches (identifyHashMismatches) where

import Data.Foldable
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Unison.Codebase
import Unison.Codebase qualified as Codebase
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Parser.Ann
import Unison.Prelude
import Unison.Sqlite
import Unison.Symbol
import Unison.Sync.Common qualified as Sync
import Unison.Sync.EntityValidation
import Unison.Sync.Types
import Prelude hiding (log)

-- | This migration just deletes all the old name lookups, it doesn't recreate them.
-- On share we'll rebuild only the required name lookups from scratch.
identifyHashMismatches :: Codebase IO Symbol Ann -> IO ()
identifyHashMismatches codebase = do
  allHashes <- Codebase.runTransaction codebase $ do
    queryListCol @Hash32
      [sql|
    SELECT DISTINCT hash.base32
      FROM hash
      -- Ensure that the hash actually exists in the codebase
      WHERE EXISTS(SELECT 1 FROM object WHERE primary_hash_id = hash.id
                    UNION SELECT 1 FROM causal WHERE causal.self_hash_id = hash.id)
    |]
  for_ allHashes \hash32 -> do
    entity <- Codebase.runTransaction codebase $ Sync.expectEntity hash32
    case validateEntity hash32 entity of
      Nothing -> pure ()
      Just err ->
        case err of
          EntityHashMismatch et (HashMismatchForEntity {supplied, computed}) ->
            Text.appendFile "hash-mismatches.csv" $ Text.intercalate "," [Hash32.toText supplied, Hash32.toText computed, tShow et] <> "\n"
          UnsupportedEntityType {} -> pure ()
          InvalidByteEncoding hash32 et err ->
            Text.appendFile "invalid-byte-encodings.csv" $ Text.intercalate "," [Hash32.toText hash32, tShow et, err] <> "\n"
          HashResolutionFailure hash32 ->
            Text.appendFile "hash-resolution-failures.csv" $ Text.intercalate "," [Hash32.toText hash32] <> "\n"
