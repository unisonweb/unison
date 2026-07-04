{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip tests covering the @##Builtin.Given@ sentinel through the
-- sharing API wire format.
--
-- The sharing API encodes a namespace as @LocalBranchBytes@: an opaque
-- 'ByteString' produced by 'putLocalBranch' and consumed by
-- 'getLocalBranch'. 'MdValues' (per-name metadata) is part of that byte
-- payload, encoded as @putMetadataSetFormat = putWord8 0 *> putFoldable
-- putReference s@. The given sentinel is just an ordinary
-- 'ReferenceBuiltin' inside that set, so no schema change is required to
-- carry it across push/pull: the wire format already round-trips it.
--
-- These tests pin that property in place so future protocol changes
-- don't accidentally regress it. They cover:
--
-- * Direct round-trip of a 'LocalBranch' containing the sentinel via
--   'putLocalBranch' / 'getLocalBranch'.
-- * Round-trip through the 'TempEntity' CBOR wire format
--   (@instance Serialise TempEntity@) — i.e. through a full sync
--   payload — preserves the @LocalBranchBytes@ exactly.
-- * Graceful degradation: a payload whose metadata set contains the
--   sentinel alongside an unknown reference round-trips identically. An
--   "older" client that doesn't understand the sentinel sees it as just
--   another opaque metadata reference and copies it through unchanged.
module Unison.Test.Sync.GivenRoundtrip (test) where

import Codec.Serialise qualified as Serialise
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import EasyTest
import U.Codebase.Reference (Reference' (..))
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds (LocalDefnId (..), LocalTextId (..))
import U.Codebase.Sqlite.Serialization qualified as S
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Util.Serialization qualified as Put
import Unison.Server.Orphans ()

-- | The textual name of the given sentinel. Mirrors
-- 'Unison.Builtin.givenSentinelName' from chunk B1; we hard-code it
-- here so this test package doesn't depend on parser-typechecker.
givenSentinelText :: Text
givenSentinelText = "Builtin.Given"

-- | A second, unrelated builtin tag used to model an arbitrary
-- metadata reference that an older client wouldn't recognise.
otherMetadataText :: Text
otherMetadataText = "Builtin.OtherTag"

-- The text-table layout for our test payload. Index 0 is the term name
-- under which the referent is published; index 1 is the builtin name
-- referred to by the given sentinel reference.
nameSegmentTextIx, sentinelTextIx, otherMetadataTextIx :: LocalTextId
nameSegmentTextIx = LocalTextId 0
sentinelTextIx = LocalTextId 1
otherMetadataTextIx = LocalTextId 2

-- The sentinel itself, expressed in 'LocalBranch' coordinates.
sentinelLocal :: BranchFull.MetadataSetFormat' LocalTextId LocalDefnId
sentinelLocal =
  BranchFull.Inline (Set.singleton (ReferenceBuiltin sentinelTextIx))

-- A second metadata reference (a different, unrelated builtin tag) so
-- we can verify multi-element metadata sets round-trip too.
sentinelPlusOtherLocal :: BranchFull.MetadataSetFormat' LocalTextId LocalDefnId
sentinelPlusOtherLocal =
  BranchFull.Inline
    ( Set.fromList
        [ ReferenceBuiltin sentinelTextIx,
          ReferenceBuiltin otherMetadataTextIx
        ]
    )

-- A single-term branch where that term carries the supplied metadata
-- set. The term itself is a builtin referent (@##SomeTerm@ encoded as
-- 'ReferenceBuiltin 0' for simplicity — index 0 is also the name's
-- text id, but that's fine; in 'LocalBranch' the text-id space is
-- shared and 'Builtin' refs name distinct entities).
branchWithMetadata ::
  BranchFull.MetadataSetFormat' LocalTextId LocalDefnId ->
  BranchFull.LocalBranch
branchWithMetadata md =
  BranchFull.Branch
    ( Map.singleton
        nameSegmentTextIx
        ( Map.singleton
            (Referent.Ref (ReferenceBuiltin nameSegmentTextIx))
            md
        )
    )
    Map.empty
    Map.empty
    Map.empty

branchLocalIds :: BranchFormat.BranchLocalIds' Text hash p c
branchLocalIds =
  BranchFormat.LocalIds
    { BranchFormat.branchTextLookup =
        Vector.fromList ["fooName", givenSentinelText, otherMetadataText],
      BranchFormat.branchDefnLookup = Vector.empty,
      BranchFormat.branchPatchLookup = Vector.empty,
      BranchFormat.branchChildLookup = Vector.empty
    }

-- | Encode a 'LocalBranch' the same way SQLite + sharing do.
encodeLocalBranchBytes :: BranchFull.LocalBranch -> ByteString
encodeLocalBranchBytes = Put.putBytes S.putLocalBranch

-- | Decode bytes back into a 'LocalBranch'.
decodeLocalBranchBytes :: ByteString -> Either String BranchFull.LocalBranch
decodeLocalBranchBytes = runGetS S.getLocalBranch

-- | Wrap a 'LocalBranch' in the same 'TempEntity' shape that travels
-- through the sharing wire format.
namespaceTempEntity :: BranchFull.LocalBranch -> TempEntity
namespaceTempEntity lb =
  Entity.N
    ( BranchFormat.SyncFull
        branchLocalIds
        (BranchFormat.LocalBranchBytes (encodeLocalBranchBytes lb))
    )

-- | Extract the 'LocalBranchBytes' payload from a 'TempEntity', if it
-- is a @SyncFull@ namespace.
namespaceBytes :: TempEntity -> Maybe ByteString
namespaceBytes = \case
  Entity.N (BranchFormat.SyncFull _ (BranchFormat.LocalBranchBytes bs)) -> Just bs
  _ -> Nothing

-- | Pull the metadata reference set off the (sole) term in the (sole)
-- name of the supplied 'LocalBranch'.
extractMetadata ::
  BranchFull.LocalBranch ->
  Maybe (Set.Set (Reference' LocalTextId LocalDefnId))
extractMetadata (BranchFull.Branch terms _ _ _) = do
  byName <- Map.lookup nameSegmentTextIx terms
  BranchFull.Inline refs <-
    Map.lookup
      (Referent.Ref (ReferenceBuiltin nameSegmentTextIx))
      byName
  pure refs

test :: Test ()
test =
  scope "syncv1.givenRoundtrip" . tests $
    [ scope "LocalBranch with given sentinel round-trips through putLocalBranch/getLocalBranch" do
        let original = branchWithMetadata sentinelLocal
            bytes = encodeLocalBranchBytes original
        case decodeLocalBranchBytes bytes of
          Left err -> crash $ "decode failed: " <> err
          Right decoded ->
            case extractMetadata decoded of
              Nothing -> crash "decoded branch missing the term metadata entry"
              Just refs ->
                expect (refs == Set.singleton (ReferenceBuiltin sentinelTextIx)),
      scope "LocalBranchBytes survives the TempEntity CBOR wire format byte-for-byte" do
        let original = branchWithMetadata sentinelLocal
            entity = namespaceTempEntity original
            roundTripped = Serialise.deserialise (Serialise.serialise entity)
        expect (entity == roundTripped)
        case (namespaceBytes entity, namespaceBytes roundTripped) of
          (Just before, Just after) -> expect (before == after)
          _ -> crash "expected SyncFull namespace before and after wire round-trip",
      scope "metadata round-trips through wire format and decodes to the same MdValues" do
        let original = branchWithMetadata sentinelLocal
            entity = namespaceTempEntity original
            wireRoundTripped =
              Serialise.deserialise (Serialise.serialise entity) :: TempEntity
        case namespaceBytes wireRoundTripped >>= either (const Nothing) Just . decodeLocalBranchBytes of
          Nothing -> crash "could not extract LocalBranch from wire-roundtripped entity"
          Just decoded ->
            expect (extractMetadata decoded == extractMetadata original),
      scope "graceful degradation: sentinel coexists with unknown metadata refs through the wire" do
        -- Models the "older client / newer server" and "newer client /
        -- older server" cases. The wire format treats any
        -- 'MetadataSet' member as an opaque 'Reference', so a client
        -- that doesn't recognise '##Builtin.Given' (resp.
        -- '##Builtin.OtherTag') still copies it through unchanged.
        let original = branchWithMetadata sentinelPlusOtherLocal
            entity = namespaceTempEntity original
            roundTripped =
              Serialise.deserialise (Serialise.serialise entity) :: TempEntity
        expect (entity == roundTripped)
        case namespaceBytes roundTripped >>= either (const Nothing) Just . decodeLocalBranchBytes of
          Nothing -> crash "could not decode LocalBranch from round-tripped entity"
          Just decoded ->
            case extractMetadata decoded of
              Nothing -> crash "decoded branch missing metadata set"
              Just refs -> do
                expect (Set.member (ReferenceBuiltin sentinelTextIx) refs)
                expect (Set.member (ReferenceBuiltin otherMetadataTextIx) refs)
                expect (Set.size refs == 2)
    ]
