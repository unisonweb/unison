-- | Hedghog generators for Sync types.
module Unison.Test.Sync.Gen
  ( genTempEntity,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as BShort
import Data.Text (Text)
import Data.Vector qualified as Vector
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import Unison.Hash (Hash (..))
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32

genTempEntity :: Gen TempEntity
genTempEntity = do
  Gen.choice
    [ Entity.TC <$> genSyncTermFormat
    ]

genSyncTermFormat :: Gen (TermFormat.SyncTermFormat' Text Hash32)
genSyncTermFormat = do
  elems <- Gen.list (Range.linear 1 4) do
    localIds <- genLocalIds
    term <- genBodyBytes
    pure (localIds, term)
  pure $ TermFormat.SyncTerm $ TermFormat.SyncLocallyIndexedComponent $ Vector.fromList elems

genBodyBytes :: Gen ByteString
genBodyBytes = Gen.bytes (Range.linear 0 100)

genLocalIds :: Gen (LocalIds.LocalIds' Text Hash32)
genLocalIds = do
  textLookup <- Vector.fromList <$> Gen.list (Range.linear 0 10) genTextLiteral
  defnLookup <- Vector.fromList <$> Gen.list (Range.linear 0 10) genHash32
  pure $ LocalIds.LocalIds {textLookup, defnLookup}

genHash32 :: Gen Hash32
genHash32 = Hash32.fromHash <$> genHash

genHash :: Gen Hash
genHash = Hash . BShort.toShort <$> Gen.bytes (Range.singleton 64)

genTextLiteral :: Gen Text
genTextLiteral = Gen.text (Range.linear 0 100) Gen.unicodeAll
