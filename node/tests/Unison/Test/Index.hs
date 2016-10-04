{-# Language TypeSynonymInstances #-}
{-# Language OverloadedStrings #-}
module Unison.Test.Index where

import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import System.Random
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Unison.Runtime.Address
import Unison.Test.BlockStore (makeRandomAddress)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as B
import qualified Unison.BlockStore as BS
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Cryptography as C
import qualified Unison.Runtime.Index as Index

instance Arbitrary BS.Series where
    arbitrary =(BS.Series . B.pack) <$> vectorOf 64 arbitrary

makeRandomId :: IO BS.Series
makeRandomId = BS.Series <$> C.randomBytes (C.noop "dummypublickey") 64

crypto :: C.Cryptography ByteString () () () ByteString ByteString ByteString
crypto = C.noop "dummypublickey"

roundTrip :: BS.BlockStore Address -> Assertion
roundTrip bs = do
  ident <- makeRandomId
  db <- Index.load bs crypto ident
  Index.insert db "keyhash" ("key", "value")
  db2 <- Index.load bs crypto ident
  result <- Index.lookup db2 "keyhash"
  case result of
    Just (k, v) | v == "value" -> pure ()
    Just (k, v) -> fail ("expected value, got " ++ unpack v)
    _ -> fail "got nothin"

nextKeyAfterRemoval :: BS.BlockStore Address -> Assertion
nextKeyAfterRemoval bs = do
  ident <- makeRandomId
  db <- Index.load bs crypto ident
  Index.insert db "1" ("k1", "v1")
  Index.insert db "2" ("k2", "v2")
  Index.insert db "3" ("k3", "v3")
  Index.insert db "4" ("k4", "v4")
  Index.delete db "2"
  result <- Index.lookupGT db "1"
  case result of
    Just (kh, (k, v)) | kh == "3" -> pure ()
    Just (kh, (k, v)) -> fail ("expected key 3, got " ++ unpack kh)
    Nothing -> fail "got nothin"

keysAfterResize :: BS.BlockStore Address -> Assertion
keysAfterResize bs = do
  ident <- makeRandomId
  db <- Index.loadSized bs crypto ident 10
  let kvp i = (pack . ("k" ++) . show $ i, pack . ("v" ++) . show $ i)
      expected = map (pack . show) [15..30]
  mapM_ (\i -> Index.insert db (pack $ show i) (kvp i)) [15..30]
  result <- Index.keys db
  if result == expected
    then pure ()
    else fail ("got unexpected result: " ++ show result)

ioTests :: IO TestTree
ioTests = do
  blockStore <- MBS.make' makeRandomAddress makeAddress
  pure $ testGroup "KeyValueStore"
    [ testCase "roundTrip" (roundTrip blockStore)
    , testCase "nextKeyAfterRemoval" (nextKeyAfterRemoval blockStore)
    , testCase "keysAfterResize" (keysAfterResize blockStore)
    ]

main = ioTests >>= defaultMain
