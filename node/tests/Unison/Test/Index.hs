{-# Language TypeSynonymInstances #-}
{-# Language OverloadedStrings #-}
module Unison.Test.Index where

import Control.Concurrent.STM (atomically)
import Data.ByteString.Char8
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

makeRandomId :: IO (BS.Series, BS.Series)
makeRandomId = do
  cp <- BS.Series <$> C.randomBytes (C.noop 0) 64
  ud <- BS.Series <$> C.randomBytes (C.noop 0) 64
  pure (cp, ud)

roundTrip :: BS.BlockStore Address -> Assertion
roundTrip bs = do
  ident <- makeRandomId
  db <- Index.load bs ident
  atomically (Index.insert "keyhash" ("key", "value") db) >>= atomically
  db2 <- Index.load bs ident
  result <- atomically $ Index.lookup "keyhash" db2
  case result of
    Just (k, v) | v == "value" -> pure ()
    Just (k, v) -> fail ("expected value, got " ++ unpack v)
    _ -> fail "got nothin"

nextKeyAfterRemoval :: BS.BlockStore Address -> Assertion
nextKeyAfterRemoval bs = do
  ident <- makeRandomId
  db <- Index.load bs ident
  result <- atomically $ do
    _ <- Index.insert "1" ("k1", "v1") db
    _ <- Index.insert "2" ("k2", "v2") db
    _ <- Index.insert "3" ("k3", "v3") db
    _ <- Index.insert "4" ("k4", "v4") db
    _ <- Index.delete "2" db
    Index.lookupGT "1" db
  case result of
    Just (kh, (k, v)) | kh == "3" -> pure ()
    Just (kh, (k, v)) -> fail ("expected key 3, got " ++ unpack kh)
    Nothing -> fail "got nothin"

runGarbageCollection :: BS.BlockStore Address -> Assertion
runGarbageCollection bs = do
  ident <- makeRandomId
  db <- Index.load bs ident
  let kvp i = (pack . ("k" ++) . show $ i, pack . ("v" ++) . show $ i)
  result <- atomically $ do
    _ <- mapM (\i -> Index.insert (pack $ show i) (kvp i) db) [0..1001]
    _ <- mapM_ (\i -> Index.delete (pack $ show i) db) [2..1001]
    Index.lookup "1" db
  case result of
    Just (k, v) | v == "v1" -> pure ()
    o -> fail ("1. got unexpected value " ++ show o)
  result2 <- atomically $ Index.lookup "2" db
  case result2 of
    Nothing -> pure ()
    Just (k, o) -> fail ("2. got unexpected value " ++ unpack o)

prop_serializeDeserializeId :: Index.Identifier -> Bool
prop_serializeDeserializeId ident = ident == (Index.textToId . Index.idToText $ ident)

ioTests :: IO TestTree
ioTests = do
  blockStore <- MBS.make' makeRandomAddress makeAddress
  pure $ testGroup "KeyValueStore"
    [ testCase "roundTrip" (roundTrip blockStore)
    , testCase "nextKeyAfterRemoval" (nextKeyAfterRemoval blockStore)
    , testProperty "serializeDeserializeID" prop_serializeDeserializeId
    -- this takes almost two minutes to run on sfultong's machine
    --, testCase "runGarbageCollection" (runGarbageCollection genVar)
    ]

main = ioTests >>= defaultMain
