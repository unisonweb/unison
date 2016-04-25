module Unison.Test.KeyValueStore where

import Data.ByteString.Char8
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Hash (Hash)
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.Runtime.KeyValueStore as KVS

makeRandomHash :: RandomGen r => MVar.MVar r -> IO Hash
makeRandomHash genVar = do
  gen <- MVar.readMVar genVar
  let (hash, newGen) = random gen
  MVar.swapMVar genVar newGen
  pure hash

roundTrip :: RandomGen r => MVar.MVar r -> Assertion
roundTrip genVar = do
  hash <- makeRandomHash genVar
  db <- KVS.load hash
  KVS.insert (pack "key") (pack "value") db
  KVS.close db
  db2 <- KVS.load hash
  result <- KVS.lookup (pack "key") db2
  case result of
    Just v | unpack v == "value" -> pure ()
    Just v -> fail ("expected value, got " ++ unpack v)
    _ -> fail "got nothin"
  KVS.close db2

nextKeyAfterRemoval :: RandomGen r => MVar.MVar r -> Assertion
nextKeyAfterRemoval genVar = do
  hash <- makeRandomHash genVar
  db <- KVS.load hash
  KVS.insert (pack "1") (pack "v1") db
  KVS.insert (pack "2") (pack "v2") db
  KVS.insert (pack "3") (pack "v3") db
  KVS.insert (pack "4") (pack "v4") db
  KVS.delete (pack "2") db
  result <- KVS.lookupGT (pack "1") db
  case result of
    Just (k, v) | unpack k == "3" -> pure ()
    Just (k, v) -> fail ("expected key 3, got " ++ unpack k)
    Nothing -> fail "got nothin"
  KVS.close db

setupGen :: IO (MVar.MVar StdGen)
setupGen = do
  gen <- getStdGen
  MVar.newMVar gen

ioTests :: IO TestTree
ioTests = do
  gen <- getStdGen
  genVar <- MVar.newMVar gen
  pure $ testGroup "KeyValueStore"
    [ testCase "roundTrip" (roundTrip genVar)
    , testCase "nextKeyAfterRemoval" (nextKeyAfterRemoval genVar)
    ]

main = ioTests >>= defaultMain
