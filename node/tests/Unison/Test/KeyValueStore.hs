module Unison.Test.KeyValueStore where

import Data.ByteString.Char8
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Runtime.Address
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.BlockStore as BS
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Runtime.KeyValueStore as KVS


type Prereqs r = (MVar.MVar r, BS.BlockStore Address)

makeRandomAddress :: RandomGen r => MVar.MVar r -> IO Address
makeRandomAddress genVar = do
  gen <- MVar.readMVar genVar
  let (hash, newGen) = random gen
  MVar.swapMVar genVar newGen
  pure hash

makeRandomBS :: RandomGen r => MVar.MVar r -> IO ByteString
makeRandomBS genVar = toBytes <$> makeRandomAddress genVar

makeRandomSeries :: RandomGen r => MVar.MVar r -> IO BS.Series
makeRandomSeries genVar = BS.Series <$> makeRandomBS genVar

makeRandomId :: RandomGen r => MVar.MVar r -> IO (BS.Series, BS.Series)
makeRandomId genVar = do
  cp <- makeRandomSeries genVar
  ud <- makeRandomSeries genVar
  pure (cp, ud)

roundTrip :: RandomGen r => Prereqs r -> Assertion
roundTrip (genVar, bs) = do

  ident <- makeRandomId genVar
  db <- KVS.load bs ident
  KVS.insert (pack "keyhash") (pack "key", pack "value") db
  db2 <- KVS.load bs ident
  result <- KVS.lookup (pack "keyhash") db2
  case result of
    Just (k, v) | unpack v == "value" -> pure ()
    Just (k, v) -> fail ("expected value, got " ++ unpack v)
    _ -> fail "got nothin"

nextKeyAfterRemoval :: RandomGen r => Prereqs r -> Assertion
nextKeyAfterRemoval (genVar, bs) = do
  ident <- makeRandomId genVar
  db <- KVS.load bs ident
  KVS.insert (pack "1") (pack "k1", pack "v1") db
  KVS.insert (pack "2") (pack "k2", pack "v2") db
  KVS.insert (pack "3") (pack "k3", pack "v3") db
  KVS.insert (pack "4") (pack "k4", pack "v4") db
  KVS.delete (pack "2") db
  result <- KVS.lookupGT (pack "1") db
  case result of
    Just (kh, (k, v)) | unpack kh == "3" -> pure ()
    Just (kh, (k, v)) -> fail ("expected key 3, got " ++ unpack kh)
    Nothing -> fail "got nothin"

runGarbageCollection :: RandomGen r => Prereqs r -> Assertion
runGarbageCollection (genVar, bs) = do
  ident <- makeRandomId genVar
  db <- KVS.load bs ident
  let kvp i = (pack . ("k" ++) . show $ i, pack . ("v" ++) . show $ i)
  mapM_ (\i -> KVS.insert (pack . show $ i) (kvp i) db) [0..1001]
  mapM_ (\i -> KVS.delete (pack . show $ i) db) [2..1001]
  result <- KVS.lookup (pack "1") db
  case result of
    Just (k, v) | unpack v == "v1" -> pure ()
    o -> fail ("1. got unexpected value " ++ show o)
  result2 <- KVS.lookup (pack "2") db
  case result2 of
    Nothing -> pure ()
    Just (k, o) -> fail ("2. got unexpected value " ++ unpack o)

ioTests :: IO TestTree
ioTests = do
  gen <- getStdGen
  genVar <- MVar.newMVar gen
  blockStore <- MBS.make' (makeRandomAddress genVar)
  let prereqs = (genVar, blockStore)
  pure $ testGroup "KeyValueStore"
    [ testCase "roundTrip" (roundTrip prereqs)
    , testCase "nextKeyAfterRemoval" (nextKeyAfterRemoval prereqs)
    -- this takes almost two minutes to run on sfultong's machine
    --, testCase "runGarbageCollection" (runGarbageCollection genVar)
    ]

main = ioTests >>= defaultMain
