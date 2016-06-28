module Unison.Test.MemBlockStore where

import Data.ByteString.Char8
import Data.Map as Map
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Hash (Hash)
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.BlockStore as BS
import qualified Unison.Store.MemBlockStore as MBS
import qualified Unison.Store.FileBlockStore as FBS
import qualified System.Directory as Directory

roundTrip :: BS.BlockStore Hash -> Assertion
roundTrip bs = do
  h <- BS.insert bs $ pack "v"
  v <- BS.lookup bs h
  case v of
    Just v2 | unpack v2 == "v" -> pure ()
    a -> fail ("lookup returned " ++ show a)

roundTripSeries :: BS.BlockStore Hash -> Assertion
roundTripSeries bs = do
  let seriesName = BS.Series $ pack "series"
  h <- BS.declareSeries bs seriesName
  h2 <- BS.update bs seriesName h (pack "v")
  case h2 of
    Nothing -> fail "got nothin"
    Just h2' -> do
      h3 <- BS.resolve bs seriesName
      case h3 of
        Just h3' | h3' == h2' -> pure ()
        a -> fail ("resolve returned " ++ show a)

appendAppendUpdate :: BS.BlockStore Hash -> Assertion
appendAppendUpdate bs = do
  let seriesName = BS.Series $ pack "series2"
  h <- BS.declareSeries bs seriesName
  (Just h2) <- BS.append bs seriesName h (pack "v")
  (Just h3) <- BS.append bs seriesName h2 (pack "v2")
  vs <- BS.resolves bs seriesName
  case vs of
    [h3', h2', h1'] | h3 == h3' && h2 == h2' && h == h1' -> pure ()
    x -> fail ("got series list of " ++ show x)
  (Just h4) <- BS.update bs seriesName h3 (pack "v3")
  vs2 <- BS.resolves bs seriesName
  case vs2 of
    [h4'] | h4 == h4' -> pure ()
    x -> fail ("2. got series list of " ++ show x)

idempotentDeclare :: BS.BlockStore Hash -> Assertion
idempotentDeclare bs = do
  let seriesName = BS.Series $ pack "series3"
  h <- BS.declareSeries bs seriesName
  h2 <- BS.declareSeries bs seriesName
  if h == h2 then pure ()
    else fail ("got back unequal hashes " ++ show h ++ " " ++ show h2)

ioTests :: IO (TestTree, IO ())
ioTests = do
  gen <- getStdGen
  genVar <- MVar.newMVar gen
  mapVar <- MVar.newMVar $ MBS.StoreData Map.empty Map.empty
  let store = MBS.makeStore genVar mapVar
  acidState <- FBS.initStore "temp"
  let fileStore = FBS.makeStore genVar acidState
  pure ( testGroup "BlockStore"
    [ testCase "roundTrip" (roundTrip store)
    , testCase "roundTripSeries" (roundTripSeries store)
    , testCase "appendAppendUpdate" (appendAppendUpdate store)
    , testCase "idempotentDeclare" (idempotentDeclare store)
    , testCase "FileRoundTrip" (roundTrip fileStore)
    , testCase "FileRoundTripSeries" (roundTripSeries fileStore)
    , testCase "FileAppendAppendUpdate" (appendAppendUpdate fileStore)
    , testCase "FileIdempotentDeclare" (idempotentDeclare fileStore)
    ], Directory.removeDirectoryRecursive "temp")
