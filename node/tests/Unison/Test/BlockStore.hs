{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language OverloadedStrings #-}

module Unison.Test.BlockStore where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (zipWithM_, foldM, when, replicateM, (>=>))
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import EasyTest
import Unison.Runtime.Address
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as B
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C

makeRandomAddress :: IO Address
makeRandomAddress = Address <$> C.randomBytes (C.noop "dummypublickey") 64

roundTrip :: BS.BlockStore Address -> Test ()
roundTrip bs = do
  h <- io (BS.insert bs $ pack "v")
  v <- io (BS.lookup bs h)
  case v of
    Just v2 | unpack v2 == "v" -> pure ()
    a -> crash ("lookup returned " ++ show a)

roundTripSeries :: BS.BlockStore Address -> Test ()
roundTripSeries bs = do
  let seriesName = BS.Series $ pack "series"
  h <- io (BS.declareSeries bs seriesName)
  h2 <- io (BS.update bs seriesName h (pack "v"))
  case h2 of
    Nothing -> crash "got nothin"
    Just h2' -> do
      h3 <- io (BS.resolve bs seriesName)
      case h3 of
        Just h3' | h3' == h2' -> ok
        a -> crash ("resolve returned " ++ show a)

appendAppendUpdate :: BS.BlockStore Address -> Test ()
appendAppendUpdate bs = do
  let seriesName = BS.Series $ pack "series2"
  h <- io (BS.declareSeries bs seriesName)
  (Just h2) <- io (BS.append bs seriesName h (pack "v"))
  (Just h3) <- io (BS.append bs seriesName h2 (pack "v2"))
  vs <- io (BS.resolves bs seriesName)
  case vs of
    [h3', h2'] | h3 == h3' && h2 == h2' -> pure ()
    x -> crash ("got series list of " ++ show x)
  (Just h4) <- io (BS.update bs seriesName h3 (pack "v3"))
  vs2 <- io (BS.resolves bs seriesName)
  case vs2 of
    [h4'] | h4 == h4' -> ok
    x -> crash ("2. got series list of " ++ show x)

idempotentDeclare :: BS.BlockStore Address -> Test ()
idempotentDeclare bs = do
  let seriesName = BS.Series $ pack "series3"
  h <- io (BS.declareSeries bs seriesName)
  h2 <- io (BS.declareSeries bs seriesName)
  if h == h2 then ok
    else crash ("got back unequal hashes " ++ show h ++ " " ++ show h2)

cantChangeWithInvalidHash :: BS.BlockStore Address -> Test ()
cantChangeWithInvalidHash bs = do
  let seriesName = BS.Series $ pack "series4"
  let series2Name = BS.Series $ pack "series5"
  h <- io (BS.declareSeries bs seriesName)
  h2 <- io (BS.declareSeries bs series2Name)
  result <- io (BS.update bs seriesName h2 $ pack "value")
  if isNothing result then ok else crash "updated series without correct hash"

genByteString :: Test ByteString
genByteString = B.pack <$> listOf 12 word8

data BlockStoreMethod
  = Insert ByteString
  | Lookup
  | DeclareSeries BS.Series
  | Update ByteString
  | Append ByteString
  | Resolve
  | Resolves deriving (Eq, Show)

isDeclareSeries :: BlockStoreMethod -> Bool
isDeclareSeries (DeclareSeries _) = True
isDeclareSeries _ = False

data BlockStoreResult
  = Key Address
  | Data (Maybe ByteString)
  | NoKey
  | KeyList [Address] deriving (Eq, Show)

blockStoreMethod :: Test BlockStoreMethod
blockStoreMethod = do
  c <- int' 0 6
  case c of
    0 -> Insert <$> genByteString
    1 -> pure Lookup
    2 -> (DeclareSeries . BS.Series) <$> genByteString
    3 -> Update <$> genByteString
    4 -> Append <$> genByteString
    5 -> pure Resolve
    6 -> pure Resolves

data TestClient = TestClient
  { lastHandle :: Address
  , result :: BlockStoreResult
  , lastSeries :: BS.Series } deriving (Show)

runCommand :: BS.BlockStore Address -> BlockStoreMethod -> TestClient -> IO TestClient
runCommand bs command tc = case command of
  (Insert v) -> do
    r <- BS.insert bs v
    pure tc { result = Key r, lastHandle = r }
  Lookup -> do
    r <- BS.lookup bs (lastHandle tc)
    pure tc { result = Data r }
  (DeclareSeries s) -> do
    r <- BS.declareSeries bs s
    pure tc { result = Key r, lastHandle = r, lastSeries = s }
  (Update v) -> do
    r <- BS.update bs (lastSeries tc) (lastHandle tc) v
    pure tc { result = maybe NoKey Key r, lastHandle = fromMaybe (lastHandle tc) r }
  (Append v) -> do
    r <- BS.append bs (lastSeries tc) (lastHandle tc) v
    pure tc { result = maybe NoKey Key r, lastHandle = fromMaybe (lastHandle tc) r }
  Resolve -> do
    r <- BS.resolve bs (lastSeries tc)
    pure tc { result = maybe NoKey Key r, lastHandle = fromMaybe (lastHandle tc) r }
  Resolves -> do
    r <- BS.resolves bs (lastSeries tc)
    pure tc { result = KeyList r }

runMethods :: BS.BlockStore Address -> MVar.MVar TestClient -> TestClient -> [BlockStoreMethod] -> IO ThreadId
runMethods blockStore clientVar client =
  forkIO . (>>= MVar.putMVar clientVar) . foldM runMethod client where
  runMethod client method = runCommand blockStore method client

test :: BS.BlockStore Address -> Test ()
test bs = tests
  [ scope "roundTrip" (roundTrip bs)
  , scope "roundTripSeries" (roundTripSeries bs)
  , scope "appendAppendUpdate" (appendAppendUpdate bs)
  , scope "idempotentDeclare" (idempotentDeclare bs)
  , scope "cantChangeWithInvalidHash" (cantChangeWithInvalidHash bs)
  ]

-- todo: randomly generated tests
