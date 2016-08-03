{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language OverloadedStrings #-}

module Unison.Test.BlockStore where

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (zipWithM_, foldM, when, replicateM, (>=>))
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Unison.Runtime.Address
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as B
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.Tasty.HUnit as HU
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C

instance Arbitrary Address where
  arbitrary = (fromBytes . B.pack) <$> vectorOf 64 arbitrary

makeRandomAddress :: IO Address
makeRandomAddress = Address <$> C.randomBytes (C.noop "dummypublickey") 64

roundTrip :: BS.BlockStore Address -> HU.Assertion
roundTrip bs = do
  h <- BS.insert bs $ pack "v"
  v <- BS.lookup bs h
  case v of
    Just v2 | unpack v2 == "v" -> pure ()
    a -> fail ("lookup returned " ++ show a)

roundTripSeries :: BS.BlockStore Address -> HU.Assertion
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

appendAppendUpdate :: BS.BlockStore Address -> HU.Assertion
appendAppendUpdate bs = do
  let seriesName = BS.Series $ pack "series2"
  h <- BS.declareSeries bs seriesName
  (Just h2) <- BS.append bs seriesName h (pack "v")
  (Just h3) <- BS.append bs seriesName h2 (pack "v2")
  vs <- BS.resolves bs seriesName
  case vs of
    [h3', h2'] | h3 == h3' && h2 == h2' -> pure ()
    x -> fail ("got series list of " ++ show x)
  (Just h4) <- BS.update bs seriesName h3 (pack "v3")
  vs2 <- BS.resolves bs seriesName
  case vs2 of
    [h4'] | h4 == h4' -> pure ()
    x -> fail ("2. got series list of " ++ show x)

idempotentDeclare :: BS.BlockStore Address -> HU.Assertion
idempotentDeclare bs = do
  let seriesName = BS.Series $ pack "series3"
  h <- BS.declareSeries bs seriesName
  h2 <- BS.declareSeries bs seriesName
  if h == h2 then pure ()
    else fail ("got back unequal hashes " ++ show h ++ " " ++ show h2)

cantChangeWithInvalidHash :: BS.BlockStore Address -> HU.Assertion
cantChangeWithInvalidHash bs = do
  let seriesName = BS.Series $ pack "series4"
  let series2Name = BS.Series $ pack "series5"
  h <- BS.declareSeries bs seriesName
  h2 <- BS.declareSeries bs series2Name
  result <- BS.update bs seriesName h2 $ pack "value"
  if isNothing result then pure ()
    else fail "updated series without correct hash"

genByteString :: Gen ByteString
genByteString = B.pack <$> listOf (choose (0, 255))

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

shrinkBS :: ByteString -> ByteString
shrinkBS = B.pack . tail . B.unpack

instance Arbitrary BlockStoreMethod where
  arbitrary = do
    c <- choose (0,6::Int)
    case c of
      0 -> Insert <$> genByteString
      1 -> pure Lookup
      2 -> (DeclareSeries . BS.Series) <$> genByteString
      3 -> Update <$> genByteString
      4 -> Append <$> genByteString
      5 -> pure Resolve
      6 -> pure Resolves
  shrink (Insert bs) = [Insert (shrinkBS bs)]
  shrink (DeclareSeries (BS.Series bs)) =
    [DeclareSeries (BS.Series . shrinkBS $ bs)]
  shrink (Update bs) = [Update . shrinkBS $ bs]
  shrink (Append bs) = [Append . shrinkBS $ bs]
  shrink x = [x]

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

prop_allSeriesHashesAreValid :: BS.BlockStore Address -> Property
prop_allSeriesHashesAreValid bs = QCM.monadicIO $ do
  firstDeclareSeries <- QCM.pick $ (DeclareSeries . BS.Series) <$> genByteString
  firstUpdate <- QCM.pick $ Update <$> genByteString
  client <- QCM.run $ Prelude.foldr (\m tc -> tc >>= runCommand bs m)
                       (pure $ TestClient undefined undefined undefined)
                       [firstUpdate, firstDeclareSeries]
  clientMethods <- QCM.pick . flip suchThat (all (not . isDeclareSeries))
    $ (arbitrary :: Gen [BlockStoreMethod])
  newClient <- QCM.run $ foldr (\m c -> c >>= runCommand bs m) (pure client) clientMethods
  seriesHashes <- QCM.run . BS.resolves bs $ lastSeries newClient
  seriesValues <- QCM.run $ mapM (BS.lookup bs) seriesHashes
  -- make sure we didn't get any Nothing values for the series
  QCM.assert $ length seriesValues == length (catMaybes seriesValues)

prop_lastKeyIsValid :: BS.BlockStore Address -> Property
prop_lastKeyIsValid blockStore = QCM.monadicIO $ do
  firstDeclareSeries <- QCM.pick $ (DeclareSeries . BS.Series) <$> genByteString
  firstUpdate <- QCM.pick $ Update <$> genByteString
  interestingClient <- QCM.run $ Prelude.foldr (\m tc -> tc >>= runCommand blockStore m)
                       (pure $ TestClient undefined undefined undefined)
                       [firstUpdate, firstDeclareSeries]
  clientMethods <- filter (not . isDeclareSeries)
    <$> QCM.pick (arbitrary :: Gen [BlockStoreMethod])
  newClient <- QCM.run $ foldr
    (\m c -> c >>= runCommand blockStore m) (pure interestingClient) clientMethods
  lookupLast <- QCM.run . BS.lookup blockStore $ lastHandle newClient
  QCM.assert . not $ isNothing lookupLast

prop_SomeoneHasAValidKey :: BS.BlockStore Address -> Property
prop_SomeoneHasAValidKey blockStore = QCM.monadicIO $ do
  let clientNumber = 100
  firstDeclareSeries <- QCM.pick $ (DeclareSeries . BS.Series) <$> genByteString
  firstUpdate <- QCM.pick $ Update <$> genByteString
  interestingClient <- QCM.run $ Prelude.foldr (\m tc -> tc >>= runCommand blockStore m)
                       (pure $ TestClient undefined undefined undefined)
                       [firstUpdate, firstDeclareSeries]
  clientVars <- QCM.run $ replicateM clientNumber MVar.newEmptyMVar
  let clients = replicate clientNumber interestingClient
  clientMethods <- QCM.pick . vectorOf clientNumber $ arbitrary
  let filteredMethods = map (filter (not . isDeclareSeries)) clientMethods
  -- run all forks
  _ <- QCM.run . sequence
    $ zipWith3 (runMethods blockStore) clientVars clients filteredMethods
  -- wait for all forks to finish
  clientResults <- QCM.run
    $ mapM (MVar.takeMVar >=> BS.lookup blockStore . lastHandle) clientVars
  QCM.assert . not . Prelude.null . catMaybes $ clientResults

makeCases :: BS.BlockStore Address -> [TestTree]
makeCases bs = [ HU.testCase "roundTrip" (roundTrip bs)
               , HU.testCase "roundTripSeries" (roundTripSeries bs)
               , HU.testCase "appendAppendUpdate" (appendAppendUpdate bs)
               , HU.testCase "idempotentDeclare" (idempotentDeclare bs)
               , HU.testCase "cantChangeWithInvalidHash" (cantChangeWithInvalidHash bs)
               ]

-- the quickcheck tests seem to take forever.
makeExhaustiveCases :: BS.BlockStore Address -> [TestTree]
makeExhaustiveCases bs = makeCases bs ++
  [ testProperty "lastKeyIsValid" (prop_lastKeyIsValid bs)
  , testProperty "allSeriesHashesAreValid" (prop_allSeriesHashesAreValid bs)
  , testProperty "someoneHasValidKey" (prop_SomeoneHasAValidKey bs)
  ]
