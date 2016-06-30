module Unison.Test.BlockStore where

import Control.Monad (zipWithM_, (>=>))
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Unison.Hash (Hash)
import qualified Data.ByteString as B
import qualified Data.IORef as IORef
import qualified Test.QuickCheck.Monadic as QCM
import qualified Test.Tasty.HUnit as HU
import qualified Unison.BlockStore as BS
import qualified Unison.Hash as Hash

instance Arbitrary Hash where
  arbitrary = (Hash.fromBytes . B.pack) <$> vectorOf 64 arbitrary

roundTrip :: BS.BlockStore Hash -> HU.Assertion
roundTrip bs = do
  h <- BS.insert bs $ pack "v"
  v <- BS.lookup bs h
  case v of
    Just v2 | unpack v2 == "v" -> pure ()
    a -> fail ("lookup returned " ++ show a)

roundTripSeries :: BS.BlockStore Hash -> HU.Assertion
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

appendAppendUpdate :: BS.BlockStore Hash -> HU.Assertion
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

idempotentDeclare :: BS.BlockStore Hash -> HU.Assertion
idempotentDeclare bs = do
  let seriesName = BS.Series $ pack "series3"
  h <- BS.declareSeries bs seriesName
  h2 <- BS.declareSeries bs seriesName
  if h == h2 then pure ()
    else fail ("got back unequal hashes " ++ show h ++ " " ++ show h2)

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

data BlockStoreResult
  = Key Hash
  | Data (Maybe ByteString)
  | NoKey
  | KeyList [Hash] deriving (Eq, Show)

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

data TestClient = TestClient
  { lastHandle :: Hash
  , result :: BlockStoreResult
  , lastSeries :: BS.Series }

runCommand :: BS.BlockStore Hash -> BlockStoreMethod -> TestClient -> IO TestClient
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

runMethods :: BS.BlockStore Hash -> IORef.IORef TestClient -> [BlockStoreMethod] -> IO ()
runMethods blockStore clientVar = mapM_ (runMethod clientVar) where
  runMethod clientVar method = do
    client <- IORef.readIORef clientVar
    newClient <- runCommand blockStore method client
    IORef.writeIORef clientVar newClient

prop_lastKeyIsValid :: BS.BlockStore Hash -> Property
prop_lastKeyIsValid blockStore = QCM.monadicIO $ do
  firstDeclareSeries <- QCM.pick $ (DeclareSeries . BS.Series) <$> genByteString
  firstUpdate <- QCM.pick $ Update <$> genByteString
  interestingClient <- QCM.run $ Prelude.foldr (\m tc -> tc >>= runCommand blockStore m)
                       (pure $ TestClient undefined undefined undefined)
                       [firstUpdate, firstDeclareSeries]
  clientMethods <- QCM.pick (arbitrary :: Gen [BlockStoreMethod])
  _ <- QCM.run $ foldr
    (\m c -> c >>= runCommand blockStore m) (pure interestingClient) clientMethods
  lookupLast <- QCM.run . BS.lookup blockStore $ lastHandle interestingClient
  QCM.assert . not $ isNothing lookupLast

prop_SomeoneHasAValidKey :: BS.BlockStore Hash -> Property
prop_SomeoneHasAValidKey blockStore = QCM.monadicIO $ do
  let clientNumber = 100
  firstDeclareSeries <- QCM.pick $ (DeclareSeries . BS.Series) <$> genByteString
  firstUpdate <- QCM.pick $ Update <$> genByteString
  interestingClient <- QCM.run $ Prelude.foldr (\m tc -> tc >>= runCommand blockStore m)
                       (pure $ TestClient undefined undefined undefined)
                       [firstUpdate, firstDeclareSeries]
  clients <- QCM.run . mapM IORef.newIORef . Prelude.take clientNumber $ repeat interestingClient
  -- run all forks
  clientMethods <- QCM.pick . vectorOf clientNumber $ arbitrary
  _ <- QCM.run $ zipWithM_ (runMethods blockStore) clients clientMethods
  maybeValues <- QCM.run $
    mapM (IORef.readIORef >=> BS.lookup blockStore . lastHandle) clients
  QCM.assert . not . Prelude.null . catMaybes $ maybeValues

makeCases :: BS.BlockStore Hash -> [TestTree]
makeCases bs = [ HU.testCase "roundTrip" (roundTrip bs)
               , HU.testCase "roundTripSeries" (roundTripSeries bs)
               , HU.testCase "appendAppendUpdate" (appendAppendUpdate bs)
               , HU.testCase "idempotentDeclare" (idempotentDeclare bs)
               {-- these take forever
               , testProperty "lastKeyIsValid" (prop_lastKeyIsValid bs)
               , testProperty "someoneHasValidKey" (prop_SomeoneHasAValidKey bs)
--}
               ]
