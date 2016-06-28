module Unison.Store.MemBlockStore where


import Data.ByteString (ByteString)
import Debug.Trace (trace)
import System.Random
import Unison.Hash (Hash)
import Unison.Hashable
import Unison.Hash.Extra ()
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Digest.Murmur64 as Murmur
import qualified Data.Map.Lazy as Map
import qualified Unison.BlockStore as BS
import qualified Unison.Hash as Hash


data StoreData = StoreData
  { hashMap :: Map.Map Hash ByteString
  , seriesMap :: Map.Map BS.Series [Hash]
  }

-- think more about threading random state, since it's used two places now

makeHash :: ByteString -> Hash
makeHash = Hash.fromBytes . LB.toStrict
  . Builder.toLazyByteString . Builder.word64LE . Murmur.asWord64 . Murmur.hash64

randomHash :: RandomGen r => r -> (Hash.Hash, r)
randomHash = random

makeRandomHash :: RandomGen r => MVar.MVar r -> IO Hash
makeRandomHash genVar = MVar.modifyMVar genVar (pure . (\(a,b) -> (b,a)) . randomHash)

-- TODO implement real garbage collection for hashes removed from series
makeStore :: RandomGen r => MVar.MVar r -> MVar.MVar StoreData -> BS.BlockStore Hash
makeStore genVar mapVar =
  let insertStore (StoreData hashMap seriesMap) v =
        let hash = makeHash v
        in (StoreData (Map.insert hash v hashMap) seriesMap, hash)
      insert v = MVar.modifyMVar mapVar $ \store -> pure . insertStore store $ v
      lookup h = MVar.readMVar mapVar >>=
        (\(StoreData hashMap _) -> pure $ Map.lookup h hashMap)
      declareSeries series = do
        hash <- makeRandomHash genVar
        MVar.modifyMVar mapVar $ \store ->
          case Map.lookup series (seriesMap store) of
            Nothing ->
              pure (store { seriesMap = Map.insert series [hash] (seriesMap store)}, hash)
            Just (h:_) -> pure (store, h)
      update series hash v = MVar.modifyMVar mapVar $ \(StoreData hashMap sm) ->
        case Map.lookup series sm of
          Just (h:_) | h == hash ->
                   let (valueStore, hash) = insertStore (StoreData hashMap sm) v
                       newMap = Map.insert series [hash] $ seriesMap valueStore
                       finalStore = valueStore { seriesMap = newMap }
                   in pure (finalStore, Just hash)
          _ -> pure (StoreData hashMap sm, Nothing)
      append series hash v = MVar.modifyMVar mapVar $ \(StoreData hashMap sm) ->
        case Map.lookup series sm of
          Just (h:_) | h == hash ->
                   let (valueStore, hash) = insertStore (StoreData hashMap sm) v
                       newMap = Map.update (Just . (hash :)) series $ seriesMap valueStore
                       finalStore = valueStore { seriesMap = newMap }
                   in pure (finalStore, Just hash)
          _ -> pure (StoreData hashMap sm, Nothing)
      resolve s = MVar.readMVar mapVar >>=
        (\(StoreData _ seriesMap) -> pure . fmap head $ Map.lookup s seriesMap)
      resolves s = MVar.readMVar mapVar >>=
        (\(StoreData _ seriesMap) -> pure $ Map.findWithDefault [] s seriesMap)
  in BS.BlockStore insert lookup declareSeries update append resolve resolves
