module Unison.BlockStore.MemBlockStore where


import Data.ByteString (ByteString)
import System.Random
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Digest.Murmur64 as Murmur
import qualified Data.IORef as IORef
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

makeRandomHash :: RandomGen r => IORef.IORef r -> IO Hash
makeRandomHash genVar = IORef.atomicModifyIORef genVar ((\(a,b) -> (b,a)) . randomHash)

-- TODO implement real garbage collection for hashes removed from series
make :: IO Hash -> IORef.IORef StoreData -> BS.BlockStore Hash
make genHash mapVar =
  let insertStore (StoreData hashMap seriesMap) v =
        let hash = makeHash v
        in (StoreData (Map.insert hash v hashMap) seriesMap, hash)
      insert v = IORef.atomicModifyIORef mapVar $ \store -> insertStore store v
      lookup h = IORef.readIORef mapVar >>=
        (\(StoreData hashMap _) -> pure $ Map.lookup h hashMap)
      declareSeries series = do
        hash <- genHash
        IORef.atomicModifyIORef mapVar $ \store ->
          case Map.lookup series (seriesMap store) of
            Nothing ->
             (store { seriesMap = Map.insert series [hash] (seriesMap store)}, hash)
            Just (h:_) -> (store, h)
            _ -> error "MemBlockStore.declareSeries had empty list of hashes in series"
      update series hash v = IORef.atomicModifyIORef mapVar $ \(StoreData hashMap sm) ->
        case Map.lookup series sm of
          Just (h:_) | h == hash ->
                   let (valueStore, hash) = insertStore (StoreData hashMap sm) v
                       newMap = Map.insert series [hash] $ seriesMap valueStore
                       finalStore = valueStore { seriesMap = newMap }
                   in (finalStore, Just hash)
          Just [] -> error "MemBlockStore.update had empty list of hashes in series"
          _ -> (StoreData hashMap sm, Nothing)
      append series hash v = IORef.atomicModifyIORef mapVar $ \(StoreData hashMap sm) ->
        case Map.lookup series sm of
          Just (h:_) | h == hash ->
                   let (valueStore, hash) = insertStore (StoreData hashMap sm) v
                       newMap = Map.update (Just . (hash :)) series $ seriesMap valueStore
                       finalStore = valueStore { seriesMap = newMap }
                   in (finalStore, Just hash)
          Just [] -> error "MemBlockStore.append had empty list of hashes in series"
          _ -> (StoreData hashMap sm, Nothing)
      resolve s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap) -> pure . fmap head $ Map.lookup s seriesMap)
      resolves s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap) -> pure $ Map.findWithDefault [] s seriesMap)
  in BS.BlockStore insert lookup declareSeries update append resolve resolves

make' :: IO Hash -> IO (BS.BlockStore Hash)
make' genHash = IORef.newIORef (StoreData Map.empty Map.empty) >>= pure . make genHash
