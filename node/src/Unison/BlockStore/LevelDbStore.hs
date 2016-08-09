{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Unison.BlockStore.LevelDbStore where

import qualified Data.ByteString as B
import qualified Unison.BlockStore as BS

#ifdef leveldb
import Data.Bytes.Serial (serialize, Serial(..))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Database.LevelDB.Base as DB

instance Serial BS.Series

dbOptions :: DB.Options
dbOptions = DB.defaultOptions { DB.createIfMissing = True }

make :: (Ord a, Serial a) => IO a -> (B.ByteString -> a) -> FilePath -> IO (BS.BlockStore a)
make genAddress hash path = do
  db <- DB.open path dbOptions
  let putDB :: (Serial k, Serial v) =>  Word8 -> k -> v -> IO ()
      putDB o k v = let sk = B.cons o . Put.runPutS $ serialize k
                        sv = Put.runPutS $ serialize v
                    in DB.put db DB.defaultWriteOptions sk sv
      getDB :: (Serial k, Serial v) => Word8 -> k -> IO (Maybe v)
      getDB o k = let sk = B.cons o . Put.runPutS $ serialize k in do
        readValue <- DB.get db DB.defaultReadOptions sk
        case readValue >>= pure . Get.runGetS deserialize of
          Nothing -> pure Nothing
          Just (Left s) -> fail s
          Just (Right v) -> pure $ pure v
      deleteDB :: Serial k => Word8 -> k -> IO ()
      deleteDB o k = let sk = B.cons o . Put.runPutS $ serialize k in
        DB.delete db DB.defaultWriteOptions sk
      insertHashMap :: (Serial k, Serial v) => k -> v -> IO ()
      insertHashMap = putDB 0
      insertSeriesAddresses :: (Serial k, Serial v) => k -> v -> IO ()
      insertSeriesAddresses = putDB 1
      insertSeriesLastAddress :: (Serial k, Serial v) => k -> v -> IO ()
      insertSeriesLastAddress = putDB 2
      insertPermanent :: Serial k => k -> IO ()
      insertPermanent p = putDB 3 p $ B.pack [1]
      getHashMap :: (Serial k, Serial v) => k -> IO (Maybe v)
      getHashMap = getDB 0
      getSeriesAddresses :: (Serial k, Serial v) => k -> IO (Maybe v)
      getSeriesAddresses = getDB 1
      getSeriesLastAddress :: (Serial k, Serial v) => k -> IO (Maybe v)
      getSeriesLastAddress = getDB 2
      insert v = do
        let address = hash v
        insertHashMap address v
        insertPermanent address
        pure address
      lookup = getHashMap
      declareSeries s = do
        existingSeries <- getSeriesLastAddress s
        case existingSeries of
          Just a -> pure a
          Nothing -> do
            address <- genAddress
            insertSeriesLastAddress s address
            pure address
      deleteSeries s = do
        deleteDB 1 s
        deleteDB 2 s
      update series address v = do
        lastAddress <- getSeriesLastAddress series
        case lastAddress of
          Nothing -> pure Nothing
          Just lastAddress -> if address == lastAddress then do
              let newAddress = hash v
              insertHashMap newAddress v
              insertSeriesLastAddress series newAddress
              insertSeriesAddresses series [newAddress]
              pure $ Just newAddress
            else pure Nothing
      append series address v = do
        lastAddress <- getSeriesLastAddress series
        case lastAddress of
          Nothing -> pure Nothing
          Just lastAddress -> if address == lastAddress then do
              let newAddress = hash v
              insertHashMap newAddress v
              insertSeriesLastAddress series newAddress
              existingList <- getSeriesAddresses series
              case existingList of
                Just existingList -> insertSeriesAddresses series (newAddress : existingList)
                Nothing -> insertSeriesAddresses series [newAddress]
              pure $ Just newAddress
            else pure Nothing
      resolve series = getSeriesAddresses series >>= \addresses -> pure
        $ maybe Nothing (Just . head) addresses
      resolves series = getSeriesAddresses series >>= \addresses -> pure
        $ fromMaybe [] addresses
  pure $ BS.BlockStore insert lookup declareSeries deleteSeries update append resolve resolves

#else
make :: Ord a => IO a -> (B.ByteString -> a) -> FilePath -> IO (BS.BlockStore a)
make _ _ _ = fail "leveldb support not enabled"
#endif
