{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Serialization where

import Data.ByteString (ByteString, readFile, writeFile)
import Data.Bytes.Get (MonadGet, runGetS)
import Data.Bytes.Put (MonadPut, runPutS)
import System.FilePath (takeDirectory)
import UnliftIO (MonadIO, liftIO)
import UnliftIO.Directory (createDirectoryIfMissing, doesFileExist)
import Prelude hiding (readFile, writeFile)

type Get a = forall m. MonadGet m => m a

type Put a = forall m. MonadPut m => a -> m ()

-- todo: do we use this?
data Format a = Format
  { get :: Get a,
    put :: Put a
  }

getFromBytes :: Get a -> ByteString -> Maybe a
getFromBytes getA bytes =
  case runGetS getA bytes of Left _ -> Nothing; Right a -> Just a

getFromFile :: MonadIO m => Get a -> FilePath -> m (Maybe a)
getFromFile getA file = do
  b <- doesFileExist file
  if b then getFromBytes getA <$> liftIO (readFile file) else pure Nothing

getFromFile' :: MonadIO m => Get a -> FilePath -> m (Either String a)
getFromFile' getA file = do
  b <- doesFileExist file
  if b
    then runGetS getA <$> liftIO (readFile file)
    else pure . Left $ "No such file: " ++ file

putBytes :: Put a -> a -> ByteString
putBytes put a = runPutS (put a)

putWithParentDirs :: MonadIO m => Put a -> FilePath -> a -> m ()
putWithParentDirs putA file a = do
  createDirectoryIfMissing True (takeDirectory file)
  liftIO . writeFile file $ putBytes putA a
