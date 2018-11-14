{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Serialization where

import Data.Bytes.Get (MonadGet, runGetS)
import Data.Bytes.Put (MonadPut, runPutS)
import Data.ByteString (ByteString, readFile, writeFile)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude hiding (readFile, writeFile)

type Get a = forall m . MonadGet m => m a
type Put a = forall m . MonadPut m => a -> m ()

data Format a = Format {
  get :: Get a,
  put :: Put a
}

getFromBytes :: Get a -> ByteString -> Maybe a
getFromBytes getA bytes =
  case runGetS getA bytes of Left _ -> Nothing; Right a -> Just a

getFromFile :: Get a -> FilePath -> IO (Maybe a)
getFromFile getA file = do
  b <- doesFileExist file
  if b then getFromBytes getA <$> readFile file else pure Nothing

putBytes :: Put a -> a -> ByteString
putBytes put a = runPutS (put a)

putWithParentDirs :: Put a -> FilePath -> a -> IO ()
putWithParentDirs putA file a = do
  createDirectoryIfMissing True (takeDirectory file)
  writeFile file $ putBytes putA a
