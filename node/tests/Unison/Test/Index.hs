{-# Language TypeSynonymInstances #-}
{-# Language OverloadedStrings #-}
module Unison.Test.Index where

import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import System.Random
import EasyTest
import Unison.Runtime.Address
import Unison.Test.BlockStore (makeRandomAddress)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as B
import qualified Unison.BlockStore as BS
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Cryptography as C
import qualified Unison.Runtime.Index as Index

makeRandomId :: IO BS.Series
makeRandomId = BS.Series <$> C.randomBytes (C.noop "dummypublickey") 64

crypto :: C.Cryptography ByteString () () () ByteString ByteString ByteString
crypto = C.noop "dummypublickey"

roundTrip :: BS.BlockStore Address -> Test ()
roundTrip bs = do
  ident <- io $ makeRandomId
  db <- io $ Index.load bs crypto ident
  io $ Index.insert db "key" "value"
  db2 <- io $ Index.load bs crypto ident
  result <- io $ Index.lookup db2 "key"
  expect (result == Just "value")

nextKeyAfterRemoval :: BS.BlockStore Address -> Test ()
nextKeyAfterRemoval bs = do
  ident <- io makeRandomId
  db <- io $ Index.load bs crypto ident
  io $ do
    Index.insert db "k1" "v1"
    Index.insert db "k2" "v2"
    Index.insert db "k3" "v3"
    Index.insert db "k4" "v4"
    Index.delete db "k2"
  result <- io $ Index.lookupGT db "k1"
  case result of
    Just (k, v) | k == "k3" -> ok
    Just (k, v) -> crash ("expected key 3, got " ++ unpack k)
    Nothing -> crash "got nothin"

keysAfterResize :: BS.BlockStore Address -> Test ()
keysAfterResize bs = do
  ident <- io makeRandomId
  db <- io (Index.loadSized bs crypto ident 10)
  let kvp =  pack . ("v" ++) . show
      expected = map (pack . show) [15..30]
  io $ mapM_ (\i -> Index.insert db (pack $ show i) (kvp i)) [15..30]
  result <- io (Index.keys db)
  expect (result == expected)

resizeWithKeyLongSharedPrefix :: BS.BlockStore Address -> Test ()
resizeWithKeyLongSharedPrefix bs = do
  ident <- io makeRandomId
  db <- io $ Index.loadSized bs crypto ident 10
  let kvp =  pack . ("v" ++) . show
      expected = map (pack . show) [100015..100030]
  io $ mapM_ (\i -> Index.insert db (pack $ show i) (kvp i)) [100015..100030]
  result <- io $ Index.keys db
  expect (result == expected)

test :: Test ()
test = scope "Index" $ do
  blockStore <- io $ MBS.make' makeRandomAddress makeAddress
  tests
    [ scope "roundTrip" (roundTrip blockStore)
    , scope "nextKeyAfterRemoval" (nextKeyAfterRemoval blockStore)
    , scope "keysAfterResize" (keysAfterResize blockStore)
    , scope "resizeWithKeyLongSharedPrefix" (resizeWithKeyLongSharedPrefix blockStore)
    ]
