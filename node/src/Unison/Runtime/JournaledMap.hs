{-# Language DeriveGeneric #-}

module Unison.Runtime.JournaledMap where

import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial)
import Data.Map (Map)
import GHC.Generics
import qualified Data.Map as Map
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.Runtime.Block as B
import qualified Unison.Runtime.Journal as J

type JournaledMap k v = J.Journal (Map k v) (Update k v)

data Update k v
  = Insert k v
  | Delete k
  | Noop
  | Clear
  deriving Generic
instance (Serial k, Serial v) => Serial (Update k v)

insert :: (Serial k, Serial v) => k -> v -> JournaledMap k v -> IO ()
insert k v = J.update (Insert k v)

delete :: k -> JournaledMap k v -> IO ()
delete k = J.update (Delete k)

lookup :: (Serial k, Ord k, Serial v) => k -> JournaledMap k v -> IO (Maybe v)
lookup k j = atomically $ Map.lookup k <$> J.get j

keys :: JournaledMap k v -> IO [k]
keys j = Map.keys <$> atomically (J.get j)

fromBlocks :: (Eq h, Ord k, Serial k, Serial v)
           => BS.BlockStore h
           -> B.Block (Maybe ByteString)
           -> B.Block (Maybe ByteString)
           -> IO (JournaledMap k v)
fromBlocks bs keyframe diffs = J.fromBlocks bs Noop apply ks ds where
  ks = B.serial Map.empty $ keyframe
  ds = B.serial Noop $ diffs
  apply (Insert k v) m = Map.insert k v m
  apply (Delete k) m = Map.delete k m
  apply Clear _ = Map.empty
  apply Noop m = m

fromSeries :: (Eq h, Ord k, Serial k, Serial v)
           => BS.BlockStore h
           -> BS.Series
           -> BS.Series
           -> IO (JournaledMap k v)
fromSeries bs keyframe diffs = fromBlocks bs (B.fromSeries keyframe) (B.fromSeries diffs)

fromEncryptedSeries :: (Eq h, Ord k, Serial k, Serial v)
                    => C.Cryptography t1 t2 t3 t4 t5 t6 ByteString
                    -> BS.BlockStore h
                    -> BS.Series
                    -> BS.Series
                    -> IO (JournaledMap k v)
fromEncryptedSeries crypto bs keyframe diffs =
  fromBlocks bs (B.encrypted crypto (B.fromSeries keyframe)) (B.encrypted crypto (B.fromSeries diffs))
