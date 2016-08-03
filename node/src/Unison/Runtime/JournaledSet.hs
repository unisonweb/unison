{-# Language DeriveGeneric #-}

module Unison.Runtime.JournaledSet where

import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial)
import Data.Set (Set)
import GHC.Generics
import qualified Data.Set as Set
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.Runtime.Block as B
import qualified Unison.Runtime.Journal as J

type JournaledSet s = J.Journal (Set s) (Update s)

data Update s
  = Insert s
  | Delete s
  | UnionUpdate (Set s)
  | IntersectUpdate (Set s)
  | DifferenceUpdate (Set s)
  deriving Generic
instance (Ord s, Serial s) => Serial (Update s)

insert :: Serial s => s -> JournaledSet s -> IO ()
insert s = J.update (Insert s)

delete :: s -> JournaledSet s -> IO ()
delete s = J.update (Delete s)

contains :: Ord s => s -> JournaledSet s -> IO Bool
contains s set = atomically $ Set.member s <$> J.get set

unionUpdate :: Set s -> JournaledSet s -> IO ()
unionUpdate s = J.update (UnionUpdate s)

intersectUpdate :: Set s -> JournaledSet s -> IO ()
intersectUpdate s = J.update (IntersectUpdate s)

differenceUpdate :: Set s -> JournaledSet s -> IO ()
differenceUpdate s = J.update (DifferenceUpdate s)

fromBlocks :: (Eq h, Ord s, Serial s)
  => BS.BlockStore h
  -> B.Block (Maybe ByteString)
  -> B.Block (Maybe ByteString)
  -> IO (JournaledSet s)
fromBlocks bs keyframe diffs = J.fromBlocks bs apply ks ds where
  ks = B.serial Set.empty $ keyframe
  ds = B.serial Nothing $ diffs
  apply (Insert s) = Set.insert s
  apply (Delete s) = Set.delete s
  apply (UnionUpdate b) = Set.union b
  apply (IntersectUpdate b) = Set.intersection b
  apply (DifferenceUpdate b) = Set.difference b

fromSeries :: (Eq h, Ord s, Serial s)
  => BS.BlockStore h
  -> BS.Series
  -> BS.Series
  -> IO (JournaledSet s)
fromSeries bs keyframe diffs = fromBlocks bs (B.fromSeries keyframe) (B.fromSeries diffs)

fromEncryptedSeries :: (Eq h, Ord s, Serial s)
  => C.Cryptography t1 t2 t3 t4 t5 t6 ByteString
  -> BS.BlockStore h
  -> BS.Series
  -> BS.Series
  -> IO (JournaledSet s)
fromEncryptedSeries crypto bs keyframe diffs = let crypted s = B.encrypted crypto (B.fromSeries s) in fromBlocks bs (crypted keyframe) (crypted diffs)
