{-# Language DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Runtime.JournaledTrie where

import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial(..))
import GHC.Generics
import qualified Unison.BlockStore as BS
import qualified Unison.Runtime.Block as B
import qualified Unison.Runtime.Journal as J
import qualified Data.Trie as T
import qualified Data.Trie.Internal as TI

type JournaledTrie v = J.Journal (T.Trie v) (Maybe (Update v))

data Update v
  = Insert ByteString v
  | Delete ByteString
  | DeleteSubtree ByteString
  | UnionL (T.Trie v)
  deriving Generic
instance Serial v => Serial (Update v)

instance Serial v => Serial (T.Trie v) where
  serialize t = serialize (T.toList t)
  deserialize = T.fromList <$> deserialize

fromBlocks :: (Eq h, Serial v)
           => BS.BlockStore h
           -> B.Block (Maybe ByteString)
           -> B.Block (Maybe ByteString)
           -> IO (JournaledTrie v)
fromBlocks bs keyframe diffs = J.fromBlocks bs Nothing apply ks ds where
  ks = B.serial T.empty $ keyframe
  ds = B.serial Nothing $ diffs
  apply Nothing t = t
  apply (Just (Insert k b)) t = T.insert k b t
  apply (Just (UnionL t2)) t = T.unionL t t2
  apply (Just (Delete k)) t = T.delete k t
  apply (Just (DeleteSubtree k)) t =
    -- somewhat insane way of deleting a subtree of the Trie
    TI.alterBy_ (\_ _ _ _ -> (Nothing, T.empty)) k undefined t
