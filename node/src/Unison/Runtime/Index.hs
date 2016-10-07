{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Unison.Runtime.Index
  (Unison.Runtime.Index.lookup
  ,Unison.Runtime.Index.delete
  ,Unison.Runtime.Index.insert
  ,Unison.Runtime.Index.lookupGT
  ,Unison.Runtime.Index.keys
  ,load
  ,loadSized
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial)
import Data.Foldable
import Data.Trie (Trie)
import GHC.Generics
import Unison.Cryptography
import qualified Data.ByteString as ByteString
import qualified Data.Bytes.Serial as S
import qualified Data.Trie as Trie
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.Runtime.Block as B

defaultSize :: Int -- default maximum index trie size, before another level of index is added
defaultSize = 1024

type Key = ByteString
type Value = ByteString

data TypedSeries a = TypedSeries { series :: BS.Series, defaultValue :: a } deriving Generic

data Index = Index { value :: TypedSeries (Maybe Value)
                   , branches :: Trie (TypedSeries Index)
                   } deriving Generic

data IndexState h t1 t2 t3 t4 t5 t6 = IndexState
  (BS.BlockStore h)
  (Cryptography t1 t2 t3 t4 t5 t6 ByteString)
  (TypedSeries Index)
  Int -- size limit for how big each index level can be

instance Serial BS.Series
instance Serial a => Serial (TypedSeries a)
instance Serial Index where
  serialize (Index value branches) = S.serialize value *> S.serialize (Trie.toList branches)
  deserialize = Index <$> S.deserialize <*> fmap Trie.fromList S.deserialize

toBlock :: Serial a => Cryptography t1 t2 t3 t4 t5 t6 ByteString -> TypedSeries a -> B.Block a
toBlock crypto ts =
  B.serial (defaultValue ts) . B.encrypted crypto . B.fromSeries $ series ts

deserialize :: Serial a => BS.BlockStore h -> Cryptography t1 t2 t3 t4 t5 t6 ByteString -> TypedSeries a -> IO a
deserialize bs crypto ts = B.get bs $ toBlock crypto ts

randomSeries :: Cryptography t1 t2 t3 t4 t5 t6 ByteString -> IO BS.Series
randomSeries crypto = BS.Series <$> C.randomBytes crypto 64

makeIndex :: Eq a => BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString
  -> Maybe Value -> Trie (TypedSeries Index) -> IO (TypedSeries Index)
makeIndex bs crypto value branches = do
  rootSeries@(BS.Series bytes) <- randomSeries crypto
  h0 <- BS.declareSeries bs rootSeries
  let
    valueSeries = TypedSeries (BS.Series $ bytes `mappend` "values") value
    node = Index valueSeries branches
    typedSeries = TypedSeries rootSeries node
    block = toBlock crypto typedSeries
  _ <- B.tryUpdate bs block h0 node
  pure typedSeries

emptyTT :: Eq a => BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString
  -> IO (TypedSeries Index)
emptyTT bs crypto = makeIndex bs crypto Nothing Trie.empty

isEmpty :: BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString -> Index -> IO Bool
isEmpty bs crypto index = if not . null $ branches index
  then pure False
  else do
  let
    block = toBlock crypto $ value index
  null <$> B.get bs block

load :: Eq a => BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString -> BS.Series
  -> IO (IndexState a t1 t2 t3 t4 t5 t6)
load bs crypto series = loadSized bs crypto series defaultSize

loadSized :: Eq a => BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString -> BS.Series
  -> Int -> IO (IndexState a t1 t2 t3 t4 t5 t6)
loadSized bs crypto series size = do
  let block = B.fromSeries series
      blockTyped = B.serialM (emptyTT bs crypto) block
  index <- B.get bs blockTyped
  _ <- B.modify' bs blockTyped (const index)
  pure $ IndexState bs crypto index size

{-
-- break up a Trie into an assoc list of smaller (Keys,Tries)
redistributeTrie :: Trie (TypedSeries Index) -> IO [(ByteString, TypedSeries Index)]
redistributeTrie trie = let
  redistributeBranch i = do
    let submap = Trie.submap i insertedBranches
        tailKeys = Trie.fromList
                   . map (\(k,v) -> (ByteString.tail k, v))
                   . Trie.toList
        submap' = tailKeys $ Trie.delete i submap
    value <- case Trie.lookup i insertedBranches of
      Nothing -> pure Nothing
      Just index -> do
        (Index value _) <- deserialize bs crypto index
        B.get bs $ toBlock crypto value
    makeIndex bs crypto value submap'
  allKeys = map ByteString.singleton [0..255]
  in do
  rebalancedIndexes <- mapM redistributeBranch allKeys
  pure . Trie.fromList $ zip allKeys rebalancedIndexes
-}

insert :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> Key -> Value -> IO ()
insert (IndexState bs crypto index maxSize) kh v = insert' index kh
  where
    insert' index kh = do
      (Index value branches) <- deserialize bs crypto index
      if ByteString.null kh
        then let block = toBlock crypto value
             in void (B.modify' bs block $ const (Just v))
        else case Trie.match branches kh of
               Just (_, branchIndex, remainingKH) -> insert' branchIndex remainingKH
               Nothing -> do
                 newIndex <- emptyTT bs crypto
                 insert' newIndex mempty
                 let insertedBranches = Trie.insert kh newIndex branches
                     redistributeBranch i = do
                       let submap = Trie.submap i insertedBranches
                           tailKeys = Trie.fromList
                                      . map (\(k,v) -> (ByteString.tail k, v))
                                      . Trie.toList
                           submap' = tailKeys $ Trie.delete i submap
                       value <- case Trie.lookup i insertedBranches of
                         Nothing -> pure Nothing
                         Just index -> do
                           (Index value _) <- deserialize bs crypto index
                           B.get bs $ toBlock crypto value
                       makeIndex bs crypto value submap'
                     allKeys = map ByteString.singleton [0..255]
                 balancedBranches <- if Trie.size insertedBranches <= maxSize
                   then pure insertedBranches
                   else
                   do
                     rebalancedIndexes <- mapM redistributeBranch allKeys
                     pure . Trie.fromList $ zip allKeys rebalancedIndexes
                 let newIndex = Index value balancedBranches
                 void $ B.modify' bs (toBlock crypto index) (const newIndex)

delete :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> Key -> IO ()
delete (IndexState bs crypto index _) kh = delete' index kh
  where
    delete' index kh = do
      (Index value branches) <- deserialize bs crypto index
      if ByteString.null kh
        then let block = toBlock crypto value
             in void (B.modify' bs block $ const Nothing)
        else case Trie.match branches kh of
               Just (_, branchIndex, remainingKH) -> do
                 delete' branchIndex remainingKH
                 -- calculating if the tree is empty is expensive.
                 -- TODO if it fails, earlier calls in the recursion stack should automatically fail
                 emptyTree <- isEmpty bs crypto $ Index value branches
                 when emptyTree $
                   let newTrie = Trie.delete kh branches
                       newIndex = Index value newTrie
                   in void $ B.modify' bs (toBlock crypto index) (const newIndex)
               _ -> pure () -- key didn't actually exist in this index

lookup :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> Key -> IO (Maybe Value)
lookup (IndexState bs crypto index _) kh = lookup' index kh
  where
    lookup' index kh = do
      (Index value branches) <- deserialize bs crypto index
      if ByteString.null kh
        then let block = toBlock crypto value
             in B.get bs block
        else case Trie.match branches kh of
               Just (_, branchIndex, remainingKH) -> lookup' branchIndex remainingKH
               _ -> pure Nothing

-- note: this won't produce an "ordered" traversal in any reasonable sense, but it should
-- at least iterate through all keys
lookupGT :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> Key
  -> IO (Maybe (Key, Value))
lookupGT (IndexState bs crypto index _) kh = lookupGT' index kh mempty
  where
    findAll index previousKH = do
      (Index value branches) <- deserialize bs crypto index
      currentVal <- B.get bs $ toBlock crypto value
      let getBranch k v = findAll v (previousKH `mappend` k)
          levelVal = fmap (\kv -> (previousKH, kv)) currentVal
          mergeOption old kv = liftM2 (<|>) old (uncurry getBranch kv)
      foldl' mergeOption (pure levelVal) $ Trie.toList branches
    lookupGT' index kh previousKH = do
      (Index _ branches) <- deserialize bs crypto index
      if ByteString.null kh
        then pure Nothing
        else
        do
          furtherMatch <- case Trie.match branches kh of
            Just (matchedKH, branchIndex, remainingKH) -> lookupGT' branchIndex remainingKH
                          (previousKH `mappend` matchedKH)
            _ -> pure Nothing
          let
            gtBranches = Trie.mapBy (\k v -> if k > kh then Just (k,v) else Nothing) branches
            getBranch k v = findAll v (previousKH `mappend` k)
            mergeOption old kv = liftM2 (<|>) old (uncurry getBranch kv)
          foldl' mergeOption (pure furtherMatch) gtBranches

keys :: Eq a => IndexState a t1 t2 t3 t4 t5 t6 -> IO [Key]
keys (IndexState bs crypto index _) = keys' index mempty <*> pure []
  where
    keys' index kh = do
      (Index value branches) <- deserialize bs crypto index
      currentVal <- B.get bs $ toBlock crypto value
      let getBranch k v = keys' v (kh `mappend` k)
          mergeVals kv old = liftM2 (.) (uncurry getBranch kv) old
      leafKeys <- foldr mergeVals (pure id) $ Trie.toList branches
      pure $ if null currentVal then leafKeys else (kh:) . leafKeys
