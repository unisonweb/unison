{-# Language DeriveGeneric #-}
module Unison.Node.UnisonBlockStore where

import Control.Applicative
import Control.Concurrent.STM (atomically)
import Data.ByteString.Char8 (pack)
import Data.Bytes.Serial (Serial)
import Data.Map (Map)
import GHC.Generics
import Unison.BlockStore (BlockStore)
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import Unison.Metadata (Metadata)
import Unison.Metadata.Extra ()
import Unison.Node.Store (Store, Store(..))
import Unison.Reference (Reference)
import Unison.Reference.Extra ()
import Unison.Term (Term)
import Unison.Term.Extra ()
import Unison.Type (Type)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.BlockStore as BS
import qualified Unison.Note as Note
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.Block as B
import qualified Unison.Runtime.Journal as J

type JournaledStore v = J.Journal (StoreData v) (Update v)

data StoreData v = StoreData
  { termMap :: Map Hash (Term v)
  , annotationMap :: Map Reference (Type v)
  , metadataMap :: Map Reference (Metadata v Reference)
  } deriving Generic
instance (Ord v, Serial v) => Serial (StoreData v)

data Update v
 = WriteTerm Hash (Term v)
 | AnnotateTerm Reference (Type v)
 | WriteMetadata Reference (Metadata v Reference)
 deriving Generic
instance (Ord v, Serial v) => Serial (Update v)

make :: (Eq a, Serial v, Ord v) => BlockStore a ->  IO (Store IO v)
make bs = let
  maybeToEither b Nothing = Left b
  maybeToEither _ (Just a) = Right a
  emptyStore = StoreData Map.empty Map.empty Map.empty
  keyframeBlock = B.serial emptyStore . B.fromSeries . BS.Series $ pack "keyframes"
  updateBlock = B.serial Nothing . B.fromSeries . BS.Series $ pack "updates"
  apply (WriteTerm hash term) (StoreData trm tym mm) =
    StoreData (Map.insert hash term trm) tym mm
  apply (AnnotateTerm ref typ) (StoreData trm tym mm) =
    StoreData trm (Map.insert ref typ tym) mm
  apply (WriteMetadata ref met) (StoreData trm tym mm) =
    StoreData trm tym (Map.insert ref met mm)
  in do
  journaledStore <- J.fromBlocks bs apply keyframeBlock updateBlock
  let readTerm h = Note.noted . atomically $ (maybeToEither (Note.note $ "term not found " ++ show h) . Map.lookup h . termMap)
        <$> J.get journaledStore
      typeOfTerm r = Note.noted . atomically $ (maybeToEither (Note.note $ "type not found " ++ show r) . Map.lookup r . annotationMap)
        <$> J.get journaledStore
      readMetadata r = Note.noted . atomically $ (maybeToEither (Note.note $ "metadata not found " ++ show r) . Map.lookup r . metadataMap)
        <$> J.get journaledStore
      writeTerm h t = Note.lift $ J.update (WriteTerm h t) journaledStore
      annotateTerm r t = Note.lift $ J.update (AnnotateTerm r t) journaledStore
      writeMetadata r m = Note.lift $ J.update (WriteMetadata r m) journaledStore
      hashes limit =
        let limitf = maybe id Set.intersection limit
            termHashes = Note.lift . atomically
              $ (limitf . Set.map Reference.Derived . Map.keysSet . termMap)
              <$> J.get journaledStore
            metadataHashes = Note.lift . atomically $ (limitf . Map.keysSet . metadataMap)
              <$> J.get journaledStore
        in liftA2 Set.union termHashes metadataHashes
  pure $ Store hashes readTerm writeTerm typeOfTerm annotateTerm readMetadata writeMetadata
