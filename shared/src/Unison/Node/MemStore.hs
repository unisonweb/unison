module Unison.Node.MemStore (make) where

import Data.Map (Map)
import Unison.Hash (Hash)
import Unison.Metadata (Metadata)
import Unison.Note (Noted)
import Unison.Node.Store (Store(Store))
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Note as Note

-- | Store implementation that just uses a local `MVar`
make :: IO (Store IO v)
make = store <$> MVar.newMVar (S Map.empty Map.empty Map.empty) where
  store v = Store (withS v hashes)
                  (withS v readTerm)
                  (withS2 v writeTerm)
                  (withS v typeOfTerm)
                  (withS2 v annotateTerm)
                  (withS v readMetadata)
                  (withS2 v writeMetadata) where
    hashes s limit =
      pure (maybe id Set.intersection limit (Map.keysSet (metadata' s)))
    readTerm s hash =
      Note.fromMaybe (unknown "hash" hash) $ Map.lookup hash (terms' s)
    writeTerm s hash t =
      Note.lift . set v $ s { terms' = Map.insert hash t (terms' s) }
    typeOfTerm s ref =
      Note.fromMaybe (unknown "reference" ref) $ Map.lookup ref (typeOfTerm' s)
    annotateTerm s ref t =
      Note.lift . set v $ s { typeOfTerm' = Map.insert ref t (typeOfTerm' s) }
    readMetadata s ref =
      Note.fromMaybe (unknown "reference" ref) $ Map.lookup ref (metadata' s)
    writeMetadata s ref md =
      Note.lift . set v $ s { metadata' = Map.insert ref md (metadata' s) }
    unknown :: Show r => String -> r -> String
    unknown msg r = "unknown " ++ msg ++ ": " ++ show r
    set :: MVar.MVar a -> a -> IO ()
    set v a = MVar.modifyMVar_ v (\_ -> pure a)

withS :: MVar.MVar (S v) -> (S v -> i -> Noted IO o) -> i -> Noted IO o
withS s f i = Note.lift (MVar.readMVar s) >>= \s -> f s i

withS2 :: MVar.MVar (S v) -> (S v -> i -> i2 -> Noted IO o) -> i -> i2 -> Noted IO o
withS2 s f i i2 = Note.lift (MVar.readMVar s) >>= \s -> f s i i2

data S v =
  S { terms' :: Map Hash (Term v)
--  , typeDeclarations' :: Map Hash (Type v)
    , typeOfTerm' :: Map Reference (Type v)
    , metadata' :: Map Reference (Metadata v Reference) }

