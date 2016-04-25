{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Unison.Runtime.ExtraBuiltins where

import System.Random
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import Unison.Node.Builtin
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Text as Text
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Note as Note
import qualified Unison.Reference as R
import qualified Unison.Runtime.KeyValueStore as KVS
import qualified Unison.Runtime.ResourcePool as RP
import qualified Unison.Term as Term
import qualified Unison.Type as Type



--store k v = Type.app (Type.app (Type.ref (R.Builtin "Store")) k) v
store k v = Type.ref (R.Builtin "Store") `Type.app` k `Type.app` v

makeRandomHash :: RandomGen r => MVar.MVar r -> IO Hash
makeRandomHash genVar = do
  gen <- MVar.readMVar genVar
  let (hash, newGen) = random gen
  MVar.swapMVar genVar newGen
  pure hash

makeAPI :: IO (WHNFEval -> [Builtin])
makeAPI = do
  stdGen <- getStdGen
  genVar <- MVar.newMVar stdGen
  resourcePool <- RP.make 3 10 KVS.load KVS.close
  let nextHash = makeRandomHash genVar
  {--
  stringStore <- MVar.newMVar Nothing
  let getAndSetStore = do
      storeState <- MVar.readMVar stringStore
      case storeState of
        Nothing -> do
          ss <- openLocalState (KeyValue Map.empty)
          MVar.putMVar stringStore $ Just ss
          pure ss
        Just ss -> pure ss
  --}
  -- TODO - change from String/String store to Term/Term store
  pure (\whnf -> map (\(r, o, t, m) -> Builtin r o t m)
     [ let r = R.Builtin "KeyValue.empty"
           op [] = Note.lift $ do
             hash <- nextHash
             pure . Term.lit $ Term.KeyValueStore hash
           op _ = fail "KeyValue.empty unpossible"
           type' = Type.forall' ["k", "v"] $ remote (store (Type.v' "k") (Type.v' "v"))
       in (r, Just (I.Primop 0 op), type', prefix "stringStore")
     , let r = R.Builtin "KeyValue.lookup"
           op [indexToken, key] = inject g indexToken key where
             inject g indexToken key = do
               i <- whnf indexToken
               k <- whnf key
               g i k
             -- since there's only one index, indexToken is ignored right now
             g (Term.Store' h) (Term.Text' t) = do
               val <- Note.lift $ do
                 (db, _) <- RP.acquire resourcePool h
                 {-- TODO fix
                 result <- KVS.lookup (Text.unpack t) db
                 pure $ maybe "" Text.pack result -- TODO introduce maybe type to unison
--}
                 undefined
               pure . Term.lit $ Term.Text val
             g s k = pure $ Term.ref r `Term.app` s `Term.app` k
           op _ = fail "KeyValue.lookup unpossible"
           type' = Type.forall' ["k", "v"]
             $ Type.v' "k" --> store (Type.v' "k") (Type.v' "v") --> remote (Type.v' "v")
       in (r, Just (I.Primop 2 op), type', prefix "lookupKey")
     , let r = R.Builtin "KeyValue.insert"
           op [k, v, store] = inject g k v store where
             inject g k v store = do
               k' <- whnf k
               v' <- whnf v
               s <- whnf store
               g k' v' s
             g (Term.Text' k) (Term.Text' v) (Term.Store' h) = do
               Note.lift $ do
                 (db, _) <- RP.acquire resourcePool h
                 -- TODO fix
                 --KVS.insert (Text.unpack k) (Text.unpack v) db
                 undefined
               pure unitRef
             g k v store = pure $ Term.ref r `Term.app` k `Term.app` v `Term.app` store
           op _ = fail "KeyValue.insert unpossible"
       in (r, Just (I.Primop 3 op), str --> str --> store str str --> remote unitT, prefix "insertKey")
     ])


{--


data Index k v

index : Remote! (Index k v)
lookup : k -> Index k v -> Remote! (Maybe v)
delete : k -> Index k v -> Remote! ()
-- keys : Index k v -> Stream Remote! k

--}
