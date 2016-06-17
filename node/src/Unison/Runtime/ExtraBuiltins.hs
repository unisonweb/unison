{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Unison.Runtime.ExtraBuiltins where

import System.Random
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import Unison.Node.Builtin
import Unison.Parsers (unsafeParseType)
import Unison.Type (Type)
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Note as Note
import qualified Unison.Reference as R
import qualified Unison.Runtime.KeyValueStore as KVS
import qualified Unison.Runtime.ResourcePool as RP
import qualified Unison.SerializationAndHashing as SAH
import qualified Unison.Term as Term
import qualified Unison.Type as Type

store :: Ord v => Type v -> Type v -> Type v
store k v = Type.ref (R.Builtin "Store") `Type.app` k `Type.app` v

makeRandomHash :: RandomGen r => MVar.MVar r -> IO Hash
makeRandomHash genVar = do
  gen <- MVar.readMVar genVar
  let (hash, newGen) = random gen
  _ <- MVar.swapMVar genVar newGen
  pure hash

makeAPI :: IO (WHNFEval -> [Builtin])
makeAPI = do
  stdGen <- getStdGen
  genVar <- MVar.newMVar stdGen
  resourcePool <- RP.make 3 10 KVS.load KVS.close
  let nextHash = makeRandomHash genVar
  -- TODO - change from String/String store to Term/Term store
  pure (\whnf -> map (\(r, o, t, m) -> Builtin r o t m)
     [ let r = R.Builtin "KeyValue.empty"
           op [] = Note.lift $ do
             hash <- nextHash
             pure . Term.lit $ Term.KeyValueStore hash
           op _ = fail "KeyValue.empty unpossible"
           type' = unsafeParseType "forall k v. Remote (Store k v)"
       in (r, Just (I.Primop 0 op), type', prefix "stringStore")
     , let r = R.Builtin "KeyValue.lookup"
           op [indexToken, key] = inject g indexToken key where
             inject g indexToken key = do
               i <- whnf indexToken
               k <- whnf key
               g i k
             g (Term.Store' h) k = do
               val <- Note.lift $ do
                 (db, _) <- RP.acquire resourcePool h
                 result <- KVS.lookup (SAH.hash' k) db
                 case result >>= (pure . SAH.deserializeTermFromBytes) of
                   Just (Left s) -> fail ("KeyValue.lookup could not deserialize: " ++ s)
                   Just (Right t) -> pure $ some t
                   Nothing -> pure none
               pure val
             g s k = pure $ Term.ref r `Term.app` s `Term.app` k
           op _ = fail "KeyValue.lookup unpossible"
           type' = unsafeParseType "forall k v. k -> Store k v -> Remote (Option v)"
       in (r, Just (I.Primop 2 op), type', prefix "lookupKey")
     , let r = R.Builtin "KeyValue.insert"
           op [k, v, store] = inject g k v store where
             inject g k v store = do
               k' <- whnf k
               v' <- whnf v
               s <- whnf store
               g k' v' s
             g k v (Term.Store' h) = do
               Note.lift $ do
                 (db, _) <- RP.acquire resourcePool h
                 KVS.insert (SAH.hash' k) (SAH.serializeTerm v) db
               pure unitRef
             g k v store = pure $ Term.ref r `Term.app` k `Term.app` v `Term.app` store
           op _ = fail "KeyValue.insert unpossible"
       in (r, Just (I.Primop 3 op), unsafeParseType "String -> String -> Store String String -> Remote Unit", prefix "insertKey")
     ])


{--


data Index k v

index : Remote! (Index k v)
lookup : k -> Index k v -> Remote! (Maybe v)
delete : k -> Index k v -> Remote! ()
-- keys : Index k v -> Stream Remote! k

--}
