{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Unison.Runtime.ExtraBuiltins where

import Data.ByteString (ByteString)
import System.Random
import Unison.BlockStore (Series(..))
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import Unison.Node.Builtin
import Unison.Parsers (unsafeParseType)
import Unison.Type (Type)
import qualified Control.Concurrent.MVar as MVar
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Hash as Hash
import qualified Unison.Note as Note
import qualified Unison.Reference as R
import qualified Unison.Runtime.Address as Address
import qualified Unison.Runtime.Index as KVS
import qualified Unison.Runtime.ResourcePool as RP
import qualified Unison.SerializationAndHashing as SAH
import qualified Unison.Term as Term
import qualified Unison.Type as Type

storeT :: Ord v => Type v -> Type v -> Type v
storeT k v = Type.ref (R.Builtin "Index") `Type.app` k `Type.app` v

store :: Term.Term V -> Term.Term V
store h = Term.ref (R.Builtin "Index") `Term.app` h

pattern Store' s <- Term.App'
                    (Term.Ref' (R.Builtin "Index"))
                    (Term.Lit' (Term.Text s))

makeRandomHash :: RandomGen r => MVar.MVar r -> IO Hash
makeRandomHash genVar = do
  gen <- MVar.readMVar genVar
  let (hash, newGen) = random gen
  _ <- MVar.swapMVar genVar newGen
  pure hash

makeRandomBS :: RandomGen r => MVar.MVar r -> IO ByteString
makeRandomBS genVar = Hash.toBytes <$> makeRandomHash genVar

makeRandomSeries :: RandomGen r => MVar.MVar r -> IO Series
makeRandomSeries genVar = Series <$> makeRandomBS genVar

makeRandomAddress :: RandomGen r => MVar.MVar r -> IO Address.Address
makeRandomAddress genVar = Address.Address <$> makeRandomBS genVar

makeRandomIdentifier :: RandomGen r => MVar.MVar r -> IO (Series, Series)
makeRandomIdentifier genVar = do
  cp <- makeRandomSeries genVar
  ud <- makeRandomSeries genVar
  pure (cp, ud)


makeAPI :: IO (WHNFEval -> [Builtin])
makeAPI = do
  stdGen <- getStdGen
  genVar <- MVar.newMVar stdGen
  let nextAddress = makeRandomAddress genVar
      nextID = makeRandomIdentifier genVar
  blockStore <- FBS.make' nextAddress "Index"
  resourcePool <- RP.make 3 10 (KVS.load blockStore) (const (pure ()))
  pure (\whnf -> map (\(r, o, t, m) -> Builtin r o t m)
     [ let r = R.Builtin "Index.empty"
           op [] = Note.lift $ do
             ident <- nextID
             pure . store . Term.lit . Term.Text . KVS.idToText $ ident
           op _ = fail "Index.empty unpossible"
           type' = unsafeParseType "forall k v. Remote (Store k v)"
       in (r, Just (I.Primop 0 op), type', prefix "empty")
     , let r = R.Builtin "Index.lookup"
           op [indexToken, key] = inject g indexToken key where
             inject g indexToken key = do
               i <- whnf indexToken
               k <- whnf key
               g i k
             g (Store' h) k = do
               val <- Note.lift $ do
                 (db, _) <- RP.acquire resourcePool . KVS.textToId $ h
                 result <- KVS.lookup (SAH.hash' k) db
                 case result >>= (pure . SAH.deserializeTermFromBytes . snd) of
                   Just (Left s) -> fail ("Index.lookup could not deserialize: " ++ s)
                   Just (Right t) -> pure $ some t
                   Nothing -> pure none
               pure val
             g s k = pure $ Term.ref r `Term.app` s `Term.app` k
           op _ = fail "Index.lookup unpossible"
           type' = unsafeParseType "forall k v. k -> Store k v -> Remote (Option v)"
       in (r, Just (I.Primop 2 op), type', prefix "lookup")
     , let r = R.Builtin "Index.insert"
           op [k, v, store] = inject g k v store where
             inject g k v store = do
               k' <- whnf k
               v' <- whnf v
               s <- whnf store
               g k' v' s
             g k v (Store' h) = do
               Note.lift $ do
                 (db, _) <- RP.acquire resourcePool . KVS.textToId $ h
                 KVS.insert (SAH.hash' k) (SAH.serializeTerm k, SAH.serializeTerm v) db
               pure unitRef
             g k v store = pure $ Term.ref r `Term.app` k `Term.app` v `Term.app` store
           op _ = fail "Index.insert unpossible"
       in (r, Just (I.Primop 3 op), unsafeParseType "String -> String -> Store String String -> Remote Unit", prefix "insert")
     ])

