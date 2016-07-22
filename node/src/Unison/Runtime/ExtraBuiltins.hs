{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Unison.Runtime.ExtraBuiltins where

import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Unison.BlockStore (Series(..), BlockStore)
import Unison.Node.Builtin
import Unison.Parsers (unsafeParseType)
import Unison.Type (Type)
import qualified Unison.Cryptography as C
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Note as Note
import qualified Unison.Reference as R
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

makeAPI :: Eq a => BlockStore a -> C.Cryptography k syk sk skp s h ByteString
  -> IO (WHNFEval -> [Builtin])
makeAPI blockStore crypto = do
  let nextID = do
        cp <- C.randomBytes crypto 64
        ud <- C.randomBytes crypto 64
        pure (Series cp, Series ud)
  resourcePool <- RP.make 3 10 (KVS.loadEncrypted blockStore crypto) KVS.flush
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
                 result <- atomically $ KVS.lookup (SAH.hash' k) db
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
                 atomically
                   (KVS.insert (SAH.hash' k) (SAH.serializeTerm k, SAH.serializeTerm v) db)
                   >>= atomically
               pure unitRef
             g k v store = pure $ Term.ref r `Term.app` k `Term.app` v `Term.app` store
           op _ = fail "Index.insert unpossible"
       in (r, Just (I.Primop 3 op), unsafeParseType "String -> String -> Store String String -> Remote Unit", prefix "insert")
     ])

