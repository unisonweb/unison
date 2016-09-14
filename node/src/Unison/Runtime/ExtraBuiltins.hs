{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Unison.Runtime.ExtraBuiltins where

import Control.Exception (finally)
import Control.Concurrent.STM (atomically)
import Data.ByteString (ByteString)
import Unison.BlockStore (Series(..), BlockStore)
import Unison.Node.Builtin
import Unison.Parsers (unsafeParseType)
import Unison.Type (Type)
import Unison.Util.Logger (Logger)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Unison.Cryptography as C
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Hash as Hash
import qualified Unison.Note as Note
import qualified Unison.Reference as R
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Html as Html
import qualified Unison.Runtime.Http as Http
import qualified Unison.Runtime.Index as Index
import qualified Unison.Runtime.ResourcePool as RP
import qualified Unison.SerializationAndHashing as SAH
import qualified Unison.Term as Term
import qualified Unison.Type as Type
-- import qualified Unison.Util.Logger as L

indexT :: Ord v => Type v -> Type v -> Type v
indexT k v = Type.ref (R.Builtin "Index") `Type.app` k `Type.app` v

index :: Remote.Node -> Term.Term V -> Term.Term V
index node h = Term.ref (R.Builtin "Index") `Term.apps` [Term.node node, h]

linkT :: Ord v => Type v
linkT = Type.ref (R.Builtin "Html.Link")

link :: Term.Term V -> Term.Term V -> Term.Term V
link href description = Term.ref (R.Builtin "Html.Link") `Term.app` href `Term.app` description

linkToTerm :: Html.Link -> Term.Term V
linkToTerm (Html.Link href description) = link (Term.lit $ Term.Text href)
  (Term.lit $ Term.Text description)

pattern Index' node s <-
  Term.App' (Term.App' (Term.Ref' (R.Builtin "Index")) (Term.Distributed' (Term.Node node)))
            (Term.Text' s)

pattern Link' href description <-
  Term.App' (Term.App' (Term.Ref' (R.Builtin "Html.Link"))
                       (Term.Text' href))
  (Term.Text' description)

-- TODO rewrite builtins not to use unsafe code
make :: Eq a
     => Logger -> BlockStore a -> C.Cryptography k syk sk skp s h ByteString
     -> IO (WHNFEval -> [Builtin])
make _ blockStore crypto = do
  let nextID = do
        cp <- C.randomBytes crypto 64
        ud <- C.randomBytes crypto 64
        pure (Series cp, Series ud)
  resourcePool <- RP.make 3 10 (Index.loadEncrypted blockStore crypto) Index.flush
  pure (\whnf -> map (\(r, o, t, m) -> Builtin r o t m)
     [ -- Index
       let r = R.Builtin "Index.empty#"
           op [self] = do
             ident <- Note.lift nextID
             Term.Distributed' (Term.Node self) <- whnf self
             pure . index self . Term.lit . Term.Text . Index.idToText $ ident
           op _ = fail "Index.empty# unpossible"
           type' = unsafeParseType "forall k v . Node -> Index k v"
       in (r, Just (I.Primop 1 op), type', prefix "Index.empty#")
     , let r = R.Builtin "Index.keys#"
           op [indexToken] = do
             Term.Text' h <- whnf indexToken
             Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool . Index.textToId $ h
               flip finally cleanup $ do
                 keyBytes <- atomically $ Index.keys db
                 case traverse SAH.deserializeTermFromBytes keyBytes of
                   Left err -> fail ("Index.keys# could not deserialize: " ++ err)
                   Right terms -> pure $ Term.vector terms
           op _ = fail "Index.keys# unpossible"
           type' = unsafeParseType "forall k . Text -> Vector k"
       in (r, Just (I.Primop 1 op), type', prefix "Index.keys#")
     , let r = R.Builtin "Index.1st-key#"
           op [indexToken] = do
             Term.Text' h <- whnf indexToken
             Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool . Index.textToId $ h
               flip finally cleanup $ do
                 keyBytes <- atomically $ Index.keys db
                 case keyBytes of
                   [] -> pure none
                   (keyBytes:_) -> case SAH.deserializeTermFromBytes keyBytes of
                     Left err -> fail ("Index.1st-key# could not deserialize: " ++ err)
                     Right terms -> pure $ some terms
           op _ = fail "Index.1st-key# unpossible"
           type' = unsafeParseType "forall k . Text -> Optional k"
       in (r, Just (I.Primop 1 op), type', prefix "Index.1st-key#")
     , let r = R.Builtin "Index.increment#"
           op [key, indexToken] = do
             key <- whnf key
             Term.Text' h <- whnf indexToken
             Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool . Index.textToId $ h
               flip finally cleanup $ do
                 entry <- atomically $ Index.lookupGT (SAH.hash' key) db
                 case entry of
                   Nothing -> pure none
                   Just (_, (keyBytes, _)) -> case SAH.deserializeTermFromBytes keyBytes of
                     Left err -> fail ("Index.increment# could not deserialize: " ++ err)
                     Right term -> pure $ some term
           op _ = fail "Index.increment# unpossible"
           type' = unsafeParseType "forall k . k -> Text -> Optional k"
       in (r, Just (I.Primop 2 op), type', prefix "Index.increment#")
     , let r = R.Builtin "Index.representation#"
           op [index] = do
             Index' node tok <- whnf index
             pure $ pair' (Term.node node) (Term.text tok)
           op _ = fail "Index.representation# unpossible"
           type' = unsafeParseType "forall k v . Index k v -> (Node, Text)"
       in (r, Just (I.Primop 1 op), type', prefix "Index.representation#")
     , let r = R.Builtin "Index.lookup#"
           op [key, indexToken] = inject g indexToken key where
             inject g indexToken key = do
               i <- whnf indexToken
               k <- whnf key
               g i k
             g (Term.Text' h) k = do
               val <- Note.lift $ do
                 (db, cleanup) <- RP.acquire resourcePool . Index.textToId $ h
                 flip finally cleanup $ do
                   result <- atomically $ Index.lookup (SAH.hash' k) db
                   case result >>= (pure . SAH.deserializeTermFromBytes . snd) of
                     Just (Left s) -> fail ("Index.lookup# could not deserialize: " ++ s)
                     Just (Right t) -> pure $ some t
                     Nothing -> pure none
               pure val
             g s k = pure $ Term.ref r `Term.app` s `Term.app` k
           op _ = fail "Index.lookup# unpossible"
           type' = unsafeParseType "forall k v . k -> Text -> Optional v"
       in (r, Just (I.Primop 2 op), type', prefix "Index.lookup#")
     , let r = R.Builtin "Index.delete#"
           op [key, indexToken] = do
             Term.Text' indexToken <- whnf indexToken
             key <- whnf key
             (db, cleanup) <- Note.lift . RP.acquire resourcePool . Index.textToId $ indexToken
             Note.lift . flip finally cleanup $ do
               _ <- atomically $ Index.delete (SAH.hash' key) db
               pure unitRef
           op _ = fail "Index.delete# unpossible"
           type' = unsafeParseType "forall k . k -> Text -> Unit"
       in (r, Just (I.Primop 2 op), type', prefix "Index.delete#")
     , let r = R.Builtin "Index.insert#"
           op [k, v, index] = do
             k <- whnf k
             v <- whnf v
             Term.Text' indexToken <- whnf index
             Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool . Index.textToId $ indexToken
               flip finally cleanup $ atomically
                 (Index.insert (SAH.hash' k) (SAH.serializeTerm k, SAH.serializeTerm v) db)
                 >>= atomically
               pure unitRef
           op _ = fail "Index.insert# unpossible"
           type' = unsafeParseType "forall k v . k -> v -> Text -> Unit"
       in (r, Just (I.Primop 3 op), type', prefix "Index.insert#")

     -- Html
     , let r = R.Builtin "Html.get-links"
           op [html] = do
             html' <- whnf html
             pure $ case html' of
               Term.Text' h -> Term.vector' . Vector.fromList . map linkToTerm
                 $ Html.getLinks h
               x -> Term.ref r `Term.app` x
           op _ = fail "Html.get-links unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Text -> Vector Html.Link", prefix "Html.get-links")
     , let r = R.Builtin "Html.plain-text"
           op [html] = do
             html' <- whnf html
             pure $ case html' of
               Term.Text' h -> Term.text $ Html.toPlainText h
               x -> Term.ref r `Term.app` x
           op _ = fail "Html.plain-text unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Text -> Text", prefix "Html.plain-text")
     , let r = R.Builtin "Html.get-href"
           op [link] = do
             link' <- whnf link
             pure $ case link' of
               Link' href _ -> Term.lit (Term.Text href)
               x -> Term.ref r `Term.app` x
           op _ = fail "Html.get-href unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Html.Link -> Text", prefix "Html.get-href")
     , let r = R.Builtin "Html.get-description"
           op [link] = do
             link' <- whnf link
             pure $ case link' of
               Link' _ d -> Term.lit (Term.Text d)
               x -> Term.ref r `Term.app` x
           op _ = fail "Html.get-description unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Html.Link -> Text", prefix "Html.get-description")

     -- Http
     , let r = R.Builtin "Http.get-url#"
           op [url] = do
             url <- whnf url
             case url of
               Term.Text' url -> Note.lift $ do
                   result <- Http.get $ Text.unpack url
                   pure $ case result of
                     Right x -> right $ Term.text x
                     Left x -> left . Term.text . Text.pack $ show x
               x -> pure $ Term.ref r `Term.app` x
           op _ = fail "Http.get-url# unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Text -> Either Text Text", prefix "Http.get-url#")

     -- Hashing
     -- add erase, comparison functions
     , let r = R.Builtin "hash#"
           op [e] = do
             e <- whnf e
             let h = Hash.base64 . Hash.fromBytes . SAH.hash' $ e
             pure $ Term.builtin "Hash" `Term.app` (Term.text h)
           op _ = fail "hash"
           t = "forall a . a -> Hash a"
       in (r, Just (I.Primop 1 op), unsafeParseType t, prefix "hash#")
     , let r = R.Builtin "Hash.base64"
           op [e] = do
             Term.App' _ (Term.Text' r1) <- whnf e
             pure (Term.text r1)
           op _ = fail "Hash.base64"
           t = "forall a . Hash a -> Text"
       in (r, Just (I.Primop 1 op), unsafeParseType t, prefix "Hash.base64")
     , let r = R.Builtin "Hash.erase"
           op [e] = pure e
           op _ = fail "Hash.erase"
           t = "forall a . Hash a -> Hash Unit"
       in (r, Just (I.Primop 1 op), unsafeParseType t, prefix "Hash.erase")
     , let r = R.Builtin "Hash.=="
           op [h1,h2] = do
             Term.App' _ (Term.Text' r1) <- whnf h1
             Term.App' _ (Term.Text' r2) <- whnf h2
             pure $ if r1 == r2 then true else false
           op _ = fail "Hash.=="
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.==")
     , let r = R.Builtin "Hash.<"
           op [h1,h2] = do
             Term.App' _ (Term.Text' r1) <- whnf h1
             Term.App' _ (Term.Text' r2) <- whnf h2
             pure $ if r1 < r2 then true else false
           op _ = fail "Hash.<"
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.<")
     , let r = R.Builtin "Hash.<="
           op [h1,h2] = do
             Term.App' _ (Term.Text' r1) <- whnf h1
             Term.App' _ (Term.Text' r2) <- whnf h2
             pure $ if r1 <= r2 then true else false
           op _ = fail "Hash.<="
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.<=")
     , let r = R.Builtin "Hash.>"
           op [h1,h2] = do
             Term.App' _ (Term.Text' r1) <- whnf h1
             Term.App' _ (Term.Text' r2) <- whnf h2
             pure $ if r1 > r2 then true else false
           op _ = fail "Hash.>"
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.>")
     , let r = R.Builtin "Hash.>="
           op [h1,h2] = do
             Term.App' _ (Term.Text' r1) <- whnf h1
             Term.App' _ (Term.Text' r2) <- whnf h2
             pure $ if r1 >= r2 then true else false
           op _ = fail "Hash.>="
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.>=")
     , let r = R.Builtin "Hash.Order"
       in (r, Nothing, unsafeParseType "∀ a . Order (Hash a)", prefix "Hash.Order")
     ])

hashCompareTyp :: Type V
hashCompareTyp = unsafeParseType "∀ a . Hash a -> Hash a -> Boolean"
