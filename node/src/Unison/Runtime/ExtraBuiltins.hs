{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Unison.Runtime.ExtraBuiltins where

import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial, serialize)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Unison.BlockStore (Series(..), BlockStore)
import Unison.Builtin
import Unison.Parsers (unsafeParseType)
import Unison.Type (Type)
import Unison.Util.Logger (Logger)
import Unison.Var (Var)
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.URI as URI
import qualified Unison.Cryptography as C
import qualified Unison.Hash as Hash
import qualified Unison.Interpreter as I
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

index :: Var v => Remote.Node -> Term.Term v -> Term.Term v
index node h = Term.ref (R.Builtin "Index") `Term.apps` [Term.node node, h]

linkT :: Ord v => Type v
linkT = Type.ref (R.Builtin "Html.Link")

link :: Var v => Term.Term v -> Term.Term v -> Term.Term v
link href description = Term.ref (R.Builtin "Html.Link") `Term.app` href `Term.app` description

linkToTerm :: Var v => Html.Link -> Term.Term v
linkToTerm (Html.Link href description) = link (Term.lit $ Term.Text href)
  (Term.lit $ Term.Text description)

pattern Index' node s <-
  Term.App' (Term.App' (Term.Ref' (R.Builtin "Index")) (Term.Distributed' (Term.Node node)))
            (Term.Text' s)

pattern Link' href description <-
  Term.App' (Term.App' (Term.Ref' (R.Builtin "Html.Link"))
                       (Term.Text' href))
  (Term.Text' description)

deserializeTermPair :: (Serial v, Var v) =>
  ByteString -> Either String (Term.Term v, Term.Term v)
deserializeTermPair = Get.runGetS $ (,) <$> SAH.deserializeTerm <*> SAH.deserializeTerm


-- TODO rewrite builtins not to use unsafe code
make :: (Serial v, Var v, Eq a)
     => Logger -> BlockStore a -> C.Cryptography k syk sk skp s h ByteString
     -> IO [Builtin v]
make _ blockStore crypto = do
  let nextID = decodeUtf8 . Base64.encode <$> C.randomBytes crypto 64
      acquire = Index.load blockStore crypto . Series . Base64.decodeLenient . encodeUtf8
  resourcePool <- RP.make 3 10 acquire (const $ pure ())
  pure (map (\(r, o, t, m) -> Builtin r o t m)
     [ -- Index
       let r = R.Builtin "Index.empty#"
           op [Term.Distributed' (Term.Node self)] = do
             ident <- Note.lift nextID
             pure . index self . Term.lit . Term.Text $ ident
           op _ = fail "Index.empty# unpossible"
           type' = unsafeParseType "forall k v . Node -> Index k v"
       in (r, Just (I.Primop 1 op), type', prefix "Index.empty#")
     , let r = R.Builtin "Index.keys#"
           op [Term.Text' h] = Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool h
               flip finally cleanup $ do
                 keyBytes <- Index.keys db
                 case traverse SAH.deserializeTermFromBytes keyBytes of
                   Left err -> fail ("Index.keys# could not deserialize: " ++ err)
                   Right terms -> pure $ Term.vector terms
           op _ = fail "Index.keys# unpossible"
           type' = unsafeParseType "forall k . Text -> Vector k"
       in (r, Just (I.Primop 1 op), type', prefix "Index.keys#")
     , let r = R.Builtin "Index.1st-key#"
           op [Term.Text' h] = Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool h
               flip finally cleanup $ do
                 keyBytes <- Index.keys db
                 case keyBytes of
                   [] -> pure none
                   (keyBytes:_) -> case SAH.deserializeTermFromBytes keyBytes of
                     Left err -> fail ("Index.1st-key# could not deserialize: " ++ err)
                     Right terms -> pure $ some terms
           op _ = fail "Index.1st-key# unpossible"
           type' = unsafeParseType "forall k . Text -> Optional k"
       in (r, Just (I.Primop 1 op), type', prefix "Index.1st-key#")
     , let r = R.Builtin "Index.increment#"
           op [key, Term.Text' h] = Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool h
               flip finally cleanup $ do
                 entry <- Index.lookupGT db (SAH.hash' key)
                 case entry of
                   Nothing -> pure none
                   Just (_, keyValue) ->
                     case deserializeTermPair keyValue of
                       Left err -> fail ("Index.increment# could not deserialize: " ++ err)
                       Right (term,_) -> pure $ some term
           op _ = fail "Index.increment# unpossible"
           type' = unsafeParseType "forall k . k -> Text -> Optional k"
       in (r, Just (I.Primop 2 op), type', prefix "Index.increment#")
     , let r = R.Builtin "Index.representation#"
           op [Index' node tok] = pure $ pair' (Term.node node) (Term.text tok)
           op _ = fail "Index.representation# unpossible"
           type' = unsafeParseType "forall k v . Index k v -> (Node, Text)"
       in (r, Just (I.Primop 1 op), type', prefix "Index.representation#")
     , let r = R.Builtin "Index.lookup#"
           op [k, Term.Text' h] = Note.lift $ do
                 (db, cleanup) <- RP.acquire resourcePool h
                 flip finally cleanup $ do
                   result <- Index.lookup db (SAH.hash' k)
                   case result >>= (pure . deserializeTermPair) of
                     Just (Left s) -> fail ("Index.lookup# could not deserialize: " ++ s)
                     Just (Right (_,t)) -> pure $ some t
                     Nothing -> pure none
           op _ = fail "Index.lookup# unpossible"
           type' = unsafeParseType "forall k v . k -> Text -> Optional v"
       in (r, Just (I.Primop 2 op), type', prefix "Index.lookup#")
     , let r = R.Builtin "Index.delete#"
           op [key, Term.Text' indexToken] = do
             (db, cleanup) <- Note.lift . RP.acquire resourcePool $ indexToken
             Note.lift . flip finally cleanup $ do
               _ <- Index.delete db (SAH.hash' key)
               pure unitRef
           op _ = fail "Index.delete# unpossible"
           type' = unsafeParseType "forall k . k -> Text -> Unit"
       in (r, Just (I.Primop 2 op), type', prefix "Index.delete#")
     , let r = R.Builtin "Index.insert#"
           op [k, v, Term.Text' indexToken] = Note.lift $ do
               (db, cleanup) <- RP.acquire resourcePool indexToken
               let value = Put.runPutS $ serialize (k,v)
               flip finally cleanup $
                 Index.insert db (SAH.hash' k) value
               pure unitRef
           op _ = fail "Index.insert# unpossible"
           type' = unsafeParseType "forall k v . k -> v -> Text -> Unit"
       in (r, Just (I.Primop 3 op), type', prefix "Index.insert#")

     -- Html
     , let r = R.Builtin "Html.get-links"
           op [Term.Text' html] = pure $
             Term.vector' . Vector.fromList . map linkToTerm $ Html.getLinks html
           op _ = fail "Html.get-links unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Text -> Vector Html.Link", prefix "Html.get-links")
     , let r = R.Builtin "Html.plain-text"
           op [Term.Text' html] = pure . Term.text $ Html.toPlainText html
           op _ = fail "Html.plain-text unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Text -> Text", prefix "Html.plain-text")
     , let r = R.Builtin "Html.get-href"
           op [Link' href _] = pure $ Term.text href
           op _ = fail "Html.get-href unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Html.Link -> Text", prefix "Html.get-href")
     , let r = R.Builtin "Html.get-description"
           op [Link' _ d] = pure $ Term.text d
           op _ = fail "Html.get-description unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Html.Link -> Text", prefix "Html.get-description")

     -- Http
     , let r = R.Builtin "Http.get-url#"
           op [Term.Text' url] = Note.lift $ do
             result <- Http.get $ Text.unpack url
             pure $ case result of
               Right x -> right $ Term.text x
               Left x -> left . Term.text . Text.pack $ show x
           op _ = fail "Http.get-url# unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Text -> Either Text Text", prefix "Http.get-url#")

    , let r = R.Builtin "Uri.parse-scheme"
          op [Term.Text' url] = pure $ case URI.parseURI (Text.unpack url) of
            Nothing -> none
            Just uri -> some . Term.text . Text.pack $ URI.uriScheme uri
          op _ = error "Uri.parse-scheme unpossible"
          typ = "Text -> Optional Text"
      in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "Uri.parse-scheme")

    , let r = R.Builtin "Uri.parse-authority"
          op [Term.Text' url] = pure $
            case URI.parseURI (Text.unpack url) >>= URI.uriAuthority of
              Nothing -> none
              Just auth -> some . Term.text . Text.pack $
                URI.uriUserInfo auth ++ URI.uriRegName auth ++ URI.uriPort auth
          op _ = error "Uri.parse-authority unpossible"
          typ = "Text -> Optional Text"
      in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "Uri.parse-authority")

     -- Hashing
     , let r = R.Builtin "hash#"
           op [e] = let h = Hash.base64 . Hash.fromBytes . SAH.hash' $ e
                    in pure $ Term.builtin "Hash" `Term.app` (Term.text h)
           op _ = fail "hash"
           t = "forall a . a -> Hash a"
       in (r, Just (I.Primop 1 op), unsafeParseType t, prefix "hash#")
     , let r = R.Builtin "Hash.base64"
           op [Term.App' _ (Term.Text' r1)] = pure (Term.text r1)
           op _ = fail "Hash.base64"
           t = "forall a . Hash a -> Text"
       in (r, Just (I.Primop 1 op), unsafeParseType t, prefix "Hash.base64")
     , let r = R.Builtin "Hash.erase"
           op [e] = pure e
           op _ = fail "Hash.erase"
           t = "forall a . Hash a -> Hash Unit"
       in (r, Just (I.Primop 1 op), unsafeParseType t, prefix "Hash.erase")
     , let r = R.Builtin "Hash.=="
           op [Term.App' _ (Term.Text' r1), Term.App' _ (Term.Text' r2)] =
             pure $ if r1 == r2 then true else false
           op _ = fail "Hash.=="
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.==")
     , let r = R.Builtin "Hash.<"
           op [Term.App' _ (Term.Text' r1), Term.App' _ (Term.Text' r2)] =
             pure $ if r1 < r2 then true else false
           op _ = fail "Hash.<"
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.<")
     , let r = R.Builtin "Hash.<="
           op [Term.App' _ (Term.Text' r1), Term.App' _ (Term.Text' r2)] =
             pure $ if r1 <= r2 then true else false
           op _ = fail "Hash.<="
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.<=")
     , let r = R.Builtin "Hash.>"
           op [Term.App' _ (Term.Text' r1), Term.App' _ (Term.Text' r2)] =
             pure $ if r1 > r2 then true else false
           op _ = fail "Hash.>"
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.>")
     , let r = R.Builtin "Hash.>="
           op [Term.App' _ (Term.Text' r1), Term.App' _ (Term.Text' r2)] =
             pure $ if r1 >= r2 then true else false
           op _ = fail "Hash.>="
       in (r, Just (I.Primop 2 op), hashCompareTyp, prefix "Hash.>=")
     , let r = R.Builtin "Hash.Order"
       in (r, Nothing, unsafeParseType "∀ a . Order (Hash a)", prefix "Hash.Order")
     ])

hashCompareTyp :: Var v => Type v
hashCompareTyp = unsafeParseType "∀ a . Hash a -> Hash a -> Boolean"
