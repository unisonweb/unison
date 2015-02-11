{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Node.Server where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.Aeson.Parser as JP
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Map as M
import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Type as T
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Reference (Reference)
import Unison.Node (Node)
import qualified Unison.Node as N
import Unison.Note (Noted, unnote)
import qualified Web.Scotty as S
import Web.Scotty (ActionM)

runN :: Noted IO a -> ActionM a
runN n = liftIO (unnote n) >>= go
  where go (Left e) = S.raise (TL.pack (show e))
        go (Right a) = pure a

jsonParam :: J.FromJSON a => TL.Text -> ActionM a
jsonParam paramName = S.param paramName >>= \paramValue ->
  let val = Atto.eitherResult
          . Atto.parse JP.value
          . LBS.toStrict
          . TLE.encodeUtf8 $ paramValue
  in case val >>= JT.parseEither J.parseJSON of
    Left err -> S.raise (TL.pack err)
    Right a -> pure a

originPolicy :: ActionM ()
originPolicy =
  S.addHeader "Access-Control-Allow-Origin" "*"

originOptions :: ActionM ()
originOptions = do
  S.addHeader "Access-Control-Allow-Origin" "*"
  S.addHeader "Access-Control-Allow-Methods" "GET, POST"
  S.addHeader "Access-Control-Allow-Headers" "Content-Type"

route :: ActionM () -> ActionM ()
route action = do
  originPolicy
  body <- S.body
  liftIO (putStrLn ("request body\n" ++ show body))
  action

server :: Int -> Node IO Reference T.Type E.Term -> IO ()
server port node = S.scotty port $ do
  S.addroute OPTIONS (S.regex ".*") $ originOptions
  S.get "/admissible-type-of" . route $ do
    (h, path) <- S.jsonData
    t <- runN $ N.admissibleTypeOf node h path
    S.json t
  S.post "/create-term" $ do
    (e, md) <- S.jsonData
    k <- runN $ N.createTerm node e md
    S.json k
  S.post "/create-type" $ do
    (t, md) <- S.jsonData
    k <- runN $ N.createType node t md
    S.json k
  S.get "/dependencies" $ do
    (limit, h) <- S.jsonData
    k <- runN $ N.dependencies node limit h
    S.json k
  S.get "/dependents" $ do
    (limit, h) <- S.jsonData
    k <- runN $ N.dependents node limit h
    S.json k
  S.get "/edit-term" $ do -- this merely computes the new term, hence a GET
    (loc, a, e) <- S.jsonData
    e <- runN $ N.editTerm node loc a e
    S.json e -- we might follow this up with a 'create-term', which is a POST
  S.get "/edit-type" $ do -- this merely computes the new type and its hash, hence a GET!
    (loc, a, t) <- S.jsonData
    t <- runN $ N.editType node loc a t
    S.json t
  S.get "/metadatas" $ do
    hs <- S.jsonData
    md <- runN $ N.metadatas node hs
    S.json md
  S.get "/search" $ do
    (t,q) <- S.jsonData
    es <- runN $ N.search node t q
    S.json es
  S.get "/search-local" $ do
    (h,p,t) <- S.jsonData
    es <- runN $ N.searchLocal node h p t
    S.json es
  S.get "/terms" $ do
    hs <- S.jsonData
    r <- runN $ N.terms node hs
    S.json r
  S.get "/transitive-dependencies" $ do
    (limit,h) <- S.jsonData
    s <- runN $ N.transitiveDependencies node limit h
    S.json s
  S.get "/transitive-dependents" $ do
    (limit,h) <- S.jsonData
    s <- runN $ N.transitiveDependents node limit h
    S.json s
  S.get "/types" $ do
    hs <- S.jsonData
    ts <- runN $ N.types node hs
    S.json ts
  S.get "/type-of" . route $ do
    (h,loc) <- S.jsonData
    s <- runN $ N.typeOf node h loc
    S.json s
  S.post "/update-metadata" $ do
    (h,md) <- S.jsonData
    s <- runN $ N.updateMetadata node h md
    S.json s
  S.defaultHandler $ \msg -> originPolicy *> S.raise msg
  {-
  S.get "/type-of-constructor-argument" $ do
    (h,loc) <- S.jsonData
    s <- runN $ N.typeOf node h loc
    S.json s
  -}

instance J.ToJSON a => J.ToJSON (M.Map Hash a) where
  toJSON m = J.toJSON . M.fromList . map (\(h,v) -> (H.base64 h, v)) . M.toList $ m

instance J.ToJSON a => J.ToJSON (M.Map Reference a) where
  toJSON = J.toJSON . M.toList
