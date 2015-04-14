{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Node.Server where

import Control.Applicative
import Control.Monad.IO.Class
import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import Unison.Hash (Hash)
import Unison.Node (Node)
import Unison.Note (Noted, unnote)
import Unison.Reference (Reference)
import Web.Scotty (ActionM)
import qualified Data.Aeson as J
import qualified Data.Aeson.Parser as JP
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Unison.Hash as H
import qualified Unison.Node as N
import qualified Unison.Term as E
import qualified Unison.Type as T
import qualified Web.Scotty as S

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
  action

postRoute :: S.RoutePattern -> ActionM () -> S.ScottyM ()
postRoute s action = S.post s (route action)

server :: Int -> Node IO Reference T.Type E.Term -> IO ()
server port node = S.scotty port $ do
  S.addroute OPTIONS (S.regex ".*") $ originOptions
  postRoute "/admissible-type-of" $ do
    (h, path) <- S.jsonData
    t <- runN $ N.admissibleTypeOf node h path
    S.json t
  postRoute "/create-term" $ do
    (e, md) <- S.jsonData
    k <- runN $ N.createTerm node e md
    S.json k
  postRoute "/create-type" $ do
    (t, md) <- S.jsonData
    k <- runN $ N.createType node t md
    S.json k
  postRoute "/dependencies" $ do
    (limit, h) <- S.jsonData
    k <- runN $ N.dependencies node limit h
    S.json k
  postRoute "/dependents" $ do
    (limit, h) <- S.jsonData
    k <- runN $ N.dependents node limit h
    S.json k
  postRoute "/edit-term" $ do -- this merely computes the new term, hence a GET
    (rootLoc, loc, a, e) <- S.jsonData
    e <- runN $ N.editTerm node rootLoc loc a e
    S.json e -- we might follow this up with a 'create-term', which is a POST
  postRoute "/edit-type" $ do -- this merely computes the new type and its hash, hence a GET!
    (loc, a, t) <- S.jsonData
    t <- runN $ N.editType node loc a t
    S.json t
  postRoute "/evaluate-terms" $ do
    es <- S.jsonData
    e <- runN $ N.evaluateTerms node es
    S.json e
  postRoute "/local-info" $ do
    (e, path) <- S.jsonData
    t <- runN $ N.localInfo node e path
    S.json t
  postRoute "/metadatas" $ do
    hs <- S.jsonData
    md <- runN $ N.metadatas node hs
    S.json md
  postRoute "/search" $ do
    (e,path,limit,q,t) <- S.jsonData
    es <- runN $ N.search node e path limit q t
    S.json es
  postRoute "/terms" $ do
    hs <- S.jsonData
    r <- runN $ N.terms node hs
    S.json r
  postRoute "/transitive-dependencies" $ do
    (limit,h) <- S.jsonData
    s <- runN $ N.transitiveDependencies node limit h
    S.json s
  postRoute "/transitive-dependents" $ do
    (limit,h) <- S.jsonData
    s <- runN $ N.transitiveDependents node limit h
    S.json s
  postRoute "/types" $ do
    hs <- S.jsonData
    ts <- runN $ N.types node hs
    S.json ts
  postRoute "/type-of" . route $ do
    (h,loc) <- S.jsonData
    s <- runN $ N.typeOf node h loc
    S.json s
  postRoute "/update-metadata" $ do
    (h,md) <- S.jsonData
    s <- runN $ N.updateMetadata node h md
    S.json s
  S.defaultHandler $ \msg -> originPolicy *> S.raise msg
  {-
  postRoute "/type-of-constructor-argument" $ do
    (h,loc) <- S.jsonData
    s <- runN $ N.typeOf node h loc
    S.json s
  -}

instance J.ToJSON a => J.ToJSON (M.Map Hash a) where
  toJSON m = J.toJSON . M.fromList . map (\(h,v) -> (H.base64 h, v)) . M.toList $ m

instance J.ToJSON a => J.ToJSON (M.Map Reference a) where
  toJSON = J.toJSON . M.toList
