{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Node.Server where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Type as T
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)
import Unison.Node (Node)
import qualified Unison.Node as N
import Unison.Note (Noted, unnote)
import qualified Web.Scotty as S
import Web.Scotty (ActionM)

runN :: Noted IO a -> ActionM a
runN n = liftIO (unnote n) >>= go
  where go (Left e) = S.raise (TL.pack (show e))
        go (Right a) = pure a

server :: Int -> Node IO Hash Type Term -> IO ()
server port node = S.scotty port $ do
  S.get "/admissible-type-of" $ do
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
  S.get "/edit-term" $ do -- this merely computes the new term and its hash, hence a GET!
    (h, loc, a) <- S.jsonData
    (k, e) <- runN $ N.editTerm node h loc a
    S.json (k, e) -- we might follow this up with a 'create-term', which is a POST
  S.get "/edit-type" $ do -- this merely computes the new type and its hash, hence a GET!
    (h, loc, a) <- S.jsonData
    (k, e) <- runN $ N.editType node h loc a
    S.json (k, e)
  S.get "/metadata" $ do
    h <- S.jsonData
    md <- runN $ N.metadata node h
    S.json md
  S.get "/panel" $ do
    h <- S.jsonData
    p <- runN $ N.panel node h
    S.json p
  S.get "/search" $ do
    (t,limit,q) <- S.jsonData
    p <- runN $ N.search node t limit q
    S.json p
  S.get "/search-local" $ do
    (h,loc,t,q) <- S.jsonData
    p <- runN $ N.searchLocal node h loc t q
    S.json p
  S.get "/term" $ do
    h <- S.jsonData
    e <- runN $ N.term node h
    S.json e
  S.get "/transitive-dependencies" $ do
    (limit,h) <- S.jsonData
    s <- runN $ N.transitiveDependencies node limit h
    S.json s
  S.get "/transitive-dependents" $ do
    (limit,h) <- S.jsonData
    s <- runN $ N.transitiveDependents node limit h
    S.json s
  S.get "/type" $ do
    h <- S.jsonData
    t <- runN $ N.typ node h
    S.json t
  S.get "/type-of" $ do
    (h,loc) <- S.jsonData
    s <- runN $ N.typeOf node h loc
    S.json s
  S.post "/update-metadata" $ do
    (h,md) <- S.jsonData
    s <- runN $ N.updateMetadata node h md
    S.json s
  {-
  S.get "/type-of-constructor-argument" $ do
    (h,loc) <- S.jsonData
    s <- runN $ N.typeOf node h loc
    S.json s
  -}

instance J.ToJSON a => J.ToJSON (M.Map Hash a) where
  toJSON m = J.toJSON . M.fromList . map (\(h,v) -> (H.base64 h, v)) . M.toList $ m
