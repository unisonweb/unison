{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.CodebaseServer where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON, FromJSON)
import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import Unison.Hash (Hash)
import Unison.Codebase (Codebase)
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
import qualified Unison.Codebase as C
import qualified Web.Scotty as S
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

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

server :: (Ord v, ToJSON v, FromJSON v) => Int -> Codebase IO v -> IO ()
server port codebase = S.scotty port $ do
  S.middleware logStdoutDev
  S.middleware $ staticPolicy (noDots >-> addBase "./editor")
  S.get "/" $ S.file "./editor/editor.html"
  S.addroute OPTIONS (S.regex ".*") $ originOptions
  postRoute "/admissible-type-at" $ do
    (h, path) <- S.jsonData
    t <- runN $ C.admissibleTypeAt codebase h path
    S.json t
  postRoute "/create-term" $ do
    (e, md) <- S.jsonData
    k <- runN $ C.createTerm codebase e md
    S.json k
  postRoute "/dependencies" $ do
    (limit, h) <- S.jsonData
    k <- runN $ C.dependencies codebase limit h
    S.json k
  postRoute "/dependents" $ do
    (limit, h) <- S.jsonData
    k <- runN $ C.dependents codebase limit h
    S.json k
  postRoute "/edit-term" $ do
    (rootLoc, loc, a, e) <- S.jsonData
    e <- runN $ C.editTerm codebase rootLoc loc a e
    S.json e
  postRoute "/local-info" $ do
    (e, path) <- S.jsonData
    t <- runN $ C.localInfo codebase e path
    S.json t
  postRoute "/metadatas" $ do
    hs <- S.jsonData
    md <- runN $ C.metadatas codebase hs
    S.json md
  postRoute "/search" $ do
    (e,path,limit,q,t) <- S.jsonData
    es <- runN $ C.search codebase e path limit q t
    S.json es
  postRoute "/terms" $ do
    hs <- S.jsonData
    r <- runN $ C.terms codebase hs
    S.json r
  postRoute "/types" $ do
    hs <- S.jsonData
    ts <- runN $ C.types codebase hs
    S.json ts
  postRoute "/type-at" . route $ do
    (h,loc) <- S.jsonData
    s <- runN $ C.typeAt codebase h loc
    S.json s
  postRoute "/update-metadata" $ do
    (h,md) <- S.jsonData
    s <- runN $ C.updateMetadata codebase h md
    S.json s
  S.defaultHandler $ \msg -> originPolicy *> S.raise msg

instance J.ToJSON a => J.ToJSON (M.Map Hash a) where
  toJSON m = J.toJSON . M.fromList . map (\(h,v) -> (H.base64 h, v)) . M.toList $ m

instance J.ToJSON a => J.ToJSON (M.Map Reference a) where
  toJSON = J.toJSON . M.toList
