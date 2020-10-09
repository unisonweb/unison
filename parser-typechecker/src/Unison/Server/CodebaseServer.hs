{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.CodebaseServer where

import           Data.Aeson                     ( )
import qualified Data.ByteString.Lazy          as LZ
import           Data.OpenApi                   ( URL(..)
                                                , Info(..)
                                                , License(..)
                                                , OpenApi
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Generics                   ( )
import           Network.HTTP.Types.Status      ( ok200 )
import           Network.Wai                    ( responseLBS )
import           Network.Wai.Handler.Warp       ( run )
import           Servant.API                    (Headers,  Get
                                                , JSON
                                                , Raw
                                                , (:>)
                                                , type (:<|>)(..)
                                                )
import           Servant.Docs                   ( DocIntro(DocIntro)
                                                , docsWithIntros
                                                , markdown
                                                )
import           Servant.Server                 ( Application
                                                , Server
                                                , Tagged(Tagged)
                                                , serve
                                                )
import           Unison.Codebase                ( Codebase )
import           Unison.Parser                  ( Ann )
import           Unison.Server.Endpoints.ListNamespace
                                                ( NamespaceAPI
                                                , serveNamespace
                                                )
import           Unison.Server.Types            ( mungeString )
import           Unison.Var                     ( Var )
import           Servant.OpenApi                ( HasOpenApi(toOpenApi) )
import           Servant                        ( Header, addHeader )

type OpenApiJSON = "openapi.json"
  :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] OpenApi)

type DocAPI = UnisonAPI :<|> OpenApiJSON :<|> Raw

type UnisonAPI = NamespaceAPI

openAPI :: OpenApi
openAPI = toOpenApi api

infoObject :: Info
infoObject = mempty
  { _infoTitle       = "Unison Codebase API"
  , _infoDescription = Just
    "Provides operations for querying and manipulating a Unison codebase."
  , _infoLicense     = Just . License "MIT" . Just $ URL
                         "https://github.com/unisonweb/unison/blob/trunk/LICENSE"
  , _infoVersion     = "1.0"
  }

docsBS :: LZ.ByteString
docsBS = mungeString . markdown $ docsWithIntros [intro] api
  where intro = DocIntro "Unison Codebase Manager API Server" []

docAPI :: Proxy DocAPI
docAPI = Proxy

api :: Proxy UnisonAPI
api = Proxy

app :: Var v => Codebase IO v Ann -> Application
app codebase = serve docAPI $ server codebase

start :: Var v => Codebase IO v Ann -> Int -> IO ()
start codebase port = run port $ app codebase

server :: Var v => Codebase IO v Ann -> Server DocAPI
server codebase =
  serveNamespace codebase
    :<|> addHeader "*"
    <$>  serveOpenAPI
    :<|> Tagged serveDocs
 where
  serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
  serveOpenAPI = pure openAPI
  plain        = ("Content-Type", "text/plain")
