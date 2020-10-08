{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.CodebaseServer where

import Data.Aeson ()
import qualified Data.ByteString.Lazy as LZ
import Data.Proxy (Proxy (..))
import GHC.Generics ()
import Network.HTTP.Types.Status (ok200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import OpenAPI (InfoObject (..), LicenseObject (..), OpenAPI)
import Servant.API
  ((:>), Get, JSON,  Raw,
    type (:<|>) (..),
  )
import Servant.Docs
  ( DocIntro (DocIntro),
    docsWithIntros,
    markdown,
  )
import Servant.OpenAPI (blankInfo, toOpenAPI)
import Servant.Server
  ( Application,
    Server,
    Tagged (Tagged),
    serve,
  )
import Unison.Codebase (Codebase)
import Unison.Parser (Ann)
import Unison.Server.Endpoints.ListNamespace
  ( NamespaceAPI,
    serveNamespace,
  )
import Unison.Server.Types (mungeString)
import Unison.Var (Var)

type OpenApiJSON = "openapi.json" :> Get '[JSON] OpenAPI

type DocAPI = UnisonAPI :<|> OpenApiJSON :<|> Raw

type UnisonAPI = NamespaceAPI

openAPI :: OpenAPI
openAPI = toOpenAPI api infoObject

infoObject :: InfoObject
infoObject =
  blankInfo
    { title = "Unison Codebase API",
      description = Just "Provides operations for querying and manipulating a Unison codebase.",
      license = Just . LicenseObject "MIT" $ Just "https://github.com/unisonweb/unison/blob/trunk/LICENSE"
    }

docsBS :: LZ.ByteString
docsBS = mungeString . markdown $ docsWithIntros [intro] api
  where
    intro = DocIntro "Unison Codebase Manager API Server" []

docAPI :: Proxy DocAPI
docAPI = Proxy

api :: Proxy UnisonAPI
api = Proxy

app :: Var v => Codebase IO v Ann -> Application
app codebase = serve docAPI $ server codebase

start :: Var v => Codebase IO v Ann -> Int -> IO ()
start codebase port = run port $ app codebase

server :: Var v => Codebase IO v Ann -> Server DocAPI
server codebase = serveNamespace codebase :<|> serveOpenAPI :<|> Tagged serveDocs
  where
    serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
    serveOpenAPI = pure openAPI
    plain = ("Content-Type", "text/plain")
