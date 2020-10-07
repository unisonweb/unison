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
import Servant.API
  ( Raw,
    type (:<|>) (..),
  )
import Servant.Docs
  ( DocIntro (DocIntro),
    docsWithIntros,
    markdown,
  )
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

type UnisonAPI = NamespaceAPI :<|> Raw

docsBS :: LZ.ByteString
docsBS = mungeString . markdown $ docsWithIntros [intro] api
  where
    intro = DocIntro "Unison Codebase Manager API Server" []

api :: Proxy UnisonAPI
api = Proxy

app :: Var v => Codebase IO v Ann -> Application
app codebase = serve api $ server codebase

start :: Var v => Codebase IO v Ann -> Int -> IO ()
start codebase port = run port $ app codebase

server :: Var v => Codebase IO v Ann -> Server UnisonAPI
server codebase = serveNamespace codebase :<|> Tagged serveDocs
  where
    serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")
