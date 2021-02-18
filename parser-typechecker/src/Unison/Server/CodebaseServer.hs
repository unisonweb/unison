{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
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
import           Network.Wai.Handler.Warp       ( run, withApplication, Port )
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
import           Unison.Server.Endpoints.GetDefinitions
                                                ( DefinitionsAPI
                                                , serveDefinitions
                                                )
import           Unison.Server.Types            ( mungeString )
import           Unison.Var                     ( Var )
import           Servant.OpenApi                ( HasOpenApi(toOpenApi) )
import           Servant                        ( Header, addHeader )
import Control.Lens ( (&), (.~) )
import Data.OpenApi.Lens (info)
import qualified Data.Text as Text
import Data.Foldable (Foldable(toList))
import qualified Data.ByteString.Random.MWC as MWC
import qualified Data.ByteString.Base64 as Base64

type OpenApiJSON = "openapi.json"
  :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] OpenApi)

type DocAPI = AuthProtect "token-auth" :> (UnisonAPI :<|> OpenApiJSON :<|> Raw)

type UnisonAPI = NamespaceAPI :<|> DefinitionsAPI

genAuthServerContext :: ByteString -> Context (AuthHandler Request ()': '[])
genAuthServerContext token = authHandler token :. EmptyContext

authHandler :: ByteString -> AuthHandler Request ()
authHandler token = mkAuthHandler handler
 where
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req =
    maybe (throw401 "Authentication token missing") (const $ pure ())
      . lookup token
      $ queryString req

openAPI :: OpenApi
openAPI = toOpenApi api & info .~ infoObject

infoObject :: Info
infoObject = mempty
  { _infoTitle       = "Unison Codebase Manager API"
  , _infoDescription =
    Just "Provides operations for querying and manipulating a Unison codebase."
  , _infoLicense     = Just . License "MIT" . Just $ URL
                         "https://github.com/unisonweb/unison/blob/trunk/LICENSE"
  , _infoVersion     = "1.0"
  }

docsBS :: LZ.ByteString
docsBS = mungeString . markdown $ docsWithIntros [intro] api
 where
  intro = DocIntro (Text.unpack $ _infoTitle infoObject)
                   (toList $ Text.unpack <$> _infoDescription infoObject)

docAPI :: Proxy DocAPI
docAPI = Proxy

api :: Proxy UnisonAPI
api = Proxy

app :: Var v => Codebase IO v Ann -> ByteString -> Application
app codebase token =
  serveWithContext docAPI (genAuthServerContext token) $ server codebase

startOnPort :: Var v => Codebase IO v Ann -> Port -> IO ()
startOnPort codebase token port = do
  token <- Base64.encode $ MWC.random 64
  run port $ app codebase token

start :: Var v => Codebase IO v Ann -> (Port -> IO ()) -> IO ()
start codebase = do
  token <- Base64.encode $ MWC.random 64
  withApplication $ app codebase token

server :: Var v => Codebase IO v Ann -> Server DocAPI
server codebase =
  (serveNamespace codebase :<|> serveDefinitions codebase)
    :<|> addHeader "*"
    <$>  serveOpenAPI
    :<|> Tagged serveDocs
 where
  serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
  serveOpenAPI = pure openAPI
  plain        = ("Content-Type", "text/plain")
