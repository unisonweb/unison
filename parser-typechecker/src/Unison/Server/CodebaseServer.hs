{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.CodebaseServer where

import           Data.Aeson                     ( )
import qualified Data.ByteString.Lazy          as Lazy
import qualified Data.ByteString               as Strict
import           Data.OpenApi                   ( URL(..)
                                                , Info(..)
                                                , License(..)
                                                , OpenApi
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.Generics                   ( )
import           Network.HTTP.Types.Status      ( ok200 )
import           Network.Wai                    ( responseLBS
                                                , Request
                                                , queryString
                                                )
import           Network.Wai.Handler.Warp       ( runSettings
                                                , withApplicationSettings
                                                , defaultSettings
                                                , Port
                                                , setPort
                                                , setHost
                                                )
import           Servant.API                    (Headers,  Get
                                                , JSON
                                                , Raw
                                                , (:>)
                                                , type (:<|>)(..)
                                                )
import           Servant.API.Experimental.Auth  ( AuthProtect )
import           Servant.Server.Experimental.Auth
                                                ( AuthHandler
                                                , AuthServerData
                                                , mkAuthHandler
                                                )
import           Servant.Docs                   ( DocIntro(DocIntro)
                                                , docsWithIntros
                                                , markdown
                                                )
import           Servant.Server                 ( Application
                                                , Context(..)
                                                , Server
                                                , ServerError(..)
                                                , Tagged(Tagged)
                                                , err401
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
import           Servant                        ( Header
                                                , addHeader
                                                , throwError
                                                , serveWithContext
                                                )
import           Control.Lens                   ( (&)
                                                , (.~)
                                                )
import           Data.OpenApi.Lens              ( info )
import qualified Data.Text                     as Text
import           Data.Foldable                  ( Foldable(toList) )
import           System.Random.Stateful         ( getStdGen
                                                , newAtomicGenM
                                                , uniformByteStringM
                                                )
import qualified Data.ByteString.Base64        as Base64

type OpenApiJSON = "openapi.json"
  :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] OpenApi)

type DocAPI = AuthProtect "token-auth" :> (UnisonAPI :<|> OpenApiJSON :<|> Raw)

type UnisonAPI = NamespaceAPI :<|> DefinitionsAPI

type instance AuthServerData (AuthProtect "token-auth") = ()

genAuthServerContext
  :: Strict.ByteString -> Context (AuthHandler Request ()': '[])
genAuthServerContext token = authHandler token :. EmptyContext

authHandler :: Strict.ByteString -> AuthHandler Request ()
authHandler token = mkAuthHandler handler
 where
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req =
    maybe (throw401 "Authentication token missing or incorrect")
          (const $ pure ())
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

docsBS :: Lazy.ByteString
docsBS = mungeString . markdown $ docsWithIntros [intro] api
 where
  intro = DocIntro (Text.unpack $ _infoTitle infoObject)
                   (toList $ Text.unpack <$> _infoDescription infoObject)

docAPI :: Proxy DocAPI
docAPI = Proxy

api :: Proxy UnisonAPI
api = Proxy

app :: Var v => Codebase IO v Ann -> Strict.ByteString -> Application
app codebase token =
  serveWithContext docAPI (genAuthServerContext token) $ server codebase

genToken :: IO Strict.ByteString
genToken = do
  gen <- getStdGen
  g   <- newAtomicGenM gen
  Base64.encode <$> uniformByteStringM 24 g

-- Returns the auth token required for accessing the server.
-- It expects the token as a query parameter. E.g. if the token is "abc"
-- and `port` is 80, then the server can only be accessed at
-- http://127.0.0.1:80?abc
startOnPort :: Var v => Codebase IO v Ann -> Port -> IO Strict.ByteString
startOnPort codebase port = do
  token <- genToken
  let settings = setHost "127.0.0.1" $ setPort port defaultSettings
  runSettings settings $ app codebase token
  pure token

-- The auth token required for accessing the server is passed to the function k
start
  :: Var v => Codebase IO v Ann -> (Strict.ByteString -> Port -> IO ()) -> IO ()
start codebase k = do
  token <- genToken
  let settings = setHost "127.0.0.1" defaultSettings
  withApplicationSettings settings (pure $ app codebase token) (k token)

server :: Var v => Codebase IO v Ann -> Server DocAPI
server codebase _ =
  (serveNamespace codebase :<|> serveDefinitions codebase)
    :<|> addHeader "*"
    <$>  serveOpenAPI
    :<|> Tagged serveDocs
 where
  serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
  serveOpenAPI = pure openAPI
  plain        = ("Content-Type", "text/plain")
