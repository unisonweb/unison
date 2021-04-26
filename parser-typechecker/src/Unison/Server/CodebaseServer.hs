{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.CodebaseServer where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Async (race)
import Control.Exception (throwIO, ErrorCall(..))
import Control.Lens
  ( (&),
    (.~),
  )
import Data.Aeson ()
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (Foldable (toList))
import Data.Monoid (Endo (..), appEndo)
import Data.OpenApi
  ( Info (..),
    License (..),
    OpenApi,
    URL (..),
  )
import Data.OpenApi.Lens (info)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import qualified Data.Text as Text
import GHC.Generics ()
import Network.HTTP.Types.Status (ok200)
import Network.Wai
  ( Request,
    queryString,
    responseLBS,
  )
import Network.Wai.Handler.Warp
  ( Port,
    defaultSettings,
    setHost,
    setPort,
    withApplicationSettings,
    runSettings,
    setBeforeMainLoop
  )
import Servant
  ( Header,
    addHeader,
    serveWithContext,
    throwError,
  )
import Servant.API
  ( Get,
    Headers,
    JSON,
    Raw,
    (:>),
    type (:<|>) (..),
  )
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Docs
  ( DocIntro (DocIntro),
    docsWithIntros,
    markdown,
  )
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server
  ( Application,
    Context (..),
    Server,
    ServerError (..),
    Tagged (Tagged),
    err401,
  )
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
    mkAuthHandler,
  )
import System.Environment (lookupEnv)
import System.Random.Stateful
  ( getStdGen,
    newAtomicGenM,
    uniformByteStringM,
  )
import Text.Read (readMaybe)
import Unison.Codebase (Codebase)
import Unison.Parser (Ann)
import Unison.Server.Endpoints.FuzzyFind
  ( FuzzyFindAPI,
    serveFuzzyFind,
  )
import Unison.Server.Endpoints.GetDefinitions
  ( DefinitionsAPI,
    serveDefinitions,
  )
import Unison.Server.Endpoints.ListNamespace
  ( NamespaceAPI,
    serveNamespace,
  )
import Unison.Server.Types (mungeString)
import Unison.Var (Var)

type OpenApiJSON = "openapi.json"
  :> Get '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] OpenApi)

type DocAPI = AuthProtect "token-auth" :> (UnisonAPI :<|> OpenApiJSON :<|> Raw)

type UnisonAPI = NamespaceAPI :<|> DefinitionsAPI :<|> FuzzyFindAPI

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

data Waiter a
  = Waiter {
    notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }

-- The auth token required for accessing the server is passed to the function k
start
  :: Var v => Codebase IO v Ann -> (Strict.ByteString -> Port -> IO ()) -> IO ()
start codebase k = do
  envToken <- lookupEnv "UCM_TOKEN"
  envHost  <- lookupEnv "UCM_HOST"
  envPort  <- (readMaybe =<<) <$> lookupEnv "UCM_PORT"
  token    <- case envToken of
    Just t -> return $ C8.pack t
    _      -> genToken
  let settings = appEndo
        (  foldMap (Endo . setPort)              envPort
        <> foldMap (Endo . setHost . fromString) envHost
        )
        defaultSettings
      a = app codebase token
  case envPort of
    Nothing -> withApplicationSettings settings (pure a) (k token)
    Just p -> do
      started <- mkWaiter
      let settings' = setBeforeMainLoop (notify started ()) settings
      result <- race
                  (runSettings settings' a)
                  (waitFor started *> k token p)
      case result of
        Left () -> throwIO $ ErrorCall "Server exited unexpectedly!"
        Right x -> pure x

server :: Var v => Codebase IO v Ann -> Server DocAPI
server codebase _ =
  (serveNamespace codebase :<|> serveDefinitions codebase :<|> serveFuzzyFind codebase)
    :<|> addHeader "*"
    <$>  serveOpenAPI
    :<|> Tagged serveDocs
 where
  serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
  serveOpenAPI = pure openAPI
  plain        = ("Content-Type", "text/plain")
