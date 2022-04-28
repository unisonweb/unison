{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.CodebaseServer where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Async (race)
import Control.Exception (ErrorCall (..), throwIO)
import Control.Lens ((.~))
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Aeson ()
import qualified Data.ByteString as Strict
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.NanoID (customNanoID, defaultAlphabet, unNanoID)
import Data.OpenApi (Info (..), License (..), OpenApi, URL (..))
import qualified Data.OpenApi.Lens as OpenApi
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics ()
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Status (ok200)
import qualified Network.URI.Encode as URI
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
  ( Port,
    defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setHost,
    setPort,
    withApplicationSettings,
  )
import Servant
  ( Handler,
    HasServer,
    MimeRender (..),
    ServerT,
    serve,
    throwError,
  )
import Servant.API
  ( Accept (..),
    Capture,
    CaptureAll,
    Get,
    JSON,
    Raw,
    (:>),
    type (:<|>) (..),
  )
import Servant.Docs
  ( DocIntro (DocIntro),
    ToSample (..),
    docsWithIntros,
    markdown,
    singleSample,
  )
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server
  ( Application,
    Handler (Handler),
    Server,
    ServerError (..),
    Tagged (Tagged),
    err401,
    err404,
    hoistServer,
  )
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import System.Random.MWC (createSystemRandom)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase.Runtime as Rt
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend (Backend, BackendEnv, runBackend)
import Unison.Server.Endpoints.FuzzyFind (FuzzyFindAPI, serveFuzzyFind)
import Unison.Server.Endpoints.GetDefinitions
  ( DefinitionsAPI,
    serveDefinitions,
  )
import qualified Unison.Server.Endpoints.NamespaceDetails as NamespaceDetails
import qualified Unison.Server.Endpoints.NamespaceListing as NamespaceListing
import qualified Unison.Server.Endpoints.Projects as Projects
import Unison.Server.Errors (backendError)
import Unison.Server.Types (mungeString, setCacheControl)
import Unison.Symbol (Symbol)

-- HTML content type
data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type OpenApiJSON = "openapi.json" :> Get '[JSON] OpenApi

type UnisonAndDocsAPI = UnisonAPI :<|> OpenApiJSON :<|> Raw

type UnisonAPI =
  NamespaceListing.NamespaceListingAPI
    :<|> NamespaceDetails.NamespaceDetailsAPI
    :<|> Projects.ProjectsAPI
    :<|> DefinitionsAPI
    :<|> FuzzyFindAPI

type WebUI = CaptureAll "route" Text :> Get '[HTML] RawHtml

type ServerAPI = ("ui" :> WebUI) :<|> ("api" :> UnisonAndDocsAPI)

type StaticAPI = "static" :> Raw

type Authed api = (Capture "token" Text :> api)

type AppAPI = StaticAPI :<|> Authed ServerAPI

instance ToSample Char where
  toSamples _ = singleSample 'x'

-- BaseUrl and helpers

data BaseUrl = BaseUrl
  { urlHost :: String,
    urlToken :: Strict.ByteString,
    urlPort :: Port
  }

data BaseUrlPath = UI | Api

instance Show BaseUrl where
  show url = urlHost url <> ":" <> show (urlPort url) <> "/" <> (URI.encode . unpack . urlToken $ url)

urlFor :: BaseUrlPath -> BaseUrl -> String
urlFor path baseUrl =
  case path of
    UI -> show baseUrl <> "/ui"
    Api -> show baseUrl <> "/api"

handleAuth :: Strict.ByteString -> Text -> Handler ()
handleAuth expectedToken gotToken =
  if Text.decodeUtf8 expectedToken == gotToken
    then pure ()
    else throw401 "Authentication token missing or incorrect."
  where
    throw401 msg = throwError $ err401 {errBody = msg}

openAPI :: OpenApi
openAPI = toOpenApi api & OpenApi.info .~ infoObject

infoObject :: Info
infoObject =
  mempty
    { _infoTitle = "Unison Codebase Manager API",
      _infoDescription =
        Just "Provides operations for querying and manipulating a Unison codebase.",
      _infoLicense =
        Just . License "MIT" . Just $
          URL
            "https://github.com/unisonweb/unison/blob/trunk/LICENSE",
      _infoVersion = "1.0"
    }

docsBS :: Lazy.ByteString
docsBS = mungeString . markdown $ docsWithIntros [intro] api
  where
    intro =
      DocIntro
        (Text.unpack $ _infoTitle infoObject)
        (toList $ Text.unpack <$> _infoDescription infoObject)

unisonAndDocsAPI :: Proxy UnisonAndDocsAPI
unisonAndDocsAPI = Proxy

api :: Proxy UnisonAPI
api = Proxy

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

appAPI :: Proxy AppAPI
appAPI = Proxy

app ::
  BackendEnv ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  FilePath ->
  Strict.ByteString ->
  Application
app env rt codebase uiPath expectedToken =
  serve appAPI $ server env rt codebase uiPath expectedToken

-- | The Token is used to help prevent multiple users on a machine gain access to
-- each others codebases.
genToken :: IO Strict.ByteString
genToken = do
  g <- createSystemRandom
  n <- customNanoID defaultAlphabet 16 g
  pure $ unNanoID n

data Waiter a = Waiter
  { notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return
    Waiter
      { notify = putMVar mvar,
        waitFor = readMVar mvar
      }

ucmUIVar :: String
ucmUIVar = "UCM_WEB_UI"

ucmPortVar :: String
ucmPortVar = "UCM_PORT"

ucmHostVar :: String
ucmHostVar = "UCM_HOST"

ucmTokenVar :: String
ucmTokenVar = "UCM_TOKEN"

data CodebaseServerOpts = CodebaseServerOpts
  { token :: Maybe String,
    host :: Maybe String,
    port :: Maybe Int,
    codebaseUIPath :: Maybe FilePath
  }
  deriving (Show, Eq)

-- The auth token required for accessing the server is passed to the function k
startServer ::
  BackendEnv ->
  CodebaseServerOpts ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  (BaseUrl -> IO ()) ->
  IO ()
startServer env opts rt codebase onStart = do
  -- the `canonicalizePath` resolves symlinks
  exePath <- canonicalizePath =<< getExecutablePath
  envUI <- canonicalizePath $ fromMaybe (FilePath.takeDirectory exePath </> "ui") (codebaseUIPath opts)
  token <- case token opts of
    Just t -> return $ C8.pack t
    _ -> genToken
  let baseUrl = BaseUrl "http://127.0.0.1" token
  let settings =
        defaultSettings
          & maybe id setPort (port opts)
          & maybe id (setHost . fromString) (host opts)
  let a = app env rt codebase envUI token
  case port opts of
    Nothing -> withApplicationSettings settings (pure a) (onStart . baseUrl)
    Just p -> do
      started <- mkWaiter
      let settings' = setBeforeMainLoop (notify started ()) settings
      result <-
        race
          (runSettings settings' a)
          (waitFor started *> onStart (baseUrl p))
      case result of
        Left () -> throwIO $ ErrorCall "Server exited unexpectedly!"
        Right x -> pure x

serveIndex :: FilePath -> Handler RawHtml
serveIndex path = do
  let index = path </> "index.html"
  exists <- liftIO $ doesFileExist index
  if exists
    then fmap RawHtml . liftIO . Lazy.readFile $ path </> "index.html"
    else fail
  where
    fail =
      throwError $
        err404
          { errBody =
              BLU.fromString $
                "No codebase UI configured."
                  <> " Set the "
                  <> ucmUIVar
                  <> " environment variable to the directory where the UI is installed."
          }

serveUI :: FilePath -> Server WebUI
serveUI path _ = serveIndex path

server ::
  BackendEnv ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  FilePath ->
  Strict.ByteString ->
  Server AppAPI
server backendEnv rt codebase uiPath expectedToken =
  serveDirectoryWebApp (uiPath </> "static")
    :<|> hoistWithAuth serverAPI expectedToken serveServer
  where
    serveServer :: Server ServerAPI
    serveServer =
      ( serveUI uiPath
          :<|> serveUnisonAndDocs backendEnv rt codebase
      )

serveUnisonAndDocs :: BackendEnv -> Rt.Runtime Symbol -> Codebase IO Symbol Ann -> Server UnisonAndDocsAPI
serveUnisonAndDocs env rt codebase = serveUnison env codebase rt :<|> serveOpenAPI :<|> Tagged serveDocs

serveDocs :: Application
serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
  where
    plain :: (HeaderName, ByteString)
    plain = ("Content-Type", "text/plain")

serveOpenAPI :: Handler OpenApi
serveOpenAPI = pure openAPI

hoistWithAuth :: forall api. HasServer api '[] => Proxy api -> ByteString -> ServerT api Handler -> ServerT (Authed api) Handler
hoistWithAuth api expectedToken server token = hoistServer @api @Handler @Handler api (\h -> handleAuth expectedToken token *> h) server

serveUnison ::
  BackendEnv ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  Server UnisonAPI
serveUnison env codebase rt =
  hoistServer (Proxy @UnisonAPI) (backendHandler env) $
    (\root rel name -> setCacheControl <$> NamespaceListing.serve codebase root rel name)
      :<|> (\namespaceName mayRoot mayWidth -> setCacheControl <$> NamespaceDetails.serve rt codebase namespaceName mayRoot mayWidth)
      :<|> (\mayRoot mayOwner -> setCacheControl <$> Projects.serve codebase mayRoot mayOwner)
      :<|> (\mayRoot relativePath rawHqns width suff -> setCacheControl <$> serveDefinitions rt codebase mayRoot relativePath rawHqns width suff)
      :<|> (\mayRoot relativePath limit typeWidth query -> setCacheControl <$> serveFuzzyFind codebase mayRoot relativePath limit typeWidth query)

backendHandler :: BackendEnv -> Backend IO a -> Handler a
backendHandler env m =
  Handler $ withExceptT backendError (runReaderT (runBackend m) env)
