{-# LANGUAGE DataKinds #-}
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
import Data.ByteString qualified as Strict
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Lazy.UTF8 qualified as BLU
import Data.NanoID (customNanoID, defaultAlphabet, unNanoID)
import Data.OpenApi (Info (..), License (..), OpenApi, URL (..))
import Data.OpenApi.Lens qualified as OpenApi
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics ()
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Status (ok200)
import Network.URI.Encode as UriEncode
import Network.URI.Encode qualified as URI
import Network.Wai (Middleware, responseLBS)
import Network.Wai.Handler.Warp
  ( Port,
    defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setHost,
    setPort,
    withApplicationSettings,
  )
import Network.Wai.Middleware.Cors (cors, corsMethods, corsOrigins, simpleCorsResourcePolicy)
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
import System.FilePath qualified as FilePath
import System.Random.MWC (createSystemRandom)
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.HashQualified
import Unison.Name as Name (Name, segments)
import Unison.NameSegment qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Server.Backend (Backend, BackendEnv, runBackend)
import Unison.Server.Backend qualified as Backend
import Unison.Server.Errors (backendError)
import Unison.Server.Local.Endpoints.DefinitionSummary (TermSummaryAPI, TypeSummaryAPI, serveTermSummary, serveTypeSummary)
import Unison.Server.Local.Endpoints.FuzzyFind (FuzzyFindAPI, serveFuzzyFind)
import Unison.Server.Local.Endpoints.GetDefinitions
  ( DefinitionsAPI,
    serveDefinitions,
  )
import Unison.Server.Local.Endpoints.NamespaceDetails qualified as NamespaceDetails
import Unison.Server.Local.Endpoints.NamespaceListing qualified as NamespaceListing
import Unison.Server.Local.Endpoints.Projects qualified as Projects
import Unison.Server.Types (mungeString, setCacheControl)
import Unison.ShortHash qualified as ShortHash
import Unison.Symbol (Symbol)

-- HTML content type
data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type OpenApiJSON = "openapi.json" :> Get '[JSON] OpenApi

type UnisonAndDocsAPI = UnisonLocalAPI :<|> OpenApiJSON :<|> Raw

type LooseCodeAPI = CodebaseServerAPI

type UnisonLocalAPI = ProjectsAPI :<|> LooseCodeAPI

type CodebaseServerAPI =
  NamespaceListing.NamespaceListingAPI
    :<|> NamespaceDetails.NamespaceDetailsAPI
    :<|> Projects.ProjectsAPI
    :<|> DefinitionsAPI
    :<|> FuzzyFindAPI
    :<|> TermSummaryAPI
    :<|> TypeSummaryAPI

type ProjectsAPI =
  "projects" :> Capture "project-name" ProjectName :> "branches" :> Capture "branch-name" ProjectBranchName :> LooseCodeAPI

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

data DefinitionReference
  = TermReference (HashQualified Name) -- /terms/...
  | TypeReference (HashQualified Name) -- /types/...
  | AbilityConstructorReference (HashQualified Name) -- /ability-constructors/...
  | DataConstructorReference (HashQualified Name) -- /data-constructors/...
  deriving stock (Show)

data Service
  = LooseCodeUI Path.Absolute (Maybe DefinitionReference)
  | ProjectBranchUI (ProjectAndBranch ProjectName ProjectBranchName) (Maybe DefinitionReference)
  | Api
  deriving stock (Show)

instance Show BaseUrl where
  show url = urlHost url <> ":" <> show (urlPort url) <> "/" <> (URI.encode . unpack . urlToken $ url)

data URISegment
  = EscapeMe Text
  | DontEscape Text
  deriving stock (Show)

-- | Create a Service URL, either for the UI or the API
--
-- Examples:
--
-- >>> urlFor Api (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- "http://localhost:1234/asdf/api"
--
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> let service = LooseCodeUI (Path.absoluteEmpty) (Just (TermReference (NameOnly (Name.unsafeFromText "base.data.List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/latest/;/terms/base/data/List/map"
--
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> let service = LooseCodeUI (Path.Absolute (Path.fromText "base.data")) (Just (TermReference (NameOnly (Name.unsafeFromText "List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/latest/namespaces/base/data/;/terms/List/map"
--
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> import Unison.Core.Project (ProjectName (..), ProjectBranchName (..), ProjectAndBranch (..))
-- >>> let service = ProjectBranchUI (ProjectAndBranch (UnsafeProjectName "base") (UnsafeProjectBranchName "main")) (Just (TermReference (NameOnly (Name.unsafeFromText "List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/projects/base/main/latest/;/terms/List/map"
--
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> import Unison.Core.Project (ProjectName (..), ProjectBranchName (..), ProjectAndBranch (..))
-- >>> let service = ProjectBranchUI (ProjectAndBranch (UnsafeProjectName "@unison/base") (UnsafeProjectBranchName "@runarorama/contribution")) (Just (TermReference (NameOnly (Name.unsafeFromText "List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/projects/@unison/base/@runarorama/contribution/latest/;/terms/List/map"
urlFor :: Service -> BaseUrl -> Text
urlFor service baseUrl =
  case service of
    LooseCodeUI ns def ->
      toUrlPath ([DontEscape "ui", DontEscape "non-project-code"] <> path ns def)
    ProjectBranchUI (ProjectAndBranch projectName branchName) def ->
      toUrlPath $ [DontEscape "ui", DontEscape "projects", DontEscape $ into @Text projectName, DontEscape $ into @Text branchName] <> path Path.absoluteEmpty def
    Api -> toUrlPath [DontEscape "api"]
  where
    path :: Path.Absolute -> Maybe DefinitionReference -> [URISegment]
    path ns def =
      let nsPath = namespacePath ns
       in case definitionPath def of
            Just defPath -> [DontEscape "latest"] <> nsPath <> [DontEscape ";"] <> defPath
            Nothing -> [DontEscape "latest"] <> nsPath

    namespacePath :: Path.Absolute -> [URISegment]
    namespacePath path =
      if path == Path.absoluteEmpty
        then []
        else [DontEscape "namespaces"] <> (EscapeMe . NameSegment.toText <$> Path.toList (Path.unabsolute path))

    definitionPath :: Maybe DefinitionReference -> Maybe [URISegment]
    definitionPath def =
      toDefinitionPath <$> def

    toUrlPath :: [URISegment] -> Text
    toUrlPath path =
      path
        & fmap \case
          EscapeMe txt -> UriEncode.encodeText txt
          DontEscape txt -> txt
        & (tShow baseUrl :)
        & Text.intercalate "/"

    refToUrlText :: HashQualified Name -> [URISegment]
    refToUrlText r =
      case r of
        NameOnly n ->
          n & Name.segments & fmap (EscapeMe . NameSegment.toText) & toList
        HashOnly h ->
          [EscapeMe $ ShortHash.toText h]
        HashQualified n _ ->
          n & Name.segments & fmap (EscapeMe . NameSegment.toText) & toList

    toDefinitionPath :: DefinitionReference -> [URISegment]
    toDefinitionPath d =
      case d of
        TermReference r ->
          [DontEscape "terms"] <> refToUrlText r
        TypeReference r ->
          [DontEscape "types"] <> refToUrlText r
        AbilityConstructorReference r ->
          [DontEscape "ability-constructors"] <> refToUrlText r
        DataConstructorReference r ->
          [DontEscape "data-constructors"] <> refToUrlText r

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

api :: Proxy UnisonLocalAPI
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
  Maybe String ->
  Application
app env rt codebase uiPath expectedToken allowCorsHost =
  corsPolicy allowCorsHost $ serve appAPI $ server env rt codebase uiPath expectedToken

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

ucmAllowCorsHost :: String
ucmAllowCorsHost = "UCM_ALLOW_CORS_HOST"

ucmTokenVar :: String
ucmTokenVar = "UCM_TOKEN"

data CodebaseServerOpts = CodebaseServerOpts
  { token :: Maybe String,
    host :: Maybe String,
    port :: Maybe Int,
    allowCorsHost :: Maybe String,
    codebaseUIPath :: Maybe FilePath
  }
  deriving (Show, Eq)

defaultCodebaseServerOpts :: CodebaseServerOpts
defaultCodebaseServerOpts =
  CodebaseServerOpts
    { token = Nothing,
      host = Nothing,
      port = Nothing,
      allowCorsHost = Nothing,
      codebaseUIPath = Nothing
    }

-- The auth token required for accessing the server is passed to the function k
startServer ::
  BackendEnv ->
  CodebaseServerOpts ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  (BaseUrl -> IO a) ->
  IO a
startServer env opts rt codebase onStart = do
  -- the `canonicalizePath` resolves symlinks
  exePath <- canonicalizePath =<< getExecutablePath
  envUI <- canonicalizePath $ fromMaybe (FilePath.takeDirectory exePath </> "ui") (codebaseUIPath opts)
  token <- case token opts of
    Just t -> return $ C8.pack t
    _ -> genToken
  let baseUrl = BaseUrl (fromMaybe "http://127.0.0.1" (host opts)) token
  let settings =
        defaultSettings
          & maybe id setPort (port opts)
          & maybe id (setHost . fromString) (host opts)
  let a = app env rt codebase envUI token (allowCorsHost opts)
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
                  <> " If you're running a dev build of ucm, run `./dev-ui-install.sh`."
          }

serveUI :: FilePath -> Server WebUI
serveUI path _ = serveIndex path

-- Apply cors if there is allow-cors-host defined
corsPolicy :: Maybe String -> Middleware
corsPolicy = maybe id \allowCorsHost ->
  cors $
    const $
      Just
        simpleCorsResourcePolicy
          { corsMethods = ["GET", "OPTIONS"],
            corsOrigins = Just ([C8.pack allowCorsHost], True)
          }

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
      serveUI uiPath
        :<|> serveUnisonAndDocs backendEnv rt codebase

serveUnisonAndDocs :: BackendEnv -> Rt.Runtime Symbol -> Codebase IO Symbol Ann -> Server UnisonAndDocsAPI
serveUnisonAndDocs env rt codebase = serveUnisonLocal env codebase rt :<|> serveOpenAPI :<|> Tagged serveDocs

serveDocs :: Application
serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
  where
    plain :: (HeaderName, ByteString)
    plain = ("Content-Type", "text/plain")

serveOpenAPI :: Handler OpenApi
serveOpenAPI = pure openAPI

hoistWithAuth :: forall api. (HasServer api '[]) => Proxy api -> ByteString -> ServerT api Handler -> ServerT (Authed api) Handler
hoistWithAuth api expectedToken server token = hoistServer @api @Handler @Handler api (\h -> handleAuth expectedToken token *> h) server

serveLooseCode ::
  BackendEnv ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  Server LooseCodeAPI
serveLooseCode env codebase rt =
  hoistServer (Proxy @LooseCodeAPI) (backendHandler env) $
    (\root rel name -> setCacheControl <$> NamespaceListing.serve codebase (Left <$> root) rel name)
      :<|> (\namespaceName mayRoot renderWidth -> setCacheControl <$> NamespaceDetails.namespaceDetails rt codebase namespaceName (Left <$> mayRoot) renderWidth)
      :<|> (\mayRoot mayOwner -> setCacheControl <$> Projects.serve codebase (Left <$> mayRoot) mayOwner)
      :<|> (\mayRoot relativePath rawHqns renderWidth suff -> setCacheControl <$> serveDefinitions rt codebase (Left <$> mayRoot) relativePath rawHqns renderWidth suff)
      :<|> (\mayRoot relativePath limit renderWidth query -> setCacheControl <$> serveFuzzyFind codebase (Left <$> mayRoot) relativePath limit renderWidth query)
      :<|> (\shortHash mayName mayRoot relativeTo renderWidth -> setCacheControl <$> serveTermSummary codebase shortHash mayName (Left <$> mayRoot) relativeTo renderWidth)
      :<|> (\shortHash mayName mayRoot relativeTo renderWidth -> setCacheControl <$> serveTypeSummary codebase shortHash mayName (Left <$> mayRoot) relativeTo renderWidth)

serveProjectsAPI ::
  BackendEnv ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  ProjectName ->
  ProjectBranchName ->
  Server LooseCodeAPI
serveProjectsAPI env codebase rt projectName branchName =
  hoistServer (Proxy @LooseCodeAPI) (backendHandler env) $ do
    namespaceListingEndpoint
      :<|> namespaceDetailsEndpoint
      :<|> projectListingEndpoint
      :<|> serveDefinitionsEndpoint
      :<|> serveFuzzyFindEndpoint
      :<|> serveTermSummaryEndpoint
      :<|> serveTypeSummaryEndpoint
  where
    projectAndBranchName = ProjectAndBranch projectName branchName
    namespaceListingEndpoint _rootParam rel name = do
      root <- resolveProjectRoot
      setCacheControl <$> NamespaceListing.serve codebase (Just root) rel name
    namespaceDetailsEndpoint namespaceName _rootParam renderWidth = do
      root <- resolveProjectRoot
      setCacheControl <$> NamespaceDetails.namespaceDetails rt codebase namespaceName (Just root) renderWidth
    projectListingEndpoint _rootParam mayOwner = do
      root <- resolveProjectRoot
      setCacheControl <$> Projects.serve codebase (Just root) mayOwner

    serveDefinitionsEndpoint _rootParam relativePath rawHqns renderWidth suff = do
      root <- resolveProjectRoot
      setCacheControl <$> serveDefinitions rt codebase (Just root) relativePath rawHqns renderWidth suff

    serveFuzzyFindEndpoint _rootParam relativePath limit renderWidth query = do
      root <- resolveProjectRoot
      setCacheControl <$> serveFuzzyFind codebase (Just root) relativePath limit renderWidth query

    serveTermSummaryEndpoint shortHash mayName _rootParam relativeTo renderWidth = do
      root <- resolveProjectRoot
      setCacheControl <$> serveTermSummary codebase shortHash mayName (Just root) relativeTo renderWidth

    serveTypeSummaryEndpoint shortHash mayName _rootParam relativeTo renderWidth = do
      root <- resolveProjectRoot
      setCacheControl <$> serveTypeSummary codebase shortHash mayName (Just root) relativeTo renderWidth

    resolveProjectRoot :: Backend IO (Either ShortCausalHash CausalHash)
    resolveProjectRoot = do
      mayCH <- liftIO . Codebase.runTransaction codebase $ Backend.causalHashForProjectBranchName @IO projectAndBranchName
      case mayCH of
        Nothing -> throwError (Backend.ProjectBranchNameNotFound projectName branchName)
        Just ch -> pure (Right ch)

serveUnisonLocal ::
  BackendEnv ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  Server UnisonLocalAPI
serveUnisonLocal env codebase rt = serveProjectsAPI env codebase rt :<|> serveLooseCode env codebase rt

backendHandler :: BackendEnv -> Backend IO a -> Handler a
backendHandler env m =
  Handler $ withExceptT backendError (runReaderT (runBackend m) env)
