{-# LANGUAGE DataKinds #-}
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
import Servant qualified as Servant
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
    ToParam (..),
    ToSample (..),
    docsWithIntros,
    markdown,
    singleSample,
  )
import Servant.Docs qualified as Servant
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
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.HashQualified
import Unison.HashQualified qualified as HQ
import Unison.Name as Name (Name, segments)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Server.Backend (Backend, BackendEnv, runBackend)
import Unison.Server.Backend qualified as Backend
import Unison.Server.Backend.DefinitionDiff qualified as DefinitionDiff
import Unison.Server.Errors (backendError)
import Unison.Server.Local.Definitions qualified as Defn
import Unison.Server.Local.Endpoints.DefinitionSummary (TermSummaryAPI, TypeSummaryAPI, serveTermSummary, serveTypeSummary)
import Unison.Server.Local.Endpoints.FuzzyFind (FuzzyFindAPI, serveFuzzyFind)
import Unison.Server.Local.Endpoints.GetDefinitions
  ( DefinitionsAPI,
    serveDefinitions,
  )
import Unison.Server.Local.Endpoints.NamespaceDetails qualified as NamespaceDetails
import Unison.Server.Local.Endpoints.NamespaceListing qualified as NamespaceListing
import Unison.Server.Local.Endpoints.Projects (ListProjectBranchesEndpoint, ListProjectsEndpoint, projectBranchListingEndpoint, projectListingEndpoint)
import Unison.Server.Local.Endpoints.UCM (UCMAPI, ucmServer)
import Unison.Server.NameSearch (NameSearch (..))
import Unison.Server.NameSearch.FromNames qualified as Names
import Unison.Server.Types (TermDefinition (..), TermDiffResponse (..), TypeDefinition (..), TypeDiffResponse (..), mungeString, setCacheControl)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Pretty qualified as Pretty

-- | Fail the route with a reasonable error if the query param is missing.
type RequiredQueryParam = Servant.QueryParam' '[Servant.Required, Servant.Strict]

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

type UnisonLocalAPI =
  ("projects" :> ProjectsAPI)
    :<|> ("non-project-code" :> LooseCodeAPI)
    :<|> ("ucm" :> UCMAPI)

type CodebaseServerAPI =
  NamespaceListing.NamespaceListingAPI
    :<|> NamespaceDetails.NamespaceDetailsAPI
    :<|> DefinitionsAPI
    :<|> FuzzyFindAPI
    :<|> TermSummaryAPI
    :<|> TypeSummaryAPI

type ProjectsAPI =
  ListProjectsEndpoint
    :<|> ( Capture "project-name" ProjectName
             :> "branches"
             :> ( ListProjectBranchesEndpoint
                    :<|> (Capture "branch-name" ProjectBranchName :> CodebaseServerAPI)
                    :<|> ( "diff"
                             :> ( "terms" :> ProjectDiffTermsEndpoint
                                    :<|> "types" :> ProjectDiffTypesEndpoint
                                )
                         )
                )
         )

type ProjectDiffTermsEndpoint =
  RequiredQueryParam "oldBranchRef" ProjectBranchName
    :> RequiredQueryParam "newBranchRef" ProjectBranchName
    :> RequiredQueryParam "oldTerm" Name
    :> RequiredQueryParam "newTerm" Name
    :> Get '[JSON] TermDiffResponse

type ProjectDiffTypesEndpoint =
  RequiredQueryParam "oldBranchRef" ProjectBranchName
    :> RequiredQueryParam "newBranchRef" ProjectBranchName
    :> RequiredQueryParam "oldType" Name
    :> RequiredQueryParam "newType" Name
    :> Get '[JSON] TypeDiffResponse

instance ToParam (Servant.QueryParam' mods "oldBranchRef" a) where
  toParam _ = Servant.DocQueryParam "oldBranchRef" ["main"] "The name of the old branch" Servant.Normal

instance ToParam (Servant.QueryParam' mods "newBranchRef" a) where
  toParam _ = Servant.DocQueryParam "newBranchRef" ["main"] "The name of the new branch" Servant.Normal

instance ToParam (Servant.QueryParam' mods "oldTerm" a) where
  toParam _ = Servant.DocQueryParam "oldTerm" ["main"] "The name of the old term" Servant.Normal

instance ToParam (Servant.QueryParam' mods "newTerm" a) where
  toParam _ = Servant.DocQueryParam "newTerm" ["main"] "The name of the new term" Servant.Normal

instance ToParam (Servant.QueryParam' mods "oldType" a) where
  toParam _ = Servant.DocQueryParam "oldType" ["main"] "The name of the old type" Servant.Normal

instance ToParam (Servant.QueryParam' mods "newType" a) where
  toParam _ = Servant.DocQueryParam "newType" ["main"] "The name of the new type" Servant.Normal

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
  | -- (Project branch names, perspective within project, definition reference)
    ProjectBranchUI (ProjectAndBranch ProjectName ProjectBranchName) Path.Path (Maybe DefinitionReference)
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
-- Loose code with definition but no perspective
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> let service = LooseCodeUI (Path.absoluteEmpty) (Just (TermReference (NameOnly (Name.unsafeFromText "base.data.List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/non-project-code/latest/terms/base/data/List/map"
--
-- Loose code with definition and perspective
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> let service = LooseCodeUI (Path.Absolute (Path.fromText "base.data")) (Just (TermReference (NameOnly (Name.unsafeFromText "List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/non-project-code/latest/namespaces/base/data/;/terms/List/map"
--
-- Project with definition but no perspective
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> import Unison.Core.Project (ProjectName (..), ProjectBranchName (..), ProjectAndBranch (..))
-- >>> let service = ProjectBranchUI (ProjectAndBranch (UnsafeProjectName "base") (UnsafeProjectBranchName "main")) (Path.empty) (Just (TermReference (NameOnly (Name.unsafeFromText "List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/projects/base/main/latest/terms/List/map"
--
-- Project with definition but no perspective, contributor branch
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> import Unison.Core.Project (ProjectName (..), ProjectBranchName (..), ProjectAndBranch (..))
-- >>> let service = ProjectBranchUI (ProjectAndBranch (UnsafeProjectName "@unison/base") (UnsafeProjectBranchName "@runarorama/contribution")) (Path.empty) (Just (TermReference (NameOnly (Name.unsafeFromText "List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/projects/@unison/base/@runarorama/contribution/latest/terms/List/map"
--
-- Project with definition and perspective
-- >>> import qualified Unison.Syntax.Name as Name
-- >>> import Unison.Core.Project (ProjectName (..), ProjectBranchName (..), ProjectAndBranch (..))
-- >>> let service = ProjectBranchUI (ProjectAndBranch (UnsafeProjectName "@unison/base") (UnsafeProjectBranchName "@runarorama/contribution")) (Path.fromList ["data"]) (Just (TermReference (NameOnly (Name.unsafeFromText "List.map"))))
-- >>> let baseUrl = (BaseUrl{ urlHost = "http://localhost", urlToken = "asdf", urlPort = 1234 })
-- >>> urlFor service baseUrl
-- "http://localhost:1234/asdf/ui/projects/@unison/base/@runarorama/contribution/latest/namespaces/data/;/terms/List/map"
urlFor :: Service -> BaseUrl -> Text
urlFor service baseUrl =
  case service of
    LooseCodeUI perspective def ->
      tShow baseUrl <> "/" <> toUrlPath ([DontEscape "ui", DontEscape "non-project-code"] <> path (Path.unabsolute perspective) def)
    ProjectBranchUI (ProjectAndBranch projectName branchName) perspective def ->
      tShow baseUrl <> "/" <> toUrlPath ([DontEscape "ui", DontEscape "projects", DontEscape $ into @Text projectName, DontEscape $ into @Text branchName] <> path perspective def)
    Api -> tShow baseUrl <> "/" <> toUrlPath [DontEscape "api"]
  where
    path :: Path.Path -> Maybe DefinitionReference -> [URISegment]
    path ns def =
      let nsPath = namespacePath ns
       in case definitionPath def of
            Just defPath -> case nsPath of
              [] -> [DontEscape "latest"] <> defPath
              _ -> [DontEscape "latest"] <> nsPath <> [DontEscape ";"] <> defPath
            Nothing -> [DontEscape "latest"] <> nsPath

    namespacePath :: Path.Path -> [URISegment]
    namespacePath path =
      if path == Path.empty
        then []
        else [DontEscape "namespaces"] <> (EscapeMe . NameSegment.toEscapedText <$> Path.toList path)

    definitionPath :: Maybe DefinitionReference -> Maybe [URISegment]
    definitionPath def =
      toDefinitionPath <$> def

    toUrlPath :: [URISegment] -> Text
    toUrlPath path =
      path
        & fmap \case
          EscapeMe txt -> UriEncode.encodeText txt
          DontEscape txt -> txt
        & Text.intercalate "/"

    refToUrlText :: HashQualified Name -> [URISegment]
    refToUrlText r =
      case r of
        NameOnly n ->
          n & Name.segments & fmap (EscapeMe . NameSegment.toEscapedText) & toList
        HashOnly h ->
          [EscapeMe $ ShortHash.toText h]
        HashQualified n _ ->
          n & Name.segments & fmap (EscapeMe . NameSegment.toEscapedText) & toList

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
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  ServerT LooseCodeAPI (Backend IO)
serveLooseCode codebase rt =
  (\root rel name -> setCacheControl <$> NamespaceListing.serve codebase (Left <$> root) rel name)
    :<|> (\namespaceName mayRoot renderWidth -> setCacheControl <$> NamespaceDetails.namespaceDetails rt codebase namespaceName (Left <$> mayRoot) renderWidth)
    :<|> (\mayRoot relativePath rawHqns renderWidth suff -> setCacheControl <$> serveDefinitions rt codebase (Left <$> mayRoot) relativePath rawHqns renderWidth suff)
    :<|> (\mayRoot relativePath limit renderWidth query -> setCacheControl <$> serveFuzzyFind codebase (Left <$> mayRoot) relativePath limit renderWidth query)
    :<|> (\shortHash mayName mayRoot relativeTo renderWidth -> setCacheControl <$> serveTermSummary codebase shortHash mayName (Left <$> mayRoot) relativeTo renderWidth)
    :<|> (\shortHash mayName mayRoot relativeTo renderWidth -> setCacheControl <$> serveTypeSummary codebase shortHash mayName (Left <$> mayRoot) relativeTo renderWidth)

serveProjectsCodebaseServerAPI ::
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  ProjectName ->
  ProjectBranchName ->
  ServerT CodebaseServerAPI (Backend IO)
serveProjectsCodebaseServerAPI codebase rt projectName branchName = do
  namespaceListingEndpoint
    :<|> namespaceDetailsEndpoint
    :<|> serveDefinitionsEndpoint
    :<|> serveFuzzyFindEndpoint
    :<|> serveTermSummaryEndpoint
    :<|> serveTypeSummaryEndpoint
  where
    projectAndBranchName = ProjectAndBranch projectName branchName
    namespaceListingEndpoint _rootParam rel name = do
      root <- resolveProjectRoot codebase projectAndBranchName
      setCacheControl <$> NamespaceListing.serve codebase (Just . Right $ root) rel name
    namespaceDetailsEndpoint namespaceName _rootParam renderWidth = do
      root <- resolveProjectRoot codebase projectAndBranchName
      setCacheControl <$> NamespaceDetails.namespaceDetails rt codebase namespaceName (Just . Right $ root) renderWidth

    serveDefinitionsEndpoint _rootParam relativePath rawHqns renderWidth suff = do
      root <- resolveProjectRoot codebase projectAndBranchName
      setCacheControl <$> serveDefinitions rt codebase (Just . Right $ root) relativePath rawHqns renderWidth suff

    serveFuzzyFindEndpoint _rootParam relativePath limit renderWidth query = do
      root <- resolveProjectRoot codebase projectAndBranchName
      setCacheControl <$> serveFuzzyFind codebase (Just . Right $ root) relativePath limit renderWidth query

    serveTermSummaryEndpoint shortHash mayName _rootParam relativeTo renderWidth = do
      root <- resolveProjectRoot codebase projectAndBranchName
      setCacheControl <$> serveTermSummary codebase shortHash mayName (Just . Right $ root) relativeTo renderWidth

    serveTypeSummaryEndpoint shortHash mayName _rootParam relativeTo renderWidth = do
      root <- resolveProjectRoot codebase projectAndBranchName
      setCacheControl <$> serveTypeSummary codebase shortHash mayName (Just . Right $ root) relativeTo renderWidth

resolveProjectRoot :: (Codebase IO v a) -> (ProjectAndBranch ProjectName ProjectBranchName) -> Backend IO CausalHash
resolveProjectRoot codebase projectAndBranchName@(ProjectAndBranch projectName branchName) = do
  mayCH <- liftIO . Codebase.runTransaction codebase $ Backend.causalHashForProjectBranchName @IO projectAndBranchName
  case mayCH of
    Nothing -> throwError (Backend.ProjectBranchNameNotFound projectName branchName)
    Just ch -> pure ch

serveProjectDiffTermsEndpoint :: Codebase IO Symbol Ann -> Rt.Runtime Symbol -> ProjectName -> ProjectBranchName -> ProjectBranchName -> Name -> Name -> Backend IO TermDiffResponse
serveProjectDiffTermsEndpoint codebase rt projectName oldBranchRef newBranchRef oldTerm newTerm = do
  (oldPPED, oldNameSearch) <- contextForProjectBranch codebase projectName oldBranchRef
  (newPPED, newNameSearch) <- contextForProjectBranch codebase projectName newBranchRef
  oldTerm@TermDefinition {termDefinition = oldTermDispObject} <- Defn.termDefinitionByName codebase oldPPED oldNameSearch width rt oldTerm `whenNothingM` throwError (Backend.NoSuchDefinition (HQ.NameOnly oldTerm))
  newTerm@TermDefinition {termDefinition = newTermDisplayObj} <- Defn.termDefinitionByName codebase newPPED newNameSearch width rt newTerm `whenNothingM` throwError (Backend.NoSuchDefinition (HQ.NameOnly newTerm))
  let termDiffDisplayObject = DefinitionDiff.diffDisplayObjects oldTermDispObject newTermDisplayObj
  pure
    TermDiffResponse
      { project = projectName,
        oldBranch = oldBranchRef,
        newBranch = newBranchRef,
        oldTerm = oldTerm,
        newTerm = newTerm,
        diff = termDiffDisplayObject
      }
  where
    width = Pretty.Width 80

contextForProjectBranch :: (Codebase IO v a) -> ProjectName -> ProjectBranchName -> Backend IO (PrettyPrintEnvDecl, NameSearch Sqlite.Transaction)
contextForProjectBranch codebase projectName branchName = do
  projectRootHash <- resolveProjectRoot codebase (ProjectAndBranch projectName branchName)
  projectRootBranch <- liftIO $ Codebase.expectBranchForHash codebase projectRootHash
  hashLength <- liftIO $ Codebase.runTransaction codebase $ Codebase.hashLength
  let names = Branch.toNames (Branch.head projectRootBranch)
  let pped = PPED.makePPED (PPE.hqNamer hashLength names) (PPE.suffixifyByHash names)
  let nameSearch = Names.makeNameSearch hashLength names
  pure (pped, nameSearch)

serveProjectDiffTypesEndpoint :: Codebase IO Symbol Ann -> Rt.Runtime Symbol -> ProjectName -> ProjectBranchName -> ProjectBranchName -> Name -> Name -> Backend IO TypeDiffResponse
serveProjectDiffTypesEndpoint codebase rt projectName oldBranchRef newBranchRef oldType newType = do
  (oldPPED, oldNameSearch) <- contextForProjectBranch codebase projectName oldBranchRef
  (newPPED, newNameSearch) <- contextForProjectBranch codebase projectName newBranchRef
  oldType@TypeDefinition {typeDefinition = oldTypeDispObj} <- Defn.typeDefinitionByName codebase oldPPED oldNameSearch width rt oldType `whenNothingM` throwError (Backend.NoSuchDefinition (HQ.NameOnly oldType))
  newType@TypeDefinition {typeDefinition = newTypeDisplayObj} <- Defn.typeDefinitionByName codebase newPPED newNameSearch width rt newType `whenNothingM` throwError (Backend.NoSuchDefinition (HQ.NameOnly newType))
  let typeDiffDisplayObject = DefinitionDiff.diffDisplayObjects oldTypeDispObj newTypeDisplayObj
  pure
    TypeDiffResponse
      { project = projectName,
        oldBranch = oldBranchRef,
        newBranch = newBranchRef,
        oldType = oldType,
        newType = newType,
        diff = typeDiffDisplayObject
      }
  where
    width = Pretty.Width 80

serveProjectsAPI :: Codebase IO Symbol Ann -> Rt.Runtime Symbol -> ServerT ProjectsAPI (Backend IO)
serveProjectsAPI codebase rt =
  projectListingEndpoint codebase
    :<|> ( \projectName ->
             projectBranchListingEndpoint codebase projectName
               :<|> serveProjectsCodebaseServerAPI codebase rt projectName
               :<|> ( serveProjectDiffTermsEndpoint codebase rt projectName
                        :<|> serveProjectDiffTypesEndpoint codebase rt projectName
                    )
         )

serveUnisonLocal ::
  BackendEnv ->
  Codebase IO Symbol Ann ->
  Rt.Runtime Symbol ->
  Server UnisonLocalAPI
serveUnisonLocal env codebase rt =
  hoistServer (Proxy @UnisonLocalAPI) (backendHandler env) $
    serveProjectsAPI codebase rt :<|> serveLooseCode codebase rt :<|> (setCacheControl <$> ucmServer codebase)

backendHandler :: BackendEnv -> Backend IO a -> Handler a
backendHandler env m =
  Handler $ withExceptT backendError (runReaderT (runBackend m) env)
