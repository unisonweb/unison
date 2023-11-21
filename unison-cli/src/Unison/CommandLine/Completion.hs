{-
   This module defines tab-completion strategies for entering info via the CLI
-}
{-# LANGUAGE RecordWildCards #-}

module Unison.CommandLine.Completion
  ( -- * Completers
    exactComplete,
    prefixCompleteTermOrType,
    prefixCompleteTerm,
    prefixCompleteType,
    prefixCompletePatch,
    prefixCompleteUpgrade,
    noCompletions,
    prefixCompleteNamespace,
    -- Unused for now, but may be useful later
    prettyCompletion,
    fixupCompletion,
    haskelineTabComplete,
    sharePathCompletion,
  )
where

import Control.Lens (ifoldMap)
import Control.Lens qualified as Lens
import Control.Lens.Cons (unsnoc)
import Data.Aeson qualified as Aeson
import Data.List (isPrefixOf)
import Data.List qualified as List
import Data.List.Extra (nubOrdOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.URI qualified as URI
import System.Console.Haskeline qualified as Line
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.Completion qualified as Haskeline
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent qualified as Referent
import Unison.Auth.HTTPClient (AuthenticatedHttpClient (..))
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.CommandLine.InputPattern qualified as IP
import Unison.HashQualified' qualified as HQ'
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Server.Local.Endpoints.NamespaceListing (NamespaceListing (NamespaceListing))
import Unison.Server.Local.Endpoints.NamespaceListing qualified as Server
import Unison.Server.Types qualified as Server
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Types qualified as Share
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as P
import UnliftIO qualified
import Prelude hiding (readFile, writeFile)

-- | A completion func for use with Haskeline
haskelineTabComplete ::
  (MonadIO m) =>
  Map String IP.InputPattern ->
  Codebase m v a ->
  AuthenticatedHttpClient ->
  Path.Absolute ->
  Line.CompletionFunc m
haskelineTabComplete patterns codebase authedHTTPClient currentPath = Line.completeWordWithPrev Nothing " " $ \prev word ->
  -- User hasn't finished a command name, complete from command names
  if null prev
    then pure . exactComplete word $ Map.keys patterns
    else -- User has finished a command name; use completions for that command
    case words $ reverse prev of
      h : t -> fromMaybe (pure []) $ do
        p <- Map.lookup h patterns
        argType <- IP.argType p (length t)
        pure $ IP.suggestions argType word codebase authedHTTPClient currentPath
      _ -> pure []

-- | Things which we may want to complete for.
data CompletionType
  = NamespaceCompletion
  | TermCompletion
  | TypeCompletion
  | PatchCompletion
  | UpgradeCompletion
  deriving (Show, Eq, Ord)

-- | The empty completor.
noCompletions ::
  (MonadIO m) =>
  String ->
  Codebase m v a ->
  AuthenticatedHttpClient ->
  Path.Absolute ->
  m [System.Console.Haskeline.Completion.Completion]
noCompletions _ _ _ _ = pure []

-- | Finds names of the selected completion types within the path provided by the query.
--
-- Given a codebase with these terms:
--
-- @@
-- .base.List.map.doc
-- .base.List
-- .bar.foo
-- @@
--
-- We will return:
--
-- @@
-- .> cd bas<Tab>
-- base
--
-- .> cd base<Tab>
-- base
-- base.List
--
-- .> cd base.<Tab>
-- base.List
--
-- .> cd base.List.<Tab>
-- base.List.map
--
-- If conflicted, or if there's a # in the query, we expand completions into short-hashes.
-- This is also a convenient way to just see the shorthash for a given term.
--
-- .> view base.List.map#<Tab>
-- base.List.map#0q926sgnn6
completeWithinNamespace ::
  -- | The types of completions to return
  NESet CompletionType ->
  -- | The portion of this are that the user has already typed.
  String ->
  Path.Absolute ->
  Sqlite.Transaction [System.Console.Haskeline.Completion.Completion]
completeWithinNamespace compTypes query currentPath = do
  shortHashLen <- Codebase.hashLength
  b <- Codebase.getShallowBranchAtPath (Path.unabsolute absQueryPath) Nothing
  currentBranchSuggestions <- do
    nib <- namesInBranch shortHashLen b
    nib
      & fmap (\(isFinished, match) -> (isFinished, Text.unpack . Path.toText' $ queryPathPrefix Lens.:> NameSegment.NameSegment match))
      & filter (\(_isFinished, match) -> List.isPrefixOf query match)
      & fmap (\(isFinished, match) -> prettyCompletionWithQueryPrefix isFinished query match)
      & pure
  childSuggestions <- getChildSuggestions shortHashLen b
  let allSuggestions =
        currentBranchSuggestions
          -- Only show child suggestions when the current branch isn't ambiguous
          <> Monoid.whenM (length currentBranchSuggestions <= 1) childSuggestions
  pure . nubOrdOn Haskeline.replacement . List.sortOn Haskeline.replacement $ allSuggestions
  where
    queryPathPrefix :: Path.Path'
    querySuffix :: NameSegment.NameSegment
    (queryPathPrefix, querySuffix) = parseLaxPath'Query (Text.pack query)
    absQueryPath :: Path.Absolute
    absQueryPath = Path.resolve currentPath queryPathPrefix
    getChildSuggestions :: Int -> V2Branch.Branch Sqlite.Transaction -> Sqlite.Transaction [Completion]
    getChildSuggestions shortHashLen b = do
      nonEmptyChildren <- V2Branch.nonEmptyChildren b
      case querySuffix of
        "" -> pure []
        suffix -> do
          case Map.lookup suffix nonEmptyChildren of
            Nothing -> pure []
            Just childCausal -> do
              childBranch <- V2Causal.value childCausal
              nib <- namesInBranch shortHashLen childBranch
              nib
                & fmap
                  ( \(isFinished, match) -> (isFinished, Text.unpack . Path.toText' $ queryPathPrefix Lens.:> suffix Lens.:> NameSegment.NameSegment match)
                  )
                & filter (\(_isFinished, match) -> List.isPrefixOf query match)
                & fmap (\(isFinished, match) -> prettyCompletionWithQueryPrefix isFinished query match)
                & pure
    namesInBranch :: Int -> V2Branch.Branch Sqlite.Transaction -> Sqlite.Transaction [(Bool, Text)]
    namesInBranch hashLen b = do
      nonEmptyChildren <- V2Branch.nonEmptyChildren b
      let textifyHQ :: (NameSegment -> r -> HQ'.HashQualified NameSegment) -> Map NameSegment (Map r metadata) -> [(Bool, Text)]
          textifyHQ f xs =
            xs
              & hashQualifyCompletions f
              & fmap (HQ'.toTextWith NameSegment.toText)
              & fmap (True,)
      pure $
        ((False,) <$> dotifyNamespaces (fmap NameSegment.toText . Map.keys $ nonEmptyChildren))
          <> Monoid.whenM (NESet.member TermCompletion compTypes) (textifyHQ (hqFromNamedV2Referent hashLen) $ V2Branch.terms b)
          <> Monoid.whenM (NESet.member TypeCompletion compTypes) (textifyHQ (hqFromNamedV2Reference hashLen) $ V2Branch.types b)
          <> Monoid.whenM (NESet.member PatchCompletion compTypes) (fmap ((True,) . NameSegment.toText) . Map.keys $ V2Branch.patches b)

    -- Regrettably there'shqFromNamedV2Referencenot a great spot to combinators for V2 references and shorthashes right now.
    hqFromNamedV2Referent :: Int -> NameSegment -> Referent.Referent -> HQ'.HashQualified NameSegment
    hqFromNamedV2Referent hashLen n r = HQ'.HashQualified n (Cv.referent2toshorthash1 (Just hashLen) r)
    hqFromNamedV2Reference :: Int -> NameSegment -> Reference.Reference -> HQ'.HashQualified NameSegment
    hqFromNamedV2Reference hashLen n r = HQ'.HashQualified n (Cv.reference2toshorthash1 (Just hashLen) r)
    hashQualifyCompletions :: forall r metadata. (NameSegment -> r -> HQ'.HashQualified NameSegment) -> Map NameSegment (Map r metadata) -> [HQ'.HashQualified NameSegment]
    hashQualifyCompletions qualify defs = ifoldMap qualifyRefs defs
      where
        -- Qualify any conflicted definitions. If the query has a "#" in it, then qualify ALL
        -- completions.
        qualifyRefs :: NameSegment -> (Map r metadata) -> [HQ'.HashQualified NameSegment]
        qualifyRefs n refs
          | ((Text.isInfixOf "#" . NameSegment.toText) querySuffix) || length refs > 1 =
              refs
                & Map.keys
                <&> qualify n
          | otherwise = [HQ'.NameOnly n]

    -- If we're not completing namespaces, then all namespace completions should automatically
    -- drill-down by adding a trailing '.'
    dotifyNamespaces :: [Text] -> [Text]
    dotifyNamespaces namespaces =
      if not (NESet.member NamespaceCompletion compTypes)
        then fmap (<> ".") namespaces
        else namespaces

-- | A path parser which which is more lax with respect to well formed paths,
-- specifically we can determine a valid path prefix with a (possibly empty) suffix query.
-- This is used in tab-completion where the difference between `.base` and `.base.` is
-- relevant, but can't be detected when running something like 'Path.fromText''
--
-- >>> parseLaxPath'Query ".base."
-- (.base,"")
--
-- >>> parseLaxPath'Query ".base"
-- (.,"base")
--
-- >>> parseLaxPath'Query ".base.List"
-- (.base,"List")
--
-- >>> parseLaxPath'Query ""
-- (,"")
--
-- >>> parseLaxPath'Query "base"
-- (,"base")
--
-- >>> parseLaxPath'Query "base."
-- (base,"")
--
-- >>> parseLaxPath'Query "base.List"
-- (base,"List")
parseLaxPath'Query :: Text -> (Path.Path', NameSegment)
parseLaxPath'Query txt =
  case unsnoc (Text.splitOn "." txt) of
    -- This case is impossible due to the behaviour of 'splitOn'
    Nothing ->
      (Path.relativeEmpty', NameSegment "")
    -- ".base."
    -- ".base.List"
    Just ("" : pathPrefix, querySegment) -> (Path.AbsolutePath' . Path.Absolute . Path.fromList . fmap NameSegment $ pathPrefix, NameSegment querySegment)
    -- ""
    -- "base"
    -- "base.List"
    Just (pathPrefix, querySegment) ->
      ( Path.RelativePath' . Path.Relative . Path.fromList . fmap NameSegment $ pathPrefix,
        NameSegment querySegment
      )

-- | Completes a namespace argument by prefix-matching against the query.
prefixCompleteNamespace ::
  String ->
  Path.Absolute -> -- Current path
  Sqlite.Transaction [Line.Completion]
prefixCompleteNamespace = completeWithinNamespace (NESet.singleton NamespaceCompletion)

-- | Completes a term or type argument by prefix-matching against the query.
prefixCompleteTermOrType ::
  String ->
  Path.Absolute -> -- Current path
  Sqlite.Transaction [Line.Completion]
prefixCompleteTermOrType = completeWithinNamespace (NESet.fromList (TermCompletion NE.:| [TypeCompletion]))

-- | Completes a term argument by prefix-matching against the query.
prefixCompleteTerm ::
  String ->
  Path.Absolute -> -- Current path
  Sqlite.Transaction [Line.Completion]
prefixCompleteTerm = completeWithinNamespace (NESet.singleton TermCompletion)

-- | Completes a term or type argument by prefix-matching against the query.
prefixCompleteType ::
  String ->
  Path.Absolute -> -- Current path
  Sqlite.Transaction [Line.Completion]
prefixCompleteType = completeWithinNamespace (NESet.singleton TypeCompletion)

-- | Completes a patch argument by prefix-matching against the query.
prefixCompletePatch ::
  String ->
  Path.Absolute -> -- Current path
  Sqlite.Transaction [Line.Completion]
prefixCompletePatch = completeWithinNamespace (NESet.singleton PatchCompletion)

-- | Completes a uograde argument by prefix-matching against the query.
prefixCompleteUpgrade ::
  String ->
  Path.Absolute -> -- Current path
  Sqlite.Transaction [Line.Completion]
prefixCompleteUpgrade = completeWithinNamespace (NESet.singleton UpgradeCompletion)



-- | Renders a completion option with the prefix matching the query greyed out.
prettyCompletionWithQueryPrefix ::
  Bool ->
  -- | query
  String ->
  -- | completion
  String ->
  Line.Completion
prettyCompletionWithQueryPrefix endWithSpace query s =
  let coloredMatch = P.hiBlack (P.string query) <> P.string (drop (length query) s)
   in Line.Completion s (P.toAnsiUnbroken coloredMatch) endWithSpace

-- discards formatting in favor of better alignment
-- prettyCompletion (s, p) = Line.Completion s (P.toPlainUnbroken p) True
-- preserves formatting, but Haskeline doesn't know how to align
prettyCompletion :: Bool -> (String, P.Pretty P.ColorText) -> Line.Completion
prettyCompletion endWithSpace (s, p) = Line.Completion s (P.toAnsiUnbroken p) endWithSpace

-- | Constructs a list of 'Completion's from a query and completion options by
-- filtering them for prefix matches. A completion will be selected if it's an exact match for
-- a provided option.
exactComplete :: String -> [String] -> [Line.Completion]
exactComplete q ss = go <$> filter (isPrefixOf q) ss
  where
    go s = prettyCompletionWithQueryPrefix (s == q) q s

-- workaround for https://github.com/judah/haskeline/issues/100
-- if the common prefix of all the completions is smaller than
-- the query, we make all the replacements equal to the query,
-- which will preserve what the user has typed
fixupCompletion :: String -> [Line.Completion] -> [Line.Completion]
fixupCompletion _q [] = []
fixupCompletion _q [c] = [c]
fixupCompletion q cs@(h : t) =
  let commonPrefix (h1 : t1) (h2 : t2) | h1 == h2 = h1 : commonPrefix t1 t2
      commonPrefix _ _ = ""
      overallCommonPrefix =
        foldl commonPrefix (Line.replacement h) (Line.replacement <$> t)
   in if not (q `isPrefixOf` overallCommonPrefix)
        then [c {Line.replacement = q} | c <- cs]
        else cs

sharePathCompletion ::
  (MonadIO m) =>
  AuthenticatedHttpClient ->
  String ->
  m [Completion]
sharePathCompletion = shareCompletion (NESet.singleton NamespaceCompletion)

shareCompletion ::
  (MonadIO m) =>
  NESet CompletionType ->
  AuthenticatedHttpClient ->
  String ->
  m [Completion]
shareCompletion completionTypes authHTTPClient str =
  fromMaybe [] <$> runMaybeT do
    case Text.splitOn "." (Text.pack str) of
      [] -> empty
      [userPrefix] -> do
        userHandles <- searchUsers authHTTPClient userPrefix
        pure $
          userHandles
            & filter (userPrefix `Text.isPrefixOf`)
            <&> \handle -> prettyCompletionWithQueryPrefix False (Text.unpack userPrefix) (Text.unpack handle)
      userHandle : path -> do
        (userHandle, path, pathSuffix) <- case unsnoc path of
          Just (path, pathSuffix) -> pure (userHandle, Path.fromList (NameSegment <$> path), pathSuffix)
          Nothing -> pure (userHandle, Path.empty, "")
        NamespaceListing {namespaceListingChildren} <- MaybeT $ fetchShareNamespaceInfo authHTTPClient userHandle path
        namespaceListingChildren
          & fmap
            ( \case
                Server.Subnamespace nn ->
                  let name = Server.namespaceName nn
                   in (NamespaceCompletion, name)
                Server.TermObject nt ->
                  let name = HQ'.toTextWith NameSegment.toText $ Server.termName nt
                   in (NamespaceCompletion, name)
                Server.TypeObject nt ->
                  let name = HQ'.toTextWith NameSegment.toText $ Server.typeName nt
                   in (TermCompletion, name)
                Server.PatchObject np ->
                  let name = Server.patchName np
                   in (NamespaceCompletion, name)
            )
          & filter (\(typ, name) -> typ `NESet.member` completionTypes && pathSuffix `Text.isPrefixOf` name)
          & fmap
            ( \(_, name) ->
                let queryPath = userHandle : coerce (Path.toList path)
                    result = Text.unpack $ Text.intercalate "." (queryPath <> [name])
                 in prettyCompletionWithQueryPrefix False str result
            )
          & pure

fetchShareNamespaceInfo :: (MonadIO m) => AuthenticatedHttpClient -> Text -> Path.Path -> m (Maybe NamespaceListing)
fetchShareNamespaceInfo (AuthenticatedHttpClient httpManager) userHandle path = runMaybeT do
  let uri =
        (Share.codeserverToURI Codeserver.defaultCodeserver)
          { URI.uriPath = Text.unpack $ "/codebases/" <> userHandle <> "/browse",
            URI.uriQuery =
              if not . null $ Path.toList path
                then Text.unpack $ "?relativeTo=" <> tShow path
                else ""
          }
  req <- MaybeT $ pure (HTTP.requestFromURI uri)
  fullResp <- liftIO $ UnliftIO.tryAny $ HTTP.httpLbs req httpManager
  resp <- either (const empty) pure $ fullResp
  MaybeT . pure . Aeson.decode @Server.NamespaceListing $ HTTP.responseBody resp

searchUsers :: (MonadIO m) => AuthenticatedHttpClient -> Text -> m [Text]
searchUsers _ "" = pure []
searchUsers (AuthenticatedHttpClient httpManager) userHandlePrefix =
  fromMaybe [] <$> runMaybeT do
    let uri =
          (Share.codeserverToURI Codeserver.defaultCodeserver)
            { URI.uriPath = "/search",
              URI.uriQuery = Text.unpack $ "?query=" <> userHandlePrefix
            }
    req <- MaybeT $ pure (HTTP.requestFromURI uri)
    fullResp <- liftIO $ UnliftIO.tryAny $ HTTP.httpLbs req httpManager
    resp <- either (const empty) pure $ fullResp
    results <- (MaybeT . pure . Aeson.decode @[SearchResult] $ HTTP.responseBody resp)
    pure $
      results
        & filter (\SearchResult {tag} -> tag == "User")
        & fmap handle

data SearchResult = SearchResult
  { handle :: Text,
    tag :: Text
  }
  deriving (Show)

instance Aeson.FromJSON SearchResult where
  parseJSON = Aeson.withObject "SearchResult" \obj -> do
    handle <- obj Aeson..: "handle"
    tag <- obj Aeson..: "tag"
    pure $ SearchResult {..}
