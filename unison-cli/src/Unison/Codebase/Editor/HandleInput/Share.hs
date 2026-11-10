-- | Handlers for the @share.*@ commands, which read from Unison Share and never touch the local codebase.
module Unison.Codebase.Editor.HandleInput.Share
  ( handleShareProjectSearch,
    handleShareSearch,
    handleShareSource,
    handleShareRender,
    handleShareReleases,
    handleShareDocs,
    handleShareReleaseDetail,
  )
where

import Data.List (intersperse)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Network.HTTP.Types (status404)
import Servant.Client qualified as Servant
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Share.Definitions
import Unison.Cli.Share.Ref (SearchFilter, ShareRef (..))
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Prelude
import Unison.Server.Doc.Console (docToConsole)
import Unison.Server.Doc.Markdown.Render qualified as Doc
import Unison.Server.Doc.Markdown.Types qualified as Markdown
import Unison.Server.Syntax qualified as Syntax
import Unison.Server.Types
  ( DefinitionDisplayResults (..),
    DefinitionSearchResult (..),
    TermDefinition (..),
    TermOrTypeSummary (..),
    TermSummary (..),
    TypeDefinition (..),
    TypeSummary (..),
  )
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty qualified as P

handleShareProjectSearch :: Text -> Cli ()
handleShareProjectSearch = searchEntities'

-- | Like the search box on the front page of Share: a query that starts with an @\@@ looks for users, organizations
-- and projects, and anything else looks for terms and types, by name or by type.
handleShareSearch :: Text -> Maybe SearchFilter -> Cli ()
handleShareSearch query filter_
  | "@" `Text.isPrefixOf` query = searchEntities' query
  | otherwise = do
      results <- searchDefinitions query filter_ & onLeftM (Cli.returnEarly . Output.ServantClientError)
      case results of
        [] -> Cli.respond (Output.ShareText ("No definitions found matching " <> query))
        firstResult : _ -> do
          Cli.setNumberedArgs (SA.ShareDefinitionRef . reference <$> results)
          Cli.respond (Output.ShareDefinitionMatches (renderMatch <$> results) (reference firstResult) (length results >= maxMatches))
  where
    maxMatches = 20

    origin :: DefinitionSearchResult -> Text
    origin DefinitionSearchResult {project, branchRef} = into @Text project <> "/" <> into @Text branchRef

    reference :: DefinitionSearchResult -> Text
    reference result@DefinitionSearchResult {fqn} = origin result <> ":" <> Name.toText fqn

    -- The name in its project and its type, like @data.List.take : Nat -> [a] -> [a]@ (with @builtin@ in front of a
    -- builtin), and where it is from.
    renderMatch :: DefinitionSearchResult -> Output.ShareMatch
    renderMatch result@DefinitionSearchResult {fqn, summary} =
      Output.ShareMatch {heading = prefix <> Name.toText fqn <> typeOf, origin = origin result}
      where
        (prefix, typeOf) = case summary of
          ToTTermSummary TermSummary {summary = displayed} -> (builtinPrefix displayed, " : " <> displayText displayed)
          ToTTypeSummary TypeSummary {summary = displayed} -> (builtinPrefix displayed <> "type ", "")
        builtinPrefix = \case
          BuiltinObject _ -> "builtin "
          _ -> ""
        displayText = \case
          UserObject text -> Syntax.toPlain text
          BuiltinObject text -> Syntax.toPlain text
          MissingObject hash -> "<missing definition " <> tShow hash <> ">"

searchEntities' :: Text -> Cli ()
searchEntities' query = do
  hits <- searchEntities query & onLeftM (Cli.returnEarly . Output.ServantClientError)
  case hits of
    [] -> Cli.respond (Output.ShareText ("Nothing found matching " <> query))
    _ ->
      Cli.respond . Output.ShareText . Text.unlines $
        hits <&> \case
          ProjectHit ref summary -> ref <> describe summary
          UserHit handle name -> handle <> "  (user)" <> describe name
          OrgHit handle name -> handle <> "  (organization)" <> describe name
  where
    describe = maybe "" \s -> "  " <> Text.replace "\n" " " s

-- | Print the source of the definition at @path@.
handleShareSource :: ShareRef -> Cli ()
handleShareSource ref = do
  DefinitionDisplayResults {termDefinitions, typeDefinitions} <- lookupDefinitions ref
  let terms = Map.elems termDefinitions <&> \TermDefinition {termDefinition} -> displayObject termDefinition
  let types = Map.elems typeDefinitions <&> \TypeDefinition {typeDefinition} -> displayObject typeDefinition
  Cli.respond (Output.ShareText (Text.intercalate "\n" (types ++ terms)))
  where
    displayObject = \case
      UserObject source -> Syntax.toPlain source
      BuiltinObject builtin -> Syntax.toPlain builtin <> " (builtin)"
      MissingObject hash -> "<missing definition " <> tShow hash <> ">"

-- | Render the documentation of the definition at @path@ (a doc renders itself), or the project's README if there
-- is no path.
handleShareRender :: ShareRef -> Cli ()
handleShareRender ref@ShareRef {path} =
  case path of
    Nothing -> do
      readme <- getReadme ref & onLeftM (Cli.returnEarly . Output.ServantClientError)
      Cli.respond (Output.ShareText readme)
    Just name -> do
      DefinitionDisplayResults {termDefinitions, typeDefinitions} <- lookupDefinitions ref
      let docs =
            [doc | TermDefinition {termDocs} <- Map.elems termDefinitions, (_, _, doc) <- termDocs]
              ++ [doc | TypeDefinition {typeDocs} <- Map.elems typeDefinitions, (_, _, doc) <- typeDocs]
      when (null docs) do
        Cli.returnEarly (Output.ShareFailed ("There are no docs for " <> name))
      -- The same layout as @docs@ and @render@ give for a local definition, not Share's Markdown.
      Cli.respond (Output.ShareDocs (P.linesSpaced (intersperse "---" (docs <&> docToConsole))))

-- | Fetch the definitions at the ref's path, failing if the ref has none or the name isn't found.
lookupDefinitions :: ShareRef -> Cli DefinitionDisplayResults
lookupDefinitions ref@ShareRef {path} = do
  name <- path & onNothing (Cli.returnEarly (Output.ShareFailed "Expected a name after the `:`, like @user/project:some.name"))
  results <-
    getDefinitions ref name & onLeftM \case
      Servant.FailureResponse _ response | Servant.responseStatusCode response == status404 -> Cli.returnEarly (Output.ShareFailed ("Not found on Share: " <> name))
      err -> Cli.returnEarly (Output.ServantClientError err)
  when (not (null (missingDefinitions results))) do
    Cli.returnEarly (Output.ShareFailed ("Not found on Share: " <> Text.intercalate ", " (missingDefinitions results)))
  pure results

-- | List the releases of a project, newest first, numbered so that @share.releases.detail 2@ can refer to one.
handleShareReleases :: Text -> Text -> Cli ()
handleShareReleases handle slug = do
  releases <-
    listReleases handle slug & onLeftM \case
      Servant.FailureResponse _ response | Servant.responseStatusCode response == status404 -> Cli.returnEarly (Output.ShareFailed ("There is no project @" <> handle <> "/" <> slug <> " on Share."))
      err -> Cli.returnEarly (Output.ServantClientError err)
  case releases of
    [] -> Cli.respond (Output.ShareText ("@" <> handle <> "/" <> slug <> " has no releases."))
    _ -> do
      let refs = releases <&> \ReleaseInfo {version} -> "@" <> handle <> "/" <> slug <> "/releases/" <> version
      Cli.setNumberedArgs (SA.ShareReleaseRef <$> refs)
      let width = Text.length (tShow (length releases))
      Cli.respond . Output.ShareText . Text.unlines $
        [ Text.justifyRight width ' ' (tShow n) <> ". " <> version <> "  " <> date <> (if status == "published" then "" else "  (" <> status <> ")")
        | (n, ReleaseInfo {version, date, status}) <- zip [1 :: Int ..] releases
        ]
          ++ [ "",
               "Use `share.releases.detail <number>` (or `share.releases.detail @" <> handle <> "/" <> slug <> "/releases/<version>`) to read the release notes of one."
             ]

-- | Print the release notes (the doc named @ReleaseNotes@) of a release.
handleShareReleaseDetail :: ShareRef -> Cli ()
handleShareReleaseDetail ref = do
  when (isJust ref.path) do
    Cli.returnEarly (Output.ShareFailed "Expected a release, like @user/project/releases/1.2.3")
  results <-
    getDefinitions ref "ReleaseNotes" & onLeftM \case
      Servant.FailureResponse _ response | Servant.responseStatusCode response == status404 -> Cli.returnEarly (Output.ShareFailed "That release was not found on Share.")
      err -> Cli.returnEarly (Output.ServantClientError err)
  let DefinitionDisplayResults {termDefinitions, typeDefinitions} = results
      docs =
        [doc | TermDefinition {termDocs} <- Map.elems termDefinitions, (_, _, doc) <- termDocs]
          ++ [doc | TypeDefinition {typeDocs} <- Map.elems typeDefinitions, (_, _, doc) <- typeDocs]
  if null docs
    then Cli.respond (Output.ShareText "This release has no ReleaseNotes.")
    else Cli.respond . Output.ShareText . Text.intercalate "\n\n---\n\n" $ docs <&> Markdown.toText . Doc.toMarkdown

-- | Print the type signature (or the declaration, for a type) and then the rendered documentation of a definition.
handleShareDocs :: ShareRef -> Cli ()
handleShareDocs ref = do
  DefinitionDisplayResults {termDefinitions, typeDefinitions} <- lookupDefinitions ref
  let render header docs =
        P.linesSpaced (P.text header : if null docs then ["(This definition has no documentation.)"] else docs <&> docToConsole)
      terms =
        Map.elems termDefinitions <&> \TermDefinition {bestTermName, signature, termDocs} ->
          render (bestTermName <> " : " <> Syntax.toPlain signature) [doc | (_, _, doc) <- termDocs]
      types =
        Map.elems typeDefinitions <&> \TypeDefinition {bestTypeName, typeDefinition, typeDocs} ->
          let declaration = case typeDefinition of
                UserObject source -> Text.unlines (take 1 (Text.lines (Syntax.toPlain source)))
                BuiltinObject _ -> "builtin type " <> bestTypeName
                MissingObject hash -> "<missing definition " <> tShow hash <> ">"
           in render (Text.strip declaration) [doc | (_, _, doc) <- typeDocs]
  Cli.respond (Output.ShareDocs (P.linesSpaced (intersperse "---" (types ++ terms))))
