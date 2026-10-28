-- | Handlers for the @share.*@ commands, which read from Unison Share and never touch the local codebase.
module Unison.Codebase.Editor.HandleInput.Share
  ( handleShareProjectSearch,
    handleShareSource,
    handleShareRender,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Network.HTTP.Types (status404)
import Servant.Client qualified as Servant
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Share.Definitions
import Unison.Cli.Share.Ref (ShareRef (..))
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Server.Doc.Markdown.Render qualified as Doc
import Unison.Server.Doc.Markdown.Types qualified as Markdown
import Unison.Server.Syntax qualified as Syntax
import Unison.Server.Types (DefinitionDisplayResults (..), TermDefinition (..), TypeDefinition (..))

handleShareProjectSearch :: Text -> Cli ()
handleShareProjectSearch query = do
  hits <- searchProjects query & onLeftM (Cli.returnEarly . Output.ServantClientError)
  case hits of
    [] -> Cli.respond (Output.ShareText ("No projects found matching " <> query))
    _ ->
      Cli.respond . Output.ShareText . Text.unlines $
        hits <&> \SearchHit {projectRef, summary} ->
          projectRef <> maybe "" (\s -> "  " <> Text.replace "\n" " " s) summary

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
      Cli.respond . Output.ShareText . Text.intercalate "\n\n---\n\n" $ docs <&> Markdown.toText . Doc.toMarkdown

-- | Fetch the definitions at the ref's path, failing if the ref has none or the name isn't found.
lookupDefinitions :: ShareRef -> Cli DefinitionDisplayResults
lookupDefinitions ref@ShareRef {path} = do
  name <- path & onNothing (Cli.returnEarly (Output.ShareFailed "Expected a name after the `:`, like @user/project:some.name"))
  results <- getDefinitions ref name & onLeftM \case
    Servant.FailureResponse _ response | Servant.responseStatusCode response == status404 -> Cli.returnEarly (Output.ShareFailed ("Not found on Share: " <> name))
    err -> Cli.returnEarly (Output.ServantClientError err)
  when (not (null (missingDefinitions results))) do
    Cli.returnEarly (Output.ShareFailed ("Not found on Share: " <> Text.intercalate ", " (missingDefinitions results)))
  pure results
