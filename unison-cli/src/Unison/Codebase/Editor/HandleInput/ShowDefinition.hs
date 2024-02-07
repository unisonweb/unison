module Unison.Codebase.Editor.HandleInput.ShowDefinition (showDefinitions) where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State qualified as State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Codebase qualified as Codebase
import Unison.Syntax.DisplayObject (DisplayObject)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.DataDeclaration (Decl)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Set qualified as Set

-- | Show the provided definitions to console or scratch file.
-- The caller is responsible for ensuring that the definitions include cycles if that's
-- the desired behavior.
showDefinitions ::
  OutputLocation ->
  PPED.PrettyPrintEnvDecl ->
  (Map Reference.Reference (DisplayObject (Type Symbol Ann) (Term Symbol Ann))) ->
  ( Map
      Reference.Reference
      (DisplayObject () (Decl Symbol Ann))
  ) ->
  [HQ.HashQualified Name] ->
  Cli ()
showDefinitions outputLoc pped terms types misses = do
  Cli.Env {codebase, writeSource} <- ask
  outputPath <- getOutputPath
  case outputPath of
    _ | null terms && null types -> pure ()
    Nothing -> do
      -- If we're writing to console we don't add test-watch syntax
      let isTest _ = False
      let isSourceFile = False
      -- No filepath, render code to console.
      let renderedCodePretty = renderCodePretty pped isSourceFile isTest terms types
      Cli.respond $ DisplayDefinitions renderedCodePretty
    Just fp -> do
      -- We build an 'isTest' check to prepend "test>" to tests in a scratch file.
      testRefs <- Cli.runTransaction (Codebase.filterTermsByReferenceIdHavingType codebase (DD.testResultType mempty) (Map.keysSet terms & Set.mapMaybe Reference.toId))
      let isTest r = Set.member r testRefs
      let isSourceFile = True
      let renderedCodePretty = renderCodePretty pped isSourceFile isTest terms types
      let renderedCodeText = Text.pack $ Pretty.toPlain 80 renderedCodePretty

      -- We set latestFile to be programmatically generated, if we
      -- are viewing these definitions to a file - this will skip the
      -- next update for that file (which will happen immediately)
      #latestFile ?= (fp, True)
      liftIO $ writeSource (Text.pack fp) renderedCodeText
      let numDefinitions = Map.size terms + Map.size types
      Cli.respond $ LoadedDefinitionsToSourceFile fp numDefinitions
  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  where
    -- Get the file path to send the definition(s) to. `Nothing` means the terminal.
    getOutputPath :: Cli (Maybe FilePath)
    getOutputPath =
      case outputLoc of
        ConsoleLocation -> pure Nothing
        FileLocation path -> pure (Just path)
        LatestFileLocation -> do
          loopState <- State.get
          pure case loopState ^. #latestFile of
            Nothing -> Just "scratch.u"
            Just (path, _) -> Just path

    renderCodePretty pped isSourceFile isTest terms types =
      Pretty.syntaxToColor . Pretty.sep "\n\n" $
        Pretty.prettyTypeDisplayObjects pped types <> Pretty.prettyTermDisplayObjects pped isSourceFile isTest terms
