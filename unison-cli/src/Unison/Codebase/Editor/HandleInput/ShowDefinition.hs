module Unison.Codebase.Editor.HandleInput.ShowDefinition
  ( handleShowDefinition,
    showDefinitions,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State qualified as State
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Editor.Input (OutputLocation (..), ShowDefinitionScope (..))
import Unison.Codebase.Editor.Output
import Unison.DataDeclaration (Decl)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Server.Backend qualified as Backend
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Set qualified as Set

-- | Handle a @ShowDefinitionI@ input command, i.e. `view` or `edit`.
handleShowDefinition :: OutputLocation -> ShowDefinitionScope -> List.NonEmpty (HQ.HashQualified Name) -> Cli ()
handleShowDefinition outputLoc showDefinitionScope query = do
  env <- ask

  let hasAbsoluteQuery = any (any Name.isAbsolute) query
  (names, unbiasedPPED) <- case (hasAbsoluteQuery, showDefinitionScope) of
    -- TODO: We should instead print each definition using the names from its project-branch root.
    (True, _) -> do
      root <- Cli.getCurrentProjectRoot
      let root0 = Branch.head root
      let names = Names.makeAbsolute (Branch.toNames root0)
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
      pure (names, pped)
    (_, ShowDefinitionGlobal) -> do
      -- TODO: Maybe rewrite to be properly global
      root <- Cli.getCurrentProjectRoot
      let root0 = Branch.head root
      let names = Names.makeAbsolute $ Branch.toNames root0
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
      pure (names, pped)
    (_, ShowDefinitionLocal) -> do
      currentNames <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 currentNames) (suffixify currentNames)
      pure (currentNames, pped)
  let pped = PPED.biasTo (mapMaybe HQ.toName (List.NonEmpty.toList query)) unbiasedPPED
  Backend.DefinitionResults terms types misses <- do
    let nameSearch = NameSearch.makeNameSearch 10 names
    Cli.runTransaction (Backend.definitionsByName env.codebase nameSearch includeCycles Names.IncludeSuffixes (toList query))
  showDefinitions outputLoc pped terms types misses
  where
    suffixify =
      case outputLoc of
        ConsoleLocation -> PPE.suffixifyByHash
        FileLocation _ _ -> PPE.suffixifyByHashName
        LatestFileLocation _ -> PPE.suffixifyByHashName

    -- `view`: don't include cycles; `edit`: include cycles
    includeCycles =
      case outputLoc of
        ConsoleLocation -> Backend.DontIncludeCycles
        FileLocation _ _ -> Backend.IncludeCycles
        LatestFileLocation _ -> Backend.IncludeCycles

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
      testRefs <- Cli.runTransaction (Codebase.filterTermsByReferenceIdHavingType codebase (DD.testResultListType mempty) (Map.keysSet terms & Set.mapMaybe Reference.toId))
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
        FileLocation path _fold -> pure (Just path)
        LatestFileLocation _fold -> do
          loopState <- State.get
          pure case loopState ^. #latestFile of
            Nothing -> Just "scratch.u"
            Just (path, _) -> Just path

    renderCodePretty pped isSourceFile isTest terms types =
      Pretty.syntaxToColor . Pretty.sep "\n\n" $
        Pretty.prettyTypeDisplayObjects pped types <> Pretty.prettyTermDisplayObjects pped isSourceFile isTest terms
