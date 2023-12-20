module Unison.Codebase.Editor.HandleInput.ShowDefinition (showDefinitions) where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State qualified as State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
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
showDefinitions outputLoc ppe terms types misses = do
  Cli.Env {codebase} <- ask
  outputPath <- getOutputPath
  when (not (null types && null terms)) do
    -- We need an 'isTest' check in the output layer, so it can prepend "test>" to tests in a scratch file. Since we
    -- currently have the whole branch in memory, we just use that to make our predicate, but this could/should get this
    -- information from the database instead, once it's efficient to do so.
    testRefs <- Cli.runTransaction (Codebase.filterTermsByReferenceIdHavingType codebase (DD.testResultType mempty) (Map.keysSet terms & Set.mapMaybe Reference.toId))
    let isTest r = Set.member r testRefs

    Cli.respond $
      DisplayDefinitions
        DisplayDefinitionsOutput
          { isTest,
            outputFile = outputPath,
            prettyPrintEnv = ppe,
            terms,
            types
          }
  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  for_ outputPath \p -> do
    -- We set latestFile to be programmatically generated, if we
    -- are viewing these definitions to a file - this will skip the
    -- next update for that file (which will happen immediately)
    #latestFile ?= (p, True)
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
