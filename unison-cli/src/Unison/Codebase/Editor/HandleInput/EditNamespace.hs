module Unison.Codebase.Editor.HandleInput.EditNamespace (handleEditNamespace) where

import Control.Monad.Reader
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils qualified as NamesUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.ShowDefinition (showDefinitions)
import Unison.Codebase.Editor.Input (OutputLocation (..))
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Server.Backend (NameScoping (Within))
import Unison.Server.Backend qualified as Backend
import Unison.Util.Monoid (foldMapM)

handleEditNamespace :: OutputLocation -> [Path] -> Cli ()
handleEditNamespace outputLoc inputPaths = do
  Cli.Env {codebase} <- ask
  currentBranch <- Cli.getCurrentBranch0
  ppe <- NamesUtils.currentPrettyPrintEnvDecl Within
  let paths =
        if null inputPaths
          then [Path.empty]
          else inputPaths
  let allNamesToEdit =
        (List.nubOrd paths) & foldMap \path ->
          let b = Branch.withoutLib $ Branch.getAt0 path currentBranch
              names = (Branch.toNames b)
              prefixedNames = case Path.toName path of
                Nothing -> names
                Just pathPrefix -> Names.prefix0 pathPrefix names
           in prefixedNames
  let termRefs = Names.termReferences allNamesToEdit
  -- We only need to (optionally) include cycles for type references, not term references,
  -- because 'update' is smart enough to patch-up cycles as expected for terms.
  let typeRefsWithoutCycles = Names.typeReferences allNamesToEdit
  typeRefs <- Cli.runTransaction $
    case includeCycles of
      Backend.IncludeCycles -> foldMapM Codebase.componentReferencesForReference typeRefsWithoutCycles
      Backend.DontIncludeCycles -> pure typeRefsWithoutCycles

  terms <-
    termRefs
      & foldMapM \ref ->
        Map.singleton ref <$> Backend.displayTerm codebase ref
          & Cli.runTransaction

  types <-
    typeRefs
      & foldMapM \ref ->
        Map.singleton ref <$> Backend.displayType codebase ref
          & Cli.runTransaction
  let misses = []
  showDefinitions outputLoc ppe terms types misses
  where
    -- `view`: don't include cycles; `edit`: include cycles
    includeCycles =
      case outputLoc of
        ConsoleLocation -> Backend.DontIncludeCycles
        FileLocation _ -> Backend.IncludeCycles
        LatestFileLocation -> Backend.IncludeCycles
