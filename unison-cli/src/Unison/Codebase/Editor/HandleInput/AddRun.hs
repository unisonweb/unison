module Unison.Codebase.Editor.HandleInput.AddRun
  ( handleAddRun,
  )
where

import Control.Lens (use)
import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput.Update (doSlurpAdds)
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output (Output (NoLastRunResult, SaveTermNameConflict, SlurpOutput))
import Unison.Codebase.Editor.Slurp qualified as Slurp
import Unison.Codebase.Editor.SlurpResult qualified as SlurpResult
import Unison.CommandLine.InputPattern qualified as InputPattern
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.Name (Name)
import Unison.Parser.Ann (Ann (External))
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Syntax.Name qualified as Name
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF

handleAddRun :: Input -> Name -> Cli ()
handleAddRun input resultName = do
  let resultVar = Name.toVar resultName
  let resultSymbol = Name.toVar resultName
  (trm, typ, uf0) <-
    use #lastRunResult & onNothingM do
      Cli.returnEarly NoLastRunResult
  whenJust (Map.lookup resultSymbol (UF.hashTermsId uf0)) \_ -> do
    Cli.returnEarly (SaveTermNameConflict resultName)
  let uf =
        UF.typecheckedUnisonFile
          (UF.dataDeclarationsId' uf0)
          (UF.effectDeclarationsId' uf0)
          ([(resultSymbol, External, trm, typ)] : UF.topLevelComponents' uf0)
          (UF.watchComponents uf0)
  Cli.Env {codebase} <- ask
  currentNames <- Cli.currentNames
  let sr = Slurp.slurpFile uf resultVar currentNames
  let adds = SlurpResult.adds sr
  Cli.runTransaction . Codebase.addDefsToCodebase codebase . SlurpResult.filterUnisonFile sr $ uf
  let description = (Text.pack (InputPattern.patternName InputPatterns.saveExecuteResult) <> " " <> Name.toText resultName)
  pp <- Cli.getCurrentProjectPath
  Cli.stepAt description (pp, doSlurpAdds adds uf)
  let namesWithDefinitionsFromFile = UF.addNamesFromTypeCheckedUnisonFile uf currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 namesWithDefinitionsFromFile) (PPE.suffixifyByHash namesWithDefinitionsFromFile)
  Cli.respond $ SlurpOutput input pped.suffixifiedPPE sr
