module Unison.Cli.TypeCheck
  ( computeTypecheckingEnvironment,
  )
where

import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.FileParsers qualified as FileParsers
import Unison.Parser.Ann (Ann (..))
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile (UnisonFile)

computeTypecheckingEnvironment ::
  FileParsers.ShouldUseTndr Sqlite.Transaction ->
  Codebase IO Symbol Ann ->
  [Type Symbol Ann] ->
  UnisonFile Symbol Ann ->
  Sqlite.Transaction (Typechecker.Env Symbol Ann)
computeTypecheckingEnvironment shouldUseTndr codebase ambientAbilities unisonFile =
  FileParsers.computeTypecheckingEnvironment
    shouldUseTndr
    ambientAbilities
    (Codebase.typeLookupForDependencies codebase)
    unisonFile
