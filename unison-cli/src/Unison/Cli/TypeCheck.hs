module Unison.Cli.TypeCheck
  ( typecheckFileWithTNDR,
    typecheckFileWithoutTNDR,
    typecheckTerm,
  )
where

import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.FileParsers (computeTypecheckingEnvironment, synthesizeFile)
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Result qualified as Result
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol (Symbol))
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile qualified as UF
import Unison.Var qualified as Var

typecheckFileWithTNDR ::
  Codebase IO Symbol Ann ->
  [Type Symbol Ann] ->
  Parser.ParsingEnv ->
  UF.UnisonFile Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (UF.TypecheckedUnisonFile Symbol Ann)
    )
typecheckFileWithTNDR codebase ambient parsingEnv unisonFile = do
  typecheckingEnvironment <-
    computeTypecheckingEnvironment
      ambient
      (Codebase.typeLookupForDependencies codebase)
      parsingEnv
      unisonFile
  Result.getResult (synthesizeFile typecheckingEnvironment unisonFile)

typecheckTerm ::
  Term Symbol Ann ->
  Cli
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Type Symbol Ann)
    )
typecheckTerm tm = do
  Cli.Env {codebase} <- ask
  let v = Symbol 0 (Var.Inference Var.Other)
  liftIO $
    fmap extract
      <$> Codebase.runTransaction codebase (typecheckFileWithoutTNDR codebase [] (UF.UnisonFileId mempty mempty [(v, External, tm)] mempty))
  where
    extract tuf
      | [[(_, _, _, ty)]] <- UF.topLevelComponents' tuf = ty
      | otherwise = error "internal error: typecheckTerm"

typecheckFileWithoutTNDR ::
  Codebase IO Symbol Ann ->
  [Type Symbol Ann] ->
  UF.UnisonFile Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (UF.TypecheckedUnisonFile Symbol Ann)
    )
typecheckFileWithoutTNDR codebase ambient file = do
  typeLookup <- Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  let env =
        Typechecker.Env
          { _ambientAbilities = ambient,
            _typeLookup = typeLookup,
            _termsByShortname = Map.empty
          }
  pure $ synthesizeFile env file
