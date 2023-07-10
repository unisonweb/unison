module Unison.Cli.TypeCheck
  ( typecheck,
    typecheckFileWithTNDR,
    typecheckFile,
    typecheckTerm,
  )
where

import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.FileParsers (parseAndSynthesizeFile, synthesizeFile, synthesizeFileWithTNDR)
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Parser.Ann (Ann (..))
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.Result qualified as Result
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol (Symbol))
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile qualified as UF
import Unison.Var qualified as Var

typecheck ::
  (MonadIO m) =>
  Codebase IO Symbol Ann ->
  IO Parser.UniqueName ->
  [Type Symbol Ann] ->
  NamesWithHistory ->
  Text ->
  (Text, [L.Token L.Lexeme]) ->
  m
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Either (UF.UnisonFile Symbol Ann) (UF.TypecheckedUnisonFile Symbol Ann))
    )
typecheck codebase generateUniqueName ambient names sourceName source = liftIO do
  uniqueName <- generateUniqueName
  (Codebase.runTransaction codebase . Result.getResult) do
    let parsingEnv = Parser.ParsingEnv uniqueName names
    unisonFile <-
      Result.fromParsing $
        Parsers.parseFile (Text.unpack sourceName) (Text.unpack (fst source)) parsingEnv
    typecheckedUnisonFile <-
      synthesizeFileWithTNDR ambient (Codebase.typeLookupForDependencies codebase) parsingEnv unisonFile
    parseAndSynthesizeFile
      ambient
      (Codebase.typeLookupForDependencies codebase)
      (Parser.ParsingEnv uniqueName names)
      (Text.unpack sourceName)
      (fst source)

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
typecheckFileWithTNDR codebase ambient parsingEnv unisonFile =
  Result.getResult do
    synthesizeFileWithTNDR
      ambient
      (Codebase.typeLookupForDependencies codebase)
      parsingEnv
      unisonFile

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
      <$> Codebase.runTransaction codebase (typecheckFile codebase [] (UF.UnisonFileId mempty mempty [(v, tm)] mempty))
  where
    extract tuf
      | [[(_, _, ty)]] <- UF.topLevelComponents' tuf = ty
      | otherwise = error "internal error: typecheckTerm"

typecheckFile ::
  Codebase IO Symbol Ann ->
  [Type Symbol Ann] ->
  UF.UnisonFile Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (UF.TypecheckedUnisonFile Symbol Ann)
    )
typecheckFile codebase ambient file = do
  typeLookup <- Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  let env =
        Typechecker.Env
          { _ambientAbilities = ambient,
            _typeLookup = typeLookup,
            _termsByShortname = Map.empty
          }
  pure $ synthesizeFile env file
