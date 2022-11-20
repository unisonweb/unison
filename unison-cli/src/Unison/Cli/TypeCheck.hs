module Unison.Cli.TypeCheck
  ( typecheck,
    typecheckHelper,
    typecheckFile,
  )
where

import Control.Monad.Reader (ask)
import qualified Data.Text as Text
import qualified Unison.Builtin as Builtin
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.FileParsers (parseAndSynthesizeFile, synthesizeFile')
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.Result as Result
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Parser as Parser
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF

typecheck ::
  [Type Symbol Ann] ->
  NamesWithHistory ->
  Text ->
  (Text, [L.Token L.Lexeme]) ->
  Cli
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Either (UF.UnisonFile Symbol Ann) (UF.TypecheckedUnisonFile Symbol Ann))
    )
typecheck ambient names sourceName source =
  Cli.time "typecheck" do
    Cli.Env {codebase, generateUniqueName} <- ask
    uniqueName <- liftIO generateUniqueName
    (Cli.runTransaction . Result.getResult) $
      parseAndSynthesizeFile
        ambient
        (((<> Builtin.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
        (Parser.ParsingEnv uniqueName names)
        (Text.unpack sourceName)
        (fst source)

typecheckHelper ::
  MonadIO m =>
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
typecheckHelper codebase generateUniqueName ambient names sourceName source = do
  uniqueName <- liftIO generateUniqueName
  (liftIO . Codebase.runTransaction codebase . Result.getResult) $
    parseAndSynthesizeFile
      ambient
      (((<> Builtin.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
      (Parser.ParsingEnv uniqueName names)
      (Text.unpack sourceName)
      (fst source)

typecheckFile ::
  Codebase m Symbol Ann ->
  [Type Symbol Ann] ->
  UF.UnisonFile Symbol Ann ->
  Sqlite.Transaction
    ( Result.Result
        (Seq (Result.Note Symbol Ann))
        (Either Names (UF.TypecheckedUnisonFile Symbol Ann))
    )
typecheckFile codebase ambient file = do
  typeLookup <-
    (<> Builtin.typeLookup)
      <$> Codebase.typeLookupForDependencies codebase (UF.dependencies file)
  pure . fmap Right $ synthesizeFile' ambient typeLookup file
