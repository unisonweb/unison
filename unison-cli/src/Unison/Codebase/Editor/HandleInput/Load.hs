module Unison.Codebase.Editor.HandleInput.Load
  ( handleLoad,
    loadUnisonFile,
    EvalMode (..),
    evalUnisonFile,
  )
where

import Control.Lens ((.=))
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict qualified as State
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Environment (withArgs)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput.RuntimeUtils qualified as RuntimeUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.Slurp qualified as Slurp
import Unison.Codebase.Runtime qualified as Runtime
import Unison.FileParsers qualified as FileParsers
import Unison.Names (Names)
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile.Names qualified as UF
import Unison.WatchKind qualified as WK

handleLoad :: Maybe FilePath -> Cli ()
handleLoad maybePath = do
  latestFile <- Cli.getLatestFile
  path <- (maybePath <|> fst <$> latestFile) & onNothing (Cli.returnEarly Output.NoUnisonFile)
  Cli.Env {loadSource} <- ask
  contents <-
    liftIO (loadSource (Text.pack path)) >>= \case
      Cli.InvalidSourceNameError -> Cli.returnEarly $ Output.InvalidSourceName path
      Cli.LoadError -> Cli.returnEarly $ Output.SourceLoadFailed path
      Cli.LoadSuccess contents -> pure contents
  loadUnisonFile (Text.pack path) contents

loadUnisonFile :: Text -> Text -> Cli ()
loadUnisonFile sourceName text = do
  Cli.respond $ Output.LoadingFile sourceName
  currentNames <- Cli.currentNames
  unisonFile <- withFile currentNames sourceName text
  let sr = Slurp.slurpFile unisonFile mempty Slurp.CheckOp currentNames
  let names = UF.addNamesFromTypeCheckedUnisonFile unisonFile currentNames
  pped <- Cli.prettyPrintEnvDeclFromNames names
  let ppe = PPE.suffixifiedPPE pped
  Cli.respond $ Output.Typechecked sourceName ppe sr unisonFile
  (bindings, e) <- evalUnisonFile Permissive ppe unisonFile []
  let e' = Map.map go e
      go (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
  when (not (null e')) do
    Cli.respond $ Output.Evaluated text ppe bindings e'
  #latestTypecheckedFile .= Just (Right unisonFile)
  where
    withFile ::
      Names ->
      Text ->
      Text ->
      Cli (TypecheckedUnisonFile Symbol Ann)
    withFile names sourceName text = do
      pp <- Cli.getCurrentProjectPath
      State.modify' \loopState ->
        loopState
          & #latestFile .~ Just (Text.unpack sourceName, False)
          & #latestTypecheckedFile .~ Nothing
      Cli.Env {codebase, generateUniqueName} <- ask
      uniqueName <- liftIO generateUniqueName
      let parsingEnv =
            Parser.ParsingEnv
              { uniqueNames = uniqueName,
                uniqueTypeGuid = Cli.loadUniqueTypeGuid pp,
                names
              }
      unisonFile <-
        Cli.runTransaction (Parsers.parseFile (Text.unpack sourceName) (Text.unpack text) parsingEnv)
          & onLeftM \err -> Cli.returnEarly (Output.ParseErrors text [err])
      -- set that the file at least parsed (but didn't typecheck)
      State.modify' (& #latestTypecheckedFile .~ Just (Left unisonFile))
      typecheckingEnv <-
        Cli.runTransaction do
          computeTypecheckingEnvironment (FileParsers.ShouldUseTndr'Yes parsingEnv) codebase [] unisonFile
      let Result.Result notes maybeTypecheckedUnisonFile = FileParsers.synthesizeFile typecheckingEnv unisonFile
      maybeTypecheckedUnisonFile & onNothing do
        let namesWithFileDefinitions = UF.addNamesFromUnisonFile unisonFile names
        pped <- Cli.prettyPrintEnvDeclFromNames namesWithFileDefinitions
        let suffixifiedPPE = PPED.suffixifiedPPE pped
        let tes = [err | Result.TypeError err <- toList notes]
            cbs =
              [ bug
                | Result.CompilerBug (Result.TypecheckerBug bug) <-
                    toList notes
              ]
        when (not (null tes)) do
          currentPath <- Cli.getCurrentPath
          Cli.respond (Output.TypeErrors currentPath text suffixifiedPPE tes)
        when (not (null cbs)) do
          Cli.respond (Output.CompilerBugs text suffixifiedPPE cbs)
        Cli.returnEarlyWithoutOutput

data EvalMode = Sandboxed | Permissive | Native

-- | Evaluate all watched expressions in a UnisonFile and return
-- their results, keyed by the name of the watch variable. The tuple returned
-- has the form:
--   (hash, (ann, sourceTerm, evaluatedTerm, isCacheHit))
--
-- where
--   `hash` is the hash of the original watch expression definition
--   `ann` gives the location of the watch expression
--   `sourceTerm` is a closed term (no free vars) for the watch expression
--   `evaluatedTerm` is the result of evaluating that `sourceTerm`
--   `isCacheHit` is True if the result was computed by just looking up
--   in a cache
--
-- It's expected that the user of this action might add the
-- `(hash, evaluatedTerm)` mapping to a cache to make future evaluations
-- of the same watches instantaneous.
evalUnisonFile ::
  EvalMode ->
  PPE.PrettyPrintEnv ->
  TypecheckedUnisonFile Symbol Ann ->
  [String] ->
  Cli
    ( [(Symbol, Term Symbol ())],
      Map Symbol (Ann, WK.WatchKind, Reference.Id, Term Symbol (), Term Symbol (), Bool)
    )
evalUnisonFile mode ppe unisonFile args = do
  Cli.Env {codebase, runtime, sandboxedRuntime, nativeRuntime} <- ask
  let theRuntime = case mode of
        Sandboxed -> sandboxedRuntime
        Permissive -> runtime
        Native -> nativeRuntime

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  Cli.with_ (withArgs args) do
    (nts, errs, map) <-
      Cli.ioE (Runtime.evaluateWatches (Codebase.toCodeLookup codebase) ppe watchCache theRuntime unisonFile) \err -> do
        Cli.returnEarly (Output.EvaluationFailure err)
    when (not $ null errs) (RuntimeUtils.displayDecompileErrors errs)
    for_ (Map.elems map) \(_loc, kind, hash, _src, value, isHit) -> do
      -- only update the watch cache when there are no errors
      when (not isHit && null errs) do
        let value' = Term.amap (\() -> Ann.External) value
        Cli.runTransaction (Codebase.putWatch kind hash value')
    pure (nts, map)
