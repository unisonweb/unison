module Unison.Codebase.Editor.HandleInput.RuntimeUtils
  ( evalUnisonTerm,
    evalUnisonTermE,
    evalUnisonTermWithLatestFile,
    latestFileCodeLookup,
    evalPureUnison,
    displayDecompileErrors,
    displayResponse,
    selectRuntime,
    EvalMode (..),
    modeProfSpec,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Unison.ABT qualified as ABT
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.CodeLookup (CodeLookup)
import Unison.Codebase.CodeLookup.Util qualified as CL
import Unison.Codebase.Editor.Output
import Unison.Codebase.Execute qualified as Codebase
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.Runtime.Profile (ProfileSpec (..))
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime (Error)
import Unison.Runtime.Decompile (DecompError)
import Unison.Runtime.Interface (Runtime, renderDecompError)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Util.Pretty qualified as P
import Unison.WatchKind qualified as WK

data EvalMode = Sandboxed | Permissive ProfileSpec

selectRuntime :: EvalMode -> Cli (Runtime Symbol)
selectRuntime mode =
  ask <&> \Cli.Env {runtime, sandboxedRuntime} -> case mode of
    Permissive _ -> runtime
    Sandboxed -> sandboxedRuntime

modeProfSpec :: EvalMode -> ProfileSpec
modeProfSpec Sandboxed = NoProf
modeProfSpec (Permissive prof) = prof

displayDecompileErrors :: [DecompError] -> Cli ()
displayDecompileErrors =
  Cli.respond . Literal . msg . fmap (P.indentN 2 . P.indentN 2 . renderDecompError)
  where
    msg em = do
      P.lines $
        [ P.warnCallout "I had trouble decompiling some results.",
          "",
          "The following errors were encountered:"
        ]
          ++ em

-- | Evaluate a single closed definition.
evalUnisonTermE ::
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Either Error (Term Symbol Ann))
evalUnisonTermE mode ppe useCache tm = do
  Cli.Env {codebase} <- ask
  evalUnisonTermWithCodeLookupE (Codebase.codebaseToCodeLookup codebase) mode ppe useCache tm

-- | The code of the latest typechecked file, not added yet, then the
-- codebase's: as for `run`, the file's definitions come first.
latestFileCodeLookup :: Cli (CodeLookup Symbol IO Ann)
latestFileCodeLookup = do
  Cli.Env {codebase} <- ask
  file <- Cli.getLatestTypecheckedFile
  pure (foldMap CL.fromTypecheckedUnisonFile file <> Codebase.codebaseToCodeLookup codebase)

-- | Evaluate a single closed definition that can refer to the latest
-- typechecked file's definitions, not added yet. Never cached.
evalUnisonTermWithLatestFile ::
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Term Symbol Ann ->
  Cli (Term Symbol Ann)
evalUnisonTermWithLatestFile mode ppe tm = do
  codeLookup <- latestFileCodeLookup
  evalUnisonTermWithCodeLookupE codeLookup mode ppe False tm
    & onLeftM (Cli.returnEarly . EvaluationFailure id)

evalUnisonTermWithCodeLookupE ::
  CodeLookup Symbol IO Ann ->
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Either Error (Term Symbol Ann))
evalUnisonTermWithCodeLookupE codeLookup mode ppe useCache tm = do
  Cli.Env {codebase} <- ask
  theRuntime <- selectRuntime mode
  let prof = modeProfSpec mode

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  let cache = if useCache then watchCache else Runtime.noCache
  r <- liftIO (Runtime.evaluateTerm' codeLookup cache ppe prof theRuntime tm)
  when useCache do
    case r of
      Right (Runtime.DecompErrs errs, _)
        -- don't cache when there were errors
        | not $ null errs -> displayDecompileErrors errs
      Right (resp, tmr) -> do
        Cli.runTransaction do
          Codebase.putWatch
            WK.RegularWatch
            (Hashing.hashClosedTerm tm)
            (Term.amap (const Ann.External) tmr)
        displayResponse resp
      Left _ -> pure ()
  pure $ r <&> Term.amap (\() -> Ann.External) . snd

displayResponse :: Runtime.Response DecompError -> Cli ()
displayResponse (Runtime.DecompErrs errs)
  | not $ null errs = displayDecompileErrors errs
displayResponse (Runtime.Profile prof) = Cli.respond (Literal msg)
  where
    msg = P.lines ["Profile Results:", ""] <> prof
displayResponse _ = pure ()

-- | Evaluate a single closed definition.
evalUnisonTerm ::
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Term Symbol Ann)
evalUnisonTerm mode ppe useCache tm =
  evalUnisonTermE mode ppe useCache tm & onLeftM (Cli.returnEarly . EvaluationFailure id)

evalPureUnison ::
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Either Error (Term Symbol Ann))
evalPureUnison ppe useCache tm =
  evalUnisonTermE mode ppe useCache tm'
  where
    mode = Permissive NoProf
    tm' =
      Term.iff
        a
        (Term.apps' (Term.builtin a "validateSandboxed") [allow, Term.delay a tm])
        tm
        (Term.app a (Term.builtin a "bug") (Term.text a msg))
    a = ABT.annotation tm
    allow =
      Term.list
        a
        [ Term.termLink a (Referent.Ref (Reference.Builtin "Debug.toText")),
          Term.termLink a (Referent.Ref (Reference.Builtin "Value.value"))
        ]
    msg = "pure code can't perform I/O"
