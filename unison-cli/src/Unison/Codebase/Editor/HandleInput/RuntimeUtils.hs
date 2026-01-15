module Unison.Codebase.Editor.HandleInput.RuntimeUtils
  ( evalUnisonTerm,
    evalUnisonTermE,
    evalPureUnison,
    displayDecompileErrors,
    displayResult,
    displayResponse,
    selectRuntime,
    EvalMode (..),
    modeProfSpec,
  )
where

import Control.Lens
import Control.Monad.Reader (MonadReader, ask)
import Unison.ABT qualified as ABT
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
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

selectRuntime :: (MonadReader Cli.Env m) => EvalMode -> m (Runtime Symbol)
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
  (MonadReader Cli.Env m, MonadIO m) =>
  ([DecompError] -> m ()) ->
  ((P.Pretty P.ColorText) -> m ()) ->
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  m (Either Error (Term Symbol Ann))
evalUnisonTermE displayDecompileErrors displayResult mode ppe useCache tm = do
  Cli.Env {codebase} <- ask
  theRuntime <- selectRuntime mode
  let prof = modeProfSpec mode

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  let cache = if useCache then watchCache else Runtime.noCache
  r <- liftIO (Runtime.evaluateTerm' (Codebase.codebaseToCodeLookup codebase) cache ppe prof theRuntime tm)
  when useCache do
    case r of
      Right (Runtime.DecompErrs errs, _)
        -- don't cache when there were errors
        | not $ null errs -> displayDecompileErrors errs
      Right (resp, tmr) -> do
        liftIO $ Codebase.runTransaction codebase $ do
          Codebase.putWatch
            WK.RegularWatch
            (Hashing.hashClosedTerm tm)
            (Term.amap (const Ann.External) tmr)
        displayResponse displayDecompileErrors displayResult resp
      Left _ -> pure ()
  pure $ r <&> Term.amap (\() -> Ann.External) . snd

displayResponse :: (Applicative m) => ([DecompError] -> m ()) -> (P.Pretty P.ColorText -> m ()) -> Runtime.Response DecompError -> m ()
displayResponse displayDecompileErrors _displayResult (Runtime.DecompErrs errs)
  | not $ null errs = displayDecompileErrors errs
displayResponse _displayDecompileErrors displayResult (Runtime.Profile prof) = displayResult msg
  where
    msg = P.lines ["Profile Results:", ""] <> prof
displayResponse _ _ _ = pure ()

displayResult :: P.Pretty P.ColorText -> Cli ()
displayResult msg = Cli.respond (Literal msg)

-- | Evaluate a single closed definition.
evalUnisonTerm ::
  ([DecompError] -> Cli ()) ->
  (P.Pretty P.ColorText -> Cli ()) ->
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Term Symbol Ann)
evalUnisonTerm displayDecompileErrors displayResult mode ppe useCache tm =
  evalUnisonTermE displayDecompileErrors displayResult mode ppe useCache tm & onLeftM (Cli.returnEarly . EvaluationFailure id)

evalPureUnison ::
  ([DecompError] -> Cli ()) ->
  (P.Pretty P.ColorText -> Cli ()) ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Either Error (Term Symbol Ann))
evalPureUnison displayDecompileErrors displayResult ppe useCache tm =
  evalUnisonTermE displayDecompileErrors displayResult mode ppe useCache tm'
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
