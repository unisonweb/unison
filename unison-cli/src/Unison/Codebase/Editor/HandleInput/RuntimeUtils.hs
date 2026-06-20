module Unison.Codebase.Editor.HandleInput.RuntimeUtils
  ( evalUnisonTerm,
    evalUnisonTermE,
    evalPureUnison,
    displayDecompileErrors,
    displayResponse,
    selectRuntime,
    applyMetaAction,
    EvalMode (..),
    modeProfSpec,
  )
where

import Data.IORef
import Data.Text qualified as Text

import Control.Lens
import Control.Monad.Reader (ask)
import Unison.ABT qualified as ABT
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Path.Parse qualified as Path.Parse
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
  theRuntime <- selectRuntime mode
  let prof = modeProfSpec mode

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  let cache = if useCache then watchCache else Runtime.noCache
      -- Wire Meta.store through to the SQLite codebase. The runtime
      -- decodes + typechecks + hashes the meta term and hands us the
      -- (Reference.Id, Term, Type) triple; we lift annotations from
      -- () to External and commit via Codebase.putTerm.
      metaPut :: Runtime.MetaPutTerm Symbol
      metaPut rid tmU tyU =
        Codebase.runTransaction codebase $
          Codebase.putTerm
            codebase
            rid
            (Term.amap (const Ann.External) tmU)
            (Ann.External <$ tyU)
  -- Mutating Meta.* builtins queue actions into this IORef during
  -- evaluation; the queue is drained and applied via Cli.stepAt
  -- after evaluation completes.
  pendingActions <- liftIO $ newIORef ([] :: [Runtime.MetaAction])
  let metaCb :: Runtime.MetaCallbacks Symbol
      metaCb =
        Runtime.MetaCallbacks
          { Runtime.metaPutTerm = metaPut,
            Runtime.metaAliasTerm = \ref name ->
              modifyIORef pendingActions (Runtime.MAliasTerm ref name :)
          }
  r <- liftIO (Runtime.evaluateTerm' (Codebase.codebaseToCodeLookup codebase) (Just metaCb) cache ppe prof theRuntime tm)
  -- Drain queued UCM-style actions in FIFO order before returning to
  -- the caller, so that e.g. @run (Meta.store … >>= alias "foo")@
  -- shows the new name visible afterwards.
  queued <- liftIO $ readIORef pendingActions
  for_ (reverse queued) applyMetaAction
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

-- | Apply one queued 'Runtime.MetaAction' to the current project
-- branch via the normal 'Cli.stepAt' machinery, so SQLite + LSP +
-- check-and-set behave exactly as if the user had typed the
-- equivalent UCM command.
applyMetaAction :: Runtime.MetaAction -> Cli ()
applyMetaAction = \case
  Runtime.MAliasTerm ref nameText ->
    case Path.Parse.parseSplit' (Text.unpack nameText) of
      Left err ->
        Cli.respond
          ( Literal
              ( P.warnCallout
                  ( "Meta.alias.term: couldn't parse destination name "
                      <> P.shown nameText
                      <> ": "
                      <> P.text err
                  )
              )
          )
      Right dest' -> do
        dest <- Cli.resolveSplit' dest'
        Cli.stepAt
          "Meta.alias.term"
          (BranchUtil.makeAddTermName dest (Referent.Ref ref))
