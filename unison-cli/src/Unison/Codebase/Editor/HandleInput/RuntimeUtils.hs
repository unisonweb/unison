module Unison.Codebase.Editor.HandleInput.RuntimeUtils
  ( evalUnisonTerm,
    evalUnisonTermE,
    evalPureUnison,
    displayDecompileErrors,
    displayResponse,
    selectRuntime,
    applyMetaAction,
    lookupTermInBranch,
    dependentsOfRef,
    EvalMode (..),
    modeProfSpec,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.IORef
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.ABT qualified as ABT
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output
import Unison.Codebase.Execute qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path.Parse
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.Runtime.Profile (ProfileSpec (..))
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Name qualified as Name
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime (Error)
import Unison.Runtime.Decompile (DecompError)
import Unison.Runtime.Interface (Runtime, renderDecompError)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Util.Pretty qualified as P
import Unison.Util.Relation qualified as Relation
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
  -- Capture a snapshot of the current namespace's terms so the
  -- read-only @Meta.lookup@ callback can resolve names without
  -- needing a Cli context. Queued aliases from the same evaluation
  -- aren't visible here (the snapshot is frozen at eval start).
  pp <- Cli.getCurrentProjectPath
  branch0 <- Cli.getBranch0FromProjectPath pp
  let metaCb :: Runtime.MetaCallbacks Symbol
      metaCb =
        Runtime.MetaCallbacks
          { Runtime.metaPutTerm = metaPut,
            Runtime.metaAliasTerm = \ref name ->
              modifyIORef pendingActions (Runtime.MAliasTerm ref name :),
            Runtime.metaAliasType = \ref name ->
              modifyIORef pendingActions (Runtime.MAliasType ref name :),
            Runtime.metaDeleteTerm = \name ->
              modifyIORef pendingActions (Runtime.MDeleteTerm name :),
            Runtime.metaMoveTerm = \old new ->
              modifyIORef pendingActions (Runtime.MMoveTerm old new :),
            Runtime.metaLookupTerm = lookupTermInBranch branch0,
            Runtime.metaDependents = dependentsOfRef codebase
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

-- | Resolve a path-style 'Text' name against a frozen branch
-- snapshot to a single 'Reference'. Returns 'Nothing' if the name
-- doesn't parse, isn't bound, is bound to a constructor (not a
-- regular term), or is ambiguous.
lookupTermInBranch ::
  Branch.Branch0 IO ->
  Text ->
  IO (Maybe Reference)
lookupTermInBranch branch nameText =
  case Path.Parse.parseSplit' (Text.unpack nameText) of
    Left _ -> pure Nothing
    Right dest' ->
      -- For lookup we don't care whether the user wrote a relative or
      -- absolute path — both collapse into a Path that BranchUtil.getTerm
      -- can search from the supplied branch root.
      let (path', seg) = dest'
          path = case path' of
            Path.AbsolutePath' (Path.Absolute p) -> p
            Path.RelativePath' p -> p
          referents = BranchUtil.getTerm (HQ'.NameOnly (path, seg)) branch
       in case Set.toList referents of
            [Referent.Ref r] -> pure (Just r)
            _ -> pure Nothing

-- | List of references that directly depend on the given term
-- reference.
dependentsOfRef :: Codebase.Codebase IO Symbol Ann -> Reference -> IO [Reference]
dependentsOfRef codebase ref = do
  deps <-
    Codebase.runTransaction codebase $
      Codebase.dependents Queries.ExcludeOwnComponent ref
  pure (Set.toList deps)

-- | Apply one queued 'Runtime.MetaAction' to the current project
-- branch via the normal 'Cli.stepAt' machinery, so SQLite + LSP +
-- check-and-set behave exactly as if the user had typed the
-- equivalent UCM command.
applyMetaAction :: Runtime.MetaAction -> Cli ()
applyMetaAction = \case
  Runtime.MAliasTerm ref nameText ->
    withParsedSplit "Meta.aliasTerm" nameText \dest ->
      Cli.stepAt
        "Meta.aliasTerm"
        (BranchUtil.makeAddTermName dest (Referent.Ref ref))
  Runtime.MAliasType ref nameText ->
    withParsedSplit "Meta.aliasType" nameText \dest -> do
      -- Like UCM's alias.type: also re-bind the type's constructors
      -- under the new name (e.g. aliasing Pair → Couple also gives
      -- you Couple.Pair). Without this the new type is unusable in
      -- constructor position.
      env <- ask
      -- Find the constructor referents and the names they were bound
      -- under in the source location. The constructor *names* (as
      -- distinct from anonymous "Constructor0" labels in the decl)
      -- come from the current namespace: we walk the project root,
      -- collect every (Referent.Con _ _, Name) pair for this type's
      -- constructors, then rebind them under the destination type.
      ctorSteps <- case Reference.toId ref of
        Nothing -> pure []
        Just typId -> do
          (declType, numCtors) <-
            Cli.runTransaction do
              (,)
                <$> Codebase.getDeclType env.codebase ref
                <*> Codebase.expectDeclNumConstructors env.codebase typId
          rootBranch0 <- Cli.getCurrentProjectRoot0
          let allTerms = Branch.deepTerms rootBranch0
              (destParentPP, destTypeSeg) = dest
              destTypeAbs =
                Path.descend (destParentPP ^. PP.absPath_) destTypeSeg
          pure
            [ ( ( destTypeAbs,
                  -- the constructor's last name segment (without the
                  -- enclosing type-name prefix)
                  case Name.reverseSegments name of
                    seg :| _ -> seg
                ),
                ctorReferent
              )
            | cid <- [0 .. fromIntegral numCtors - 1],
              let ctorReferent = Referent.Con (ConstructorReference ref cid) declType,
              name <-
                take 1 . List.sortOn Name.countSegments . Set.toList $
                  Relation.lookupDom ctorReferent allTerms
            ]
      pb <- Cli.getCurrentProjectBranch
      let destAbs :: Path.Split Path.Absolute
          destAbs = first (view PP.absPath_) dest
      Cli.stepManyAt
        pb
        "Meta.aliasType"
        ( BranchUtil.makeAddTypeName destAbs ref
            : map (\(s, r) -> BranchUtil.makeAddTermName s r) ctorSteps
        )
  Runtime.MDeleteTerm nameText ->
    withParsedSplit "Meta.deleteTerm" nameText \dest ->
      Cli.stepAt
        "Meta.deleteTerm"
        (BranchUtil.makeAnnihilateTermName dest)
  Runtime.MMoveTerm oldText newText ->
    withParsedSplit "Meta.moveTerm (source)" oldText \src ->
      withParsedSplit "Meta.moveTerm (destination)" newText \dest -> do
        -- Look up the source term so we know what to re-bind. Skip
        -- silently if missing or ambiguous — the macro asked us to
        -- move something that isn't here.
        srcReferents <- Cli.getTermsAt (HQ'.NameOnly src)
        case Set.toList srcReferents of
          [referent] -> do
            pb <- Cli.getCurrentProjectBranch
            Cli.stepManyAt
              pb
              "Meta.moveTerm"
              [ BranchUtil.makeDeleteTermName (first (view PP.absPath_) src) referent,
                BranchUtil.makeAddTermName (first (view PP.absPath_) dest) referent
              ]
          _ -> pure ()
  where
    withParsedSplit label nameText act =
      case Path.Parse.parseSplit' (Text.unpack nameText) of
        Left err ->
          Cli.respond
            ( Literal
                ( P.warnCallout
                    ( P.text label
                        <> ": couldn't parse name "
                        <> P.shown nameText
                        <> ": "
                        <> P.text err
                    )
                )
            )
        Right dest' -> do
          dest <- Cli.resolveSplit' dest'
          act dest
