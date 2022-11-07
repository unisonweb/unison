module Unison.Codebase.Editor.HandleInput.Execute
  ( evalUnisonFile,
    executeSymbolInFile,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (withArgs)
import qualified U.Util.Hash as Hash
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as Builtin
import qualified Unison.Builtin.Decls as DD
import Unison.Cli.Monad
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.NamesUtils (basicParseNames, getBasicPrettyPrintNames)
import qualified Unison.Cli.NamesUtils as NameUtils
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.MainTerm as MainTerm
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.HashQualified as HQ
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann (..))
import qualified Unison.Parser.Ann as Ann
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnv.Names as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Typechecker.TypeLookup as TypeLookup
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.Var as Var
import qualified Unison.WatchKind as WK

magicMainWatcherString :: String
magicMainWatcherString = "main"

-- | Run the term indicated by the provided symbol within the context of a given file and
-- return the results.
executeSymbolInFile ::
  Symbol ->
  Maybe (TypecheckedUnisonFile Symbol Ann) ->
  [String] ->
  Cli
    ( Term Symbol (), -- main result
      Type Symbol Ann, -- main result's type
      TypecheckedUnisonFile Symbol Ann -- The unison file which contains a watch expression for the requested symbol
    )
executeSymbolInFile mainSym mayUnisonFile exeArgs = do
  (unisonFile, mainResType) <- do
    (sym, term, typ, otyp) <- getTerm mainSym
    let wf = UF.createWatcherFile magicMainWatcherString mayUnisonFile sym term typ
    pure (wf, otyp)
  ppe <- NameUtils.executePPE unisonFile
  (_, xs) <- evalUnisonFile False ppe unisonFile exeArgs
  mainRes :: Term Symbol () <-
    let bonk (_, (_ann, watchKind, _id, _term0, term1, _isCacheHit)) = (watchKind, term1)
     in case lookup magicMainWatcherString (map bonk (Map.toList xs)) of
          Nothing ->
            error
              ( "impossible: we manually added the watcher "
                  <> show magicMainWatcherString
                  <> " with 'createWatcherFile', but it isn't here."
              )
          Just x -> pure (stripUnisonFileReferences unisonFile x)
  pure (mainRes, mainResType, unisonFile)

data GetTermResult
  = NoTermWithThatName
  | TermHasBadType (Type Symbol Ann)
  | GetTermSuccess (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)

-- | Look up runnable term with the given name in the codebase or
-- latest typechecked unison file. Return its symbol, term, type, and
-- the type of the evaluated term.
getTerm :: Symbol -> Cli (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)
getTerm main = do
  Cli.Env {codebase} <- ask
  let suffixifiedPPE ns = liftIO (Codebase.hashLength codebase) <&> (`PPE.fromSuffixNames` ns)
  getTerm' main >>= \case
    NoTermWithThatName -> do
      mainType <- Runtime.mainType <$> view #runtime
      basicPrettyPrintNames <- getBasicPrettyPrintNames
      ppe <- suffixifiedPPE (NamesWithHistory.NamesWithHistory basicPrettyPrintNames mempty)
      Cli.returnEarly $ NoMainFunction main ppe [mainType]
    TermHasBadType ty -> do
      mainType <- Runtime.mainType <$> view #runtime
      basicPrettyPrintNames <- getBasicPrettyPrintNames
      ppe <- suffixifiedPPE (NamesWithHistory.NamesWithHistory basicPrettyPrintNames mempty)
      Cli.returnEarly $ BadMainFunction main ty ppe [mainType]
    GetTermSuccess x -> pure x

getTerm' :: Symbol -> Cli GetTermResult
getTerm' mainName = do
  Cli.Env {codebase, runtime} <- ask
  let getFromCodebase = do
        parseNames <- basicParseNames
        let loadTypeOfTerm ref = liftIO (Codebase.getTypeOfTerm codebase ref)
        mainToFile runtime
          =<< MainTerm.getMainTerm loadTypeOfTerm parseNames (Var.nameStr mainName) (Runtime.mainType runtime)
        where
          mainToFile _runtime (MainTerm.NotAFunctionName _) = pure NoTermWithThatName
          mainToFile _runtime (MainTerm.NotFound _) = pure NoTermWithThatName
          mainToFile _runtime (MainTerm.BadType _ ty) = pure $ maybe NoTermWithThatName TermHasBadType ty
          mainToFile runtime (MainTerm.Success hq tm typ) =
            let v = Var.named (HQ.toText hq)
             in case getMainResultType typ runtime of
                  Nothing -> pure $ TermHasBadType typ
                  Just otyp -> pure (GetTermSuccess (v, tm, typ, otyp))
  Cli.getLatestTypecheckedFile >>= \case
    Nothing -> getFromCodebase
    Just uf -> do
      (GetTermSuccess <$> mainTermFromFile uf runtime mainName) `whenNothing` getFromCodebase

mainTermFromFile :: TypecheckedUnisonFile Symbol Ann -> Runtime.Runtime Symbol -> Symbol -> Maybe (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann)
mainTermFromFile uf runtime sym = do
  (v, tm, ty) <- UF.namedTermFromTypecheckedFile uf sym
  otyp <- getMainResultType ty runtime
  let runMain = DD.forceTerm a a (Term.var a v)
      v2 = Var.freshIn (Set.fromList [v]) v
      a = ABT.annotation tm
  pure (v2, runMain, ty, otyp)

-- | Assert that the type is a valid main type, if so, return the type of evaluating it.
getMainResultType :: Type Symbol Ann -> Runtime.Runtime Symbol -> Maybe (Type Symbol Ann)
getMainResultType ty runtime = do
  if Typechecker.fitsScheme ty (Runtime.mainType runtime)
    then Just (synthesizeForce ty)
    else Nothing

-- | synthesize the type of forcing a term
--
-- precondition: @fitsScheme typeOfFunc Runtime.mainType@ is satisfied
synthesizeForce :: Type Symbol Ann -> Type Symbol Ann
synthesizeForce typeOfFunc = do
  let term :: Term Symbol Ann
      term = Term.ref External ref
      ref = Reference.DerivedId (Reference.Id (Hash.fromByteString "deadbeef") 0)
      env =
        Typechecker.Env
          { Typechecker._ambientAbilities = [DD.exceptionType External, Type.builtinIO External],
            Typechecker._typeLookup = tl <> Builtin.typeLookup,
            Typechecker._termsByShortname = Map.empty
          }
      tl =
        TypeLookup.TypeLookup
          { TypeLookup.typeOfTerms = Map.singleton ref typeOfFunc,
            TypeLookup.dataDecls = Map.empty,
            TypeLookup.effectDecls = Map.empty
          }
  case Result.runResultT (Typechecker.synthesize env (DD.forceTerm External External term)) of
    Identity (Nothing, notes) ->
      error
        ( unlines
            [ "synthesizeForce fails although fitsScheme passed",
              "Input Type:",
              show typeOfFunc,
              "Notes:",
              show notes
            ]
        )
    Identity (Just typ, _) -> typ

-- Hack alert
--
-- After we evaluate a term all vars are transformed into references,
-- but we want to feed this result into 'slurpFile' which won't add
-- dependencies that are referenced by hash. The hacky solution for
-- now is to convert all references that match a variable defined
-- within the unison file to variable references. This is hacky both
-- because we needlessly flip-flopping between var and reference
-- representations, and because we might unexpectedly add a term from
-- the local file if it has the same hash as a term in the codebase.
stripUnisonFileReferences :: TypecheckedUnisonFile Symbol a -> Term Symbol () -> Term Symbol ()
stripUnisonFileReferences unisonFile term =
  let refMap :: Map Reference.Id Symbol
      refMap = Map.fromList . map (\(sym, (refId, _, _, _)) -> (refId, sym)) . Map.toList . UF.hashTermsId $ unisonFile
      alg () = \case
        ABT.Var x -> ABT.var x
        ABT.Cycle x -> ABT.cycle x
        ABT.Abs v x -> ABT.abs v x
        ABT.Tm t -> case t of
          Term.Ref ref
            | Just var <- (\k -> Map.lookup k refMap) =<< Reference.toId ref -> ABT.var var
          x -> ABT.tm x
   in ABT.cata alg term

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
  Bool ->
  PPE.PrettyPrintEnv ->
  TypecheckedUnisonFile Symbol Ann ->
  [String] ->
  Cli
    ( [(Symbol, Term Symbol ())],
      Map Symbol (Ann, WK.WatchKind, Reference.Id, Term Symbol (), Term Symbol (), Bool)
    )
evalUnisonFile sandbox ppe unisonFile args = do
  Cli.Env {codebase, runtime, sandboxedRuntime} <- ask
  let theRuntime = if sandbox then sandboxedRuntime else runtime

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.lookupWatchCache codebase ref
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  Cli.with_ (withArgs args) do
    rs@(_, map) <-
      Cli.ioE (Runtime.evaluateWatches (Codebase.toCodeLookup codebase) ppe watchCache theRuntime unisonFile) \err -> do
        Cli.returnEarly (EvaluationFailure err)
    for_ (Map.elems map) \(_loc, kind, hash, _src, value, isHit) ->
      when (not isHit) do
        let value' = Term.amap (\() -> Ann.External) value
        liftIO (Codebase.putWatch codebase kind hash value')
    pure rs
