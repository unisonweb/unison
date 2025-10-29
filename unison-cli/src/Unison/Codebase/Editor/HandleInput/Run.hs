module Unison.Codebase.Editor.HandleInput.Run
  ( handleRun,
  )
where

import Algebra.Graph.AdjacencyMap qualified as Graph
import Control.Lens ((.=), _1)
import Control.Monad.Except (Except)
import Control.Monad.Except qualified as Except
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.Internal qualified as Set.Internal
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as Builtin
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Load (EvalMode (..), evalUnisonFile)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.MainTerm qualified as MainTerm
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.Runtime.Profile (ProfileSpec (..))
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann (External))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference, TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup)
import Unison.Typechecker.TypeLookup qualified as TypeLookup
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, defnsAreEmpty, zipDefnsWith)
import Unison.Util.Defns qualified as Defns
import Unison.Util.Map qualified as Map
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Recursion (cata)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Var qualified as Var

handleRun :: ProfileSpec -> HQ.HashQualified Name -> [String] -> Cli ()
handleRun prof main args = do
  (unisonFile, mainResType, codebaseRef) <- do
    (sym, term, typ, otyp, codebaseRef) <- getTerm main
    uf <- createWatcherFile sym term typ
    pure (uf, otyp, codebaseRef)

  checkStale main codebaseRef

  names <- Cli.currentNames
  let namesWithFileDefinitions = UF.addNamesFromTypeCheckedUnisonFile unisonFile names
  let pped = PPED.makePPED (PPE.hqNamer 10 namesWithFileDefinitions) (PPE.suffixifyByHash namesWithFileDefinitions)
  let ppe = PPED.suffixifiedPPE pped
  let mode = Permissive prof

  (_, xs) <-
    evalUnisonFile mode ppe unisonFile args & onLeftM \err ->
      Cli.returnEarly (Output.EvaluationFailure id err)

  mainRes :: Term Symbol () <-
    case lookup magicMainWatcherString (map bonk (Map.toList xs)) of
      Nothing ->
        error
          ( "impossible: we manually added the watcher "
              <> show magicMainWatcherString
              <> " with 'createWatcherFile', but it isn't here."
          )
      Just x -> pure (stripUnisonFileReferences unisonFile x)
  #lastRunResult .= Just (Term.amap (\() -> External) mainRes, mainResType, unisonFile)
  Cli.respond (Output.RunResult ppe mainRes)
  where
    bonk (_, (_ann, watchKind, _id, _term0, term1, _isCacheHit)) =
      (watchKind, term1)

data GetTermResult
  = NoTermWithThatName
  | TermHasBadType (Type Symbol Ann)
  | GetTermSuccess (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann, Maybe TermReference)

-- | Look up runnable term with the given name in the codebase or
-- latest typechecked unison file. Return its symbol, term, type, and
-- the type of the evaluated term, and whether it was found in the codebase (Just ref) or file (Nothing)
getTerm :: HQ.HashQualified Name -> Cli (Symbol, Term Symbol Ann, Type Symbol Ann, Type Symbol Ann, Maybe TermReference)
getTerm main =
  getTerm' main >>= \case
    NoTermWithThatName -> do
      mainType <- Runtime.mainType <$> view #runtime
      names <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
      let suffixifiedPPE = PPED.suffixifiedPPE pped
      Cli.returnEarly $ Output.NoMainFunction main suffixifiedPPE [mainType]
    TermHasBadType ty -> do
      mainType <- Runtime.mainType <$> view #runtime
      names <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
      let suffixifiedPPE = PPED.suffixifiedPPE pped
      Cli.returnEarly $ Output.BadMainFunction "run" main ty suffixifiedPPE [mainType]
    GetTermSuccess x -> pure x

getTerm' :: HQ.HashQualified Name -> Cli GetTermResult
getTerm' mainName =
  let getFromCodebase = do
        Cli.Env {codebase, runtime} <- ask
        names <- Cli.currentNames
        let loadTypeOfTerm ref = Cli.runTransaction (Codebase.getTypeOfTerm codebase ref)
        mainToFile
          =<< MainTerm.getMainTerm loadTypeOfTerm names mainName (Runtime.mainType runtime)
        where
          mainToFile (MainTerm.NotFound _) = pure NoTermWithThatName
          mainToFile (MainTerm.BadType _ ty) = pure $ maybe NoTermWithThatName TermHasBadType ty
          mainToFile (MainTerm.Success hq ref tm typ) =
            let v = Var.named (HQ.toText hq)
             in checkType Nothing typ \otyp ->
                  pure (GetTermSuccess (v, tm, typ, otyp, Just ref))

      getFromFile uf = do
        let components = join $ UF.topLevelComponents uf
        -- __TODO__: We shouldn’t need to serialize mainName` for this check
        let mainComponent = filter ((\v -> Var.name v == HQ.toText mainName) . view _1) components
        case mainComponent of
          [(v, _, tm, ty)] ->
            checkType (Just uf) ty \otyp ->
              let runMain = DD.forceTerm a a (Term.var a v)
                  v2 = Var.freshIn (Set.fromList [v]) v
                  a = ABT.annotation tm
               in pure (GetTermSuccess (v2, runMain, ty, otyp, Nothing))
          _ -> getFromCodebase

      checkType :: Maybe (TypecheckedUnisonFile Symbol Ann) -> Type Symbol Ann -> (Type Symbol Ann -> Cli GetTermResult) -> Cli GetTermResult
      checkType mayTuf ty f = do
        Cli.Env {codebase, runtime} <- ask
        let ufDeps = maybe mempty UF.externalTypeDependencies mayTuf
        case Typechecker.fitsScheme ty (Runtime.mainType runtime) of
          True -> do
            tlCodebase <-
              Cli.runTransaction $
                Codebase.typeLookupForDependencies codebase Defns {terms = Set.empty, types = Type.dependencies ty <> ufDeps}
            let tlTuf = Monoid.fromMaybe (fmap UF.typecheckedToTypeLookup mayTuf)
            f $! synthesizeForce (tlTuf <> tlCodebase) ty
          False -> pure (TermHasBadType ty)
   in Cli.getLatestTypecheckedFile >>= \case
        Nothing -> getFromCodebase
        Just uf -> getFromFile uf

-- | Produce a typechecked unison file where the given term is the
-- only watcher, with the watch type set to 'magicMainWatcherString'.
createWatcherFile :: Symbol -> Term Symbol Ann -> Type Symbol Ann -> Cli (TypecheckedUnisonFile Symbol Ann)
createWatcherFile v tm typ =
  Cli.getLatestTypecheckedFile >>= \case
    Nothing -> pure (UF.typecheckedUnisonFile mempty mempty mempty [(magicMainWatcherString, [(v, External, tm, typ)])])
    Just uf ->
      let v2 = Var.freshIn (Set.fromList [v]) v
       in pure $
            UF.typecheckedUnisonFile
              (UF.dataDeclarationsId' uf)
              (UF.effectDeclarationsId' uf)
              (UF.topLevelComponents' uf)
              -- what about main's component? we have dropped them if they existed.
              [(magicMainWatcherString, [(v2, External, tm, typ)])]

-- | synthesize the type of forcing a term
--
-- precondition: @fitsScheme typeOfFunc Runtime.mainType@ is satisfied
synthesizeForce :: TypeLookup Symbol Ann -> Type Symbol Ann -> Type Symbol Ann
synthesizeForce tl typeOfFunc = do
  let term :: Term Symbol Ann
      term = Term.ref External ref
      ref = Reference.DerivedId (Reference.Id (Hash.fromByteString "deadbeef") 0)
      env =
        Typechecker.Env
          { ambientAbilities = [DD.exceptionType External, Type.builtinIO External],
            typeLookup = mempty {TypeLookup.typeOfTerms = Map.singleton ref typeOfFunc} <> tl,
            termsByShortname = Map.empty,
            freeNameToFuzzyTermsByShortName = Map.empty,
            topLevelComponents = Map.empty
          }
  case Result.runResultT
    ( Typechecker.synthesize
        PPE.empty
        Typechecker.PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled
        env
        (DD.forceTerm External External term)
    ) of
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
      refMap = Map.fromList . map (\(sym, (_, refId, _, _, _)) -> (refId, sym)) . Map.toList . UF.hashTermsId $ unisonFile
      alg (ABT.Term' _ () abt) = case abt of
        ABT.Var x -> ABT.var x
        ABT.Cycle x -> ABT.cycle x
        ABT.Abs v x -> ABT.abs v x
        ABT.Tm t -> case t of
          Term.Ref ref
            | Just var <- (\k -> Map.lookup k refMap) =<< Reference.toId ref -> ABT.var var
          x -> ABT.tm x
   in cata alg term

magicMainWatcherString :: String
magicMainWatcherString = "main"

checkStale :: HQ.HashQualified Name -> Maybe TermReference -> Cli ()
checkStale hqMain maybeCodebaseRef = do
  whenJust (HQ.asNameOnly hqMain) \main -> do
    whenJustM Cli.getLatestTypecheckedFile \unisonFile -> do
      namespace <- Cli.getCurrentBranch0

      let beingUpdated :: DefnsF Set TermReference TypeReference
          beingUpdated =
            keepBeingUpdated
              (bimap Relation.range Relation.range (Branch.deepDefns namespace))
              (UF.toDefnsIdsByName unisonFile)

      when (not (defnsAreEmpty beingUpdated)) do
        edges :: [Operations.DependencyEdge] <-
          Cli.runTransaction $
            Operations.transitiveDependentsGraphWithinScope
              Builtin.isBuiltinType
              (Branch.deepDefnsIds (Branch.deleteLibdeps namespace))
              beingUpdated

        let graph :: Graph.AdjacencyMap (Defn TermReference TypeReference)
            graph =
              List.foldl
                ( \acc edge ->
                    Graph.overlay acc case edge of
                      Operations.TermDependsOnTerm dependent dependency ->
                        Graph.edge (TermDefn (Reference.fromId dependent)) (TermDefn dependency)
                      Operations.TermDependsOnType dependent dependency ->
                        Graph.edge (TermDefn (Reference.fromId dependent)) (TypeDefn dependency)
                      Operations.TypeDependsOnType dependent dependency ->
                        Graph.edge (TypeDefn (Reference.fromId dependent)) (TypeDefn dependency)
                )
                Graph.empty
                edges

        let dependencies :: DefnsF Set TermReference TypeReference
            dependencies =
              case maybeCodebaseRef of
                Nothing ->
                  let (_, _, _, term, _) = Map.lookupJust (Name.toVar main) (UF.hashTermsId unisonFile)
                   in Term.dependencies term
                Just codebaseRef -> Defns.fromTerms (Set.singleton codebaseRef)

        whenLeft
          ( searchDependencyToBeingUpdated
              (Graph.adjacencyMap graph)
              (taggedDefns beingUpdated)
              (taggedDefns dependencies)
          )
          \path -> do
            let ppe =
                  (Branch.toPrettyPrintEnvDecl 10 namespace).suffixifiedPPE

            Cli.returnEarly (Output.StaleRun ppe main path (isNothing maybeCodebaseRef))

keepBeingUpdated ::
  DefnsF2 (Map Name) Set Referent TypeReference ->
  DefnsF (Map Name) TermReferenceId TypeReferenceId ->
  DefnsF Set TermReference TypeReference
keepBeingUpdated =
  zipDefnsWith (f (Set.mapMaybe Referent.toTermReference)) (f id)
  where
    f ::
      (Eq ref) =>
      (Set ref -> Set Reference) ->
      Map Name (Set ref) ->
      Map Name Reference.Id ->
      Set Reference
    f g namespace file =
      getConst $
        Map.mergeA
          Map.dropMissing
          Map.dropMissing
          ( Map.zipWithAMatched \_ codebaseRefs fileRefId ->
              Const (Set.delete (Reference.fromId fileRefId) (g codebaseRefs))
          )
          namespace
          file

searchDependencyToBeingUpdated ::
  Map (Defn TermReference TypeReference) (Set (Defn TermReference TypeReference)) ->
  Set (Defn TermReference TypeReference) ->
  Set (Defn TermReference TypeReference) ->
  Either [Defn TermReference TypeReference] ()
searchDependencyToBeingUpdated adjacency beingUpdated dependencies =
  case randomSetElem (Set.intersection dependencies beingUpdated) of
    Just ref -> Left [ref]
    Nothing ->
      Except.runExcept $
        State.evalStateT
          (searchDependencyToBeingUpdated1 adjacency beingUpdated [] (Set.toList dependencies))
          Set.empty

searchDependencyToBeingUpdated1 ::
  Map (Defn TermReference TypeReference) (Set (Defn TermReference TypeReference)) ->
  Set (Defn TermReference TypeReference) ->
  [Defn TermReference TypeReference] ->
  [Defn TermReference TypeReference] ->
  StateT (Set (Defn TermReference TypeReference)) (Except [Defn TermReference TypeReference]) ()
searchDependencyToBeingUpdated1 adjacency beingUpdated =
  search
  where
    search ::
      [Defn TermReference TypeReference] ->
      [Defn TermReference TypeReference] ->
      StateT (Set (Defn TermReference TypeReference)) (Except [Defn TermReference TypeReference]) ()
    search path = \case
      [] -> pure ()
      node : nodes -> do
        seen <- State.get
        if Set.member node seen
          then search path nodes
          else do
            let adjacent = Set.difference (Map.findWithDefault Set.empty node adjacency) seen
            case randomSetElem (Set.intersection adjacent beingUpdated) of
              Just ref -> Except.throwError (ref : node : path)
              Nothing -> do
                State.put $! Set.insert node seen
                search (node : path) (Set.toList adjacent)
                search path nodes

taggedDefns :: (Ord term, Ord typ) => DefnsF Set term typ -> Set (Defn term typ)
taggedDefns defns =
  Set.union (Set.mapMonotonic TermDefn defns.terms) (Set.mapMonotonic TypeDefn defns.types)

randomSetElem :: Set a -> Maybe a
randomSetElem = \case
  Set.Internal.Bin _ x _ _ -> Just x
  Set.Internal.Tip -> Nothing
