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
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import System.Environment (withArgs)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.RuntimeUtils (EvalMode (..))
import Unison.Codebase.Editor.HandleInput.RuntimeUtils qualified as RuntimeUtils
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.SlurpResult (SlurpEntry (..), TermSlurp (..))
import Unison.Codebase.Execute qualified as Codebase
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.Runtime qualified as Runtime
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.DataDeclaration (DeclOrBuiltin)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DataDeclaration qualified as DeclOrBuiltin (DeclOrBuiltin (..))
import Unison.FileParsers qualified as FileParsers
import Unison.Name (Name)
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Defns (Defns (..))
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Timing qualified as Timing
import Unison.Var qualified as Var
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
  env <- ask

  Cli.respond $ Output.LoadingFile sourceName
  oldBranch0 <- Cli.getCurrentBranch0
  let oldNames = Branch.toNames oldBranch0
  unisonFile <- parseAndTypecheckUnisonFile oldNames sourceName text
  let unisonFileNames = UF.typecheckedToNames unisonFile
  let newNames = UF.addNamesFromTypeCheckedUnisonFile unisonFile oldNames
  let newPpe = PPE.suffixifiedPPE (PPED.makePPED (PPE.hqNamer 10 newNames) (PPE.suffixifyByHash newNames))
  pp <- Cli.getCurrentProjectPath

  maybeUpdateBranchParentCausalHash <-
    Cli.runTransaction do
      Queries.projectBranchIsUpdateBranch pp.project.projectId pp.branch.branchId >>= \case
        False -> pure Nothing
        True ->
          case pp.branch.parentBranchId of
            Nothing -> pure Nothing -- impossible
            Just updateBranchParentBranchId -> do
              causalHashId <- Queries.expectProjectBranchHead pp.project.projectId updateBranchParentBranchId
              causalHash <- Queries.expectCausalHash causalHashId
              pure (Just causalHash)

  case maybeUpdateBranchParentCausalHash of
    Nothing -> do
      slurpEntries <-
        Cli.runTransaction do
          terms <-
            slurpTerms
              env.codebase
              unisonFile
              False
              (Relation.domain oldNames.terms)
              (Relation.domain unisonFileNames.terms)
          types <-
            slurpTypes
              env.codebase
              unisonFile
              False
              (Relation.domain oldNames.types)
              (Relation.domain unisonFileNames.types)
          pure Defns {terms, types}

      let aliases :: Map Referent (NESet Name)
          aliases =
            getTermAliases oldNames.terms slurpEntries.terms

      let oldPpe =
            PPE.suffixifiedPPE (PPED.makePPED (PPE.hqNamer 10 oldNames) (PPE.suffixifyByHash oldNames))

      Cli.respond (Output.Typechecked oldPpe newPpe slurpEntries aliases)
    Just updateBranchParentCausalHash -> do
      updateBranchParent <- liftIO (Codebase.expectBranchForHash env.codebase updateBranchParentCausalHash)
      let updateBranchParent0 = Branch.head updateBranchParent
      let updateBranchParentNames = Branch.toNames updateBranchParent0
      let updateBranchParentLocalNames = Branch.toNames (Branch.deleteLibdeps updateBranchParent0)
      let updateBranchLocalNames = Names.shadowing unisonFileNames (Branch.toNames (Branch.deleteLibdeps oldBranch0))

      slurpEntries <-
        Cli.runTransaction do
          terms <-
            slurpTerms
              env.codebase
              unisonFile
              True
              (Relation.domain updateBranchParentLocalNames.terms)
              (Relation.domain updateBranchLocalNames.terms)
          types <-
            slurpTypes
              env.codebase
              unisonFile
              False
              (Relation.domain updateBranchParentLocalNames.types)
              (Relation.domain updateBranchLocalNames.types)
          pure Defns {terms, types}

      let aliases :: Map Referent (NESet Name)
          aliases =
            getTermAliases updateBranchParentNames.terms slurpEntries.terms

      let oldPpe =
            PPE.suffixifiedPPE $
              PPED.makePPED
                (PPE.hqNamer 10 updateBranchParentNames)
                (PPE.suffixifyByHash updateBranchParentNames)

      Cli.respond (Output.Typechecked oldPpe newPpe slurpEntries aliases)

  when (not . null $ UF.watchComponents unisonFile) do
    Timing.time "evaluating watches" do
      evalUnisonFile Permissive newPpe unisonFile [] >>= \case
        Right (bindings, e) -> do
          when (not (null e)) do
            let f (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
            Cli.respond $ Output.Evaluated text newPpe bindings (Map.map f e)
        Left err -> Cli.respond (Output.EvaluationFailure err)

  #latestTypecheckedFile .= Just (Right unisonFile)

slurpTerms ::
  Codebase m Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Bool ->
  Map Name (Set Referent) ->
  Map Name (Set Referent) ->
  Sqlite.Transaction (Map Name (TermSlurp Symbol Ann))
slurpTerms codebase unisonFile isUpdate =
  Map.mergeA
    ( if isUpdate
        then Map.traverseMaybeMissing \_ refs ->
          case Set.findMin refs of
            Referent.Ref ref -> do
              ty <- Codebase.expectTypeOfTerm codebase ref
              pure (Just (TermSlurp'Delete ref ty))
            Referent.Con _ _ -> pure Nothing
        else Map.dropMissing
    )
    ( Map.traverseMaybeMissing \name refs ->
        case Set.findMin refs of
          Referent.Ref ref -> do
            ty <- getNewRefType name ref
            pure (Just (TermSlurp'Add ref ty))
          Referent.Con _ _ -> pure Nothing
    )
    ( Map.zipWithMaybeAMatched \name oldRefs newRefs ->
        let oldRef = Set.findMin oldRefs
            newRef = Set.findMin newRefs
         in case (oldRef, newRef) of
              (Referent.Ref oldRef1, Referent.Ref newRef1) ->
                if oldRef1 == newRef1
                  then
                    pure
                      if isUpdate || Map.member (Name.toVar name) (UF.hashTermsId unisonFile)
                        then Just TermSlurp'Unchanged
                        else Nothing
                  else do
                    oldType <- Codebase.expectTypeOfTerm codebase oldRef1
                    newType <- getNewRefType name newRef1
                    pure (Just (TermSlurp'Update oldRef oldType newRef newType))
              (Referent.Con oldRef1 _, Referent.Ref newRef1) -> do
                oldType <- Codebase.expectTypeOfConstructor codebase oldRef1
                newType <- getNewRefType name newRef1
                pure (Just (TermSlurp'Update oldRef oldType newRef newType))
              (Referent.Ref oldRef1, Referent.Con newRef1 _) -> do
                oldType <- Codebase.expectTypeOfTerm codebase oldRef1
                newType <- getNewConType name newRef1
                pure (Just (TermSlurp'Update oldRef oldType newRef newType))
              (Referent.Con _ _, Referent.Con _ _) ->
                pure Nothing
    )
  where
    getNewConType :: Name -> ConstructorReference -> Sqlite.Transaction (Type Symbol Ann)
    getNewConType name ref =
      case Map.lookup (Name.toVar name) (UF.constructorsId unisonFile) of
        Just (ConstructorReference _ conId, decl) ->
          pure (DataDeclaration.expectTypeOfConstructor (DataDeclaration.asDataDecl decl) conId)
        Nothing -> Codebase.expectTypeOfConstructor codebase ref
    getNewRefType :: Name -> TermReference -> Sqlite.Transaction (Type Symbol Ann)
    getNewRefType name ref =
      case Map.lookup (Name.toVar name) (UF.hashTermsId unisonFile) of
        Just (_, _, _, _, ty) -> pure ty
        Nothing -> Codebase.expectTypeOfTerm codebase ref

slurpTypes ::
  Codebase m Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Bool ->
  Map Name (Set TypeReference) ->
  Map Name (Set TypeReference) ->
  Sqlite.Transaction (Map Name (SlurpEntry (DeclOrBuiltin Symbol Ann)))
slurpTypes codebase unisonFile isUpdate =
  Map.mergeA
    ( if isUpdate
        then Map.traverseMissing \_ -> fmap SlurpEntry'Delete . getOldDecl . Set.findMin
        else Map.dropMissing
    )
    (Map.traverseMissing \name -> fmap SlurpEntry'Add . getNewDecl name . Set.findMin)
    ( Map.zipWithMaybeAMatched \name oldRefs newRefs ->
        let oldRef = Set.findMin oldRefs
            newRef = Set.findMin newRefs
         in if oldRef /= newRef
              then fmap Just do
                oldDecl <- getOldDecl oldRef
                newDecl <- getNewDecl name newRef
                pure (SlurpEntry'Update oldDecl newDecl)
              else
                pure
                  if isUpdate
                    then case UF.lookupDecl (Name.toVar name) unisonFile of
                      Nothing -> Nothing
                      Just _ -> Just SlurpEntry'Unchanged
                    else Just SlurpEntry'Unchanged
    )
  where
    getOldDecl :: TypeReference -> Sqlite.Transaction (DeclOrBuiltin Symbol Ann)
    getOldDecl = \case
      Reference.DerivedId ref -> DeclOrBuiltin.Decl <$> Codebase.unsafeGetTypeDeclaration codebase ref
      Reference.Builtin builtin -> pure (DeclOrBuiltin.Builtin (Builtin.expectBuiltinConstructorType builtin))
    getNewDecl :: Name -> TypeReference -> Sqlite.Transaction (DeclOrBuiltin Symbol Ann)
    getNewDecl name = \case
      Reference.DerivedId ref ->
        case UF.lookupDecl (Name.toVar name) unisonFile of
          Just (_, decl) -> pure (DeclOrBuiltin.Decl decl)
          Nothing -> DeclOrBuiltin.Decl <$> Codebase.unsafeGetTypeDeclaration codebase ref
      Reference.Builtin builtin ->
        pure (DeclOrBuiltin.Builtin (Builtin.expectBuiltinConstructorType builtin))

getTermAliases :: Relation Name Referent -> Map Name (TermSlurp Symbol Ann) -> Map Referent (NESet Name)
getTermAliases existingTerms slurpTerms =
  -- For the purpose of identifying aliases to call out, we omit names that are changing by this update.
  let (changedNames, changedRefs) =
        Map.foldlWithKey'
          ( \ ~acc@(names, refs) name -> \case
              TermSlurp'Add ref _ ->
                let !names1 = Set.insert name names
                    !refs1 = Set.insert (Referent.Ref ref) refs
                 in (names1, refs1)
              TermSlurp'Delete ref _ ->
                let !names1 = Set.insert name names
                    !refs1 = Set.insert (Referent.Ref ref) refs
                 in (names1, refs1)
              TermSlurp'Update old _ new _ ->
                let !names1 = Set.insert name names
                    !refs1 = Set.insert new (Set.insert old refs)
                 in (names1, refs1)
              TermSlurp'Unchanged -> acc
          )
          (Set.empty, Set.empty)
          slurpTerms

      step acc ref =
        let existingNames = Relation.lookupRan ref existingTerms
         in case Set.NonEmpty.nonEmptySet (Set.difference existingNames changedNames) of
              Nothing -> acc
              Just aliases -> Map.insert ref aliases acc
   in Set.foldl' step Map.empty changedRefs

parseAndTypecheckUnisonFile ::
  Names ->
  Text ->
  Text ->
  Cli (TypecheckedUnisonFile Symbol Ann)
parseAndTypecheckUnisonFile names sourceName text = do
  pp <- Cli.getCurrentProjectPath
  State.modify' \loopState ->
    loopState
      & (#latestFile .~ Just (Text.unpack sourceName, False))
      & (#latestTypecheckedFile .~ Nothing)
  Cli.Env {codebase, generateUniqueName} <- ask
  uniqueName <- liftIO generateUniqueName
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = uniqueName,
            uniqueTypeGuid = Cli.loadUniqueTypeGuid pp,
            names,
            maybeNamespace = Nothing,
            localNamespacePrefixedTypesAndConstructors = mempty
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
      tws = reverse [wrn | Result.TypeWarning wrn <- toList notes]
      suffixifiedPPE = PPED.suffixifiedPPE pped
      pped =
        let ns =
              names
                -- Shadow just the type decl and constructor names (because the unison file didn't typecheck so we
                -- don't have term `Names`)
                & Names.shadowing (UF.toNames unisonFile)
         in PPED.makePPED
              (PPE.hqNamer 10 ns)
              ( PPE.suffixifyByHashWithUnhashedTermsInScope
                  ( Set.union
                      (Set.map Name.unsafeParseVar (Map.keysSet (UF.terms unisonFile)))
                      ( foldMap
                          ( foldMap \case
                              (v, _, _) ->
                                case Var.typeOf v of
                                  Var.User _ -> Set.singleton (Name.unsafeParseVar v)
                                  _ -> Set.empty
                          )
                          (UF.watches unisonFile)
                      )
                  )
                  ns
              )

  when (not $ null tws) do
    currentPath <- Cli.getCurrentPath
    Cli.respond $
      Output.TypeWarns currentPath text suffixifiedPPE tws

  maybeTypecheckedUnisonFile & onNothing do
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
    ( Either
        Runtime.Error
        ( [(Symbol, Term Symbol ())],
          Map Symbol (Ann, WK.WatchKind, Reference.Id, Term Symbol (), Term Symbol (), Bool)
        )
    )
evalUnisonFile mode ppe unisonFile args = do
  env <- ask

  let theRuntime = case mode of
        Sandboxed -> env.sandboxedRuntime
        Permissive -> env.runtime

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction env.codebase (Codebase.lookupWatchCache env.codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  Cli.with_ (withArgs args) do
    let codeLookup = Codebase.codebaseToCodeLookup env.codebase
    liftIO (Runtime.evaluateWatches codeLookup ppe watchCache theRuntime unisonFile) >>= \case
      Right (nts, errs, map) -> do
        when (not $ null errs) (RuntimeUtils.displayDecompileErrors errs)
        for_ (Map.elems map) \(_loc, kind, hash, _src, value, isHit) -> do
          -- only update the watch cache when there are no errors
          when (not isHit && null errs) do
            let value' = Term.amap (\() -> Ann.External) value
            Cli.runTransaction (Codebase.putWatch kind hash value')
        pure (Right (nts, map))
      Left err -> pure (Left err)
