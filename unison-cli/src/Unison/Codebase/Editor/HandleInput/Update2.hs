-- | @update@ input handler.
module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,

    -- * Misc helpers to be organized later
    addDefinitionsToUnisonFile,
    makeUnisonFile,
    findCtorNames,
    findCtorNamesMaybe,
    forwardCtorNames,
    makeParsingEnv,
    prettyParseTypecheck,
    prettyParseTypecheck2,
    typecheckedUnisonFileToBranchUpdates,
    typecheckedUnisonFileToBranchAdds,
    getNamespaceDependentsOf,
    getNamespaceDependentsOf2,
    makeComplicatedPPE,
  )
where

import Control.Lens qualified as Lens
import Control.Monad.RWS (ask)
import Data.Bifoldable (bifoldMap)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra ((|>))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy.Text
import Text.Pretty.Simple (pShow)
import U.Codebase.Reference (Reference, TermReferenceId)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Builtin.Decls qualified as Decls
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.Type (Codebase)
import Unison.ConstructorReference (GConstructorReference (ConstructorReference))
import Unison.DataDeclaration (DataDeclaration, Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Debug qualified as Debug
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Name.Forward (ForwardName (..))
import Unison.Name.Forward qualified as ForwardName
import Unison.NameSegment qualified as NameSegment
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference (fromId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Sqlite (Transaction)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

handleUpdate2 :: Cli ()
handleUpdate2 = do
  Cli.Env {codebase, writeSource} <- ask
  tuf <- Cli.expectLatestTypecheckedFile
  let termAndDeclNames = getTermAndDeclNames tuf
  pp <- Cli.getCurrentProjectPath
  currentBranch0 <- Cli.getCurrentBranch0
  let namesIncludingLibdeps = Branch.toNames currentBranch0
  let namesExcludingLibdeps = Branch.toNames (currentBranch0 & over Branch.children (Map.delete NameSegment.libSegment))
  let ctorNames = forwardCtorNames namesExcludingLibdeps

  Cli.respond Output.UpdateLookingForDependents
  (pped, bigUf) <- Cli.runTransactionWithRollback \abort -> do
    dependents <-
      getNamespaceDependentsOf namesExcludingLibdeps (getExistingReferencesNamed termAndDeclNames namesExcludingLibdeps)
    hashLen <- Codebase.hashLength
    bigUf <-
      addDefinitionsToUnisonFile
        abort
        codebase
        (findCtorNames Output.UOUUpdate namesExcludingLibdeps ctorNames)
        dependents
        (UF.discardTypes tuf)
    pure (makeComplicatedPPE hashLen namesIncludingLibdeps (UF.typecheckedToNames tuf) dependents, bigUf)

  -- If the new-unison-file-to-typecheck is the same as old-unison-file-that-we-already-typechecked, then don't bother
  -- typechecking again.
  secondTuf <- do
    let smallUf = UF.discardTypes tuf
    let noChanges =
          and
            [ Map.size (UF.dataDeclarationsId smallUf) == Map.size (UF.dataDeclarationsId bigUf),
              Map.size (UF.effectDeclarationsId smallUf) == Map.size (UF.effectDeclarationsId bigUf),
              Map.size (UF.terms smallUf) == Map.size (UF.terms bigUf),
              Map.size (UF.watches smallUf) == Map.size (UF.watches bigUf)
            ]
    if noChanges
      then pure tuf
      else do
        Cli.respond Output.UpdateStartTypechecking
        parsingEnv <- makeParsingEnv pp namesIncludingLibdeps
        secondTuf <-
          prettyParseTypecheck bigUf pped parsingEnv & onLeftM \prettyUf -> do
            scratchFilePath <- fst <$> Cli.expectLatestFile
            liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUf)
            Cli.returnEarly Output.UpdateTypecheckingFailure
        Cli.respond Output.UpdateTypecheckingSuccess
        pure secondTuf

  saveTuf (findCtorNamesMaybe Output.UOUUpdate namesExcludingLibdeps ctorNames Nothing) secondTuf
  Cli.respond Output.Success

-- TODO: find a better module for this function, as it's used in a couple places
prettyParseTypecheck ::
  UnisonFile Symbol Ann ->
  PrettyPrintEnvDecl ->
  Parser.ParsingEnv Transaction ->
  Cli (Either (Pretty Pretty.ColorText) (TypecheckedUnisonFile Symbol Ann))
prettyParseTypecheck bigUf pped =
  prettyParseTypecheck2 (Pretty.prettyUnisonFile pped bigUf)

-- TODO: find a better module for this function, as it's used in a couple places
prettyParseTypecheck2 ::
  Pretty Pretty.ColorText ->
  Parser.ParsingEnv Transaction ->
  Cli (Either (Pretty Pretty.ColorText) (TypecheckedUnisonFile Symbol Ann))
prettyParseTypecheck2 prettyUf parsingEnv = do
  Cli.Env {codebase} <- ask
  let stringUf = Pretty.toPlain 80 prettyUf
  Debug.whenDebug Debug.Update do
    liftIO do
      putStrLn "--- Scratch ---"
      putStrLn stringUf
  Cli.runTransaction do
    Parsers.parseFile "<update>" stringUf parsingEnv >>= \case
      Left {} -> pure $ Left prettyUf
      Right reparsedUf -> do
        typecheckingEnv <-
          computeTypecheckingEnvironment (FileParsers.ShouldUseTndr'Yes parsingEnv) codebase [] reparsedUf
        pure case FileParsers.synthesizeFile typecheckingEnv reparsedUf of
          Result.Result _notes (Just reparsedTuf) -> Right reparsedTuf
          Result.Result _notes Nothing -> Left prettyUf

-- @makeParsingEnv path names@ makes a parsing environment with @names@ in scope, which are all relative to @path@.
makeParsingEnv :: ProjectPath -> Names -> Cli (Parser.ParsingEnv Transaction)
makeParsingEnv path names = do
  Cli.Env {generateUniqueName} <- ask
  uniqueName <- liftIO generateUniqueName
  pure do
    Parser.ParsingEnv
      { uniqueNames = uniqueName,
        uniqueTypeGuid = Cli.loadUniqueTypeGuid path,
        names
      }

-- save definitions and namespace
saveTuf :: (Name -> Either Output (Maybe [Name])) -> TypecheckedUnisonFile Symbol Ann -> Cli ()
saveTuf getConstructors tuf = do
  Cli.Env {codebase} <- ask
  pp <- Cli.getCurrentProjectPath
  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase codebase tuf
      typecheckedUnisonFileToBranchUpdates abort getConstructors tuf
  Cli.stepAt "update" (pp, Branch.batchUpdates branchUpdates)

-- @typecheckedUnisonFileToBranchUpdates getConstructors file@ returns a list of branch updates (suitable for passing
-- along to `batchUpdates` or some "step at" combinator) that corresponds to using all of the contents of @file@.
-- `getConstructors` returns the full constructor names of a decl, e.g. "Maybe" -> ["Maybe.Nothing", "Maybe.Just"]
--
-- For example, if the file contains
--
--     foo.bar.baz = <#foo>
--
-- then the returned updates will look like
--
--     [ ("foo.bar", insert-term("baz",<#foo>)) ]
typecheckedUnisonFileToBranchUpdates ::
  (forall void. Output -> Transaction void) ->
  -- | Returns 'Nothing' if the decl isn't in namesExcludingLibdeps,
  -- in which case we know the decl is new and do not need to generate
  -- delete actions for it.
  (Name -> Either Output (Maybe [Name])) ->
  TypecheckedUnisonFile Symbol Ann ->
  Transaction [(Path, Branch0 m -> Branch0 m)]
typecheckedUnisonFileToBranchUpdates abort getConstructors tuf = do
  declUpdates <- makeDeclUpdates abort
  pure $ declUpdates ++ termUpdates
  where
    makeDeclUpdates :: forall m. (forall void. Output -> Transaction void) -> Transaction [(Path, Branch0 m -> Branch0 m)]
    makeDeclUpdates abort = do
      dataDeclUpdates <- Monoid.foldMapM makeDataDeclUpdates (Map.toList $ UF.dataDeclarationsId' tuf)
      effectDeclUpdates <- Monoid.foldMapM makeEffectDeclUpdates (Map.toList $ UF.effectDeclarationsId' tuf)
      pure $ dataDeclUpdates <> effectDeclUpdates
      where
        makeDataDeclUpdates (symbol, (typeRefId, dataDecl)) = makeDeclUpdates (symbol, (typeRefId, Right dataDecl))
        makeEffectDeclUpdates (symbol, (typeRefId, effectDecl)) = makeDeclUpdates (symbol, (typeRefId, Left effectDecl))

        makeDeclUpdates :: (Symbol, (TypeReferenceId, Decl Symbol Ann)) -> Transaction [(Path, Branch0 m -> Branch0 m)]
        makeDeclUpdates (symbol, (typeRefId, decl)) = do
          -- some decls will be deleted, we want to delete their
          -- constructors as well
          deleteConstructorActions <-
            (maybe [] (map (BranchUtil.makeAnnihilateTermName . Path.splitFromName)) <$> getConstructors (Name.unsafeParseVar symbol)) & onLeft abort
          let deleteTypeAction = BranchUtil.makeAnnihilateTypeName split
              split = splitVar symbol
              insertTypeAction = BranchUtil.makeAddTypeName split (Reference.fromId typeRefId)
              insertTypeConstructorActions =
                let referentIdsWithNames = zip (Decl.constructorVars (Decl.asDataDecl decl)) (Decl.declConstructorReferents typeRefId decl)
                 in map
                      ( \(sym, rid) ->
                          let splitConName = splitVar sym
                           in BranchUtil.makeAddTermName splitConName (Reference.fromId <$> rid)
                      )
                      referentIdsWithNames
              deleteStuff = deleteTypeAction : deleteConstructorActions
              addStuff = insertTypeAction : insertTypeConstructorActions
          pure $ deleteStuff ++ addStuff

    termUpdates :: [(Path, Branch0 m -> Branch0 m)]
    termUpdates =
      tuf
        & UF.hashTermsId
        & Map.toList
        & foldMap \(var, (_, ref, wk, _, _)) ->
          if WK.watchKindShouldBeStoredInDatabase wk
            then
              let split = splitVar var
               in [ BranchUtil.makeAnnihilateTermName split,
                    BranchUtil.makeAddTermName split (Referent.fromTermReferenceId ref)
                  ]
            else []

    splitVar :: Symbol -> Path.Split
    splitVar = Path.splitFromName . Name.unsafeParseVar

typecheckedUnisonFileToBranchAdds :: TypecheckedUnisonFile Symbol Ann -> [(Path, Branch0 m -> Branch0 m)]
typecheckedUnisonFileToBranchAdds tuf = do
  declAdds ++ termAdds
  where
    declAdds :: [(Path, Branch0 m -> Branch0 m)]
    declAdds = do
      foldMap makeDataDeclAdds (Map.toList $ UF.dataDeclarationsId' tuf)
        ++ foldMap makeEffectDeclUpdates (Map.toList $ UF.effectDeclarationsId' tuf)
      where
        makeDataDeclAdds (symbol, (typeRefId, dataDecl)) = makeDeclAdds (symbol, (typeRefId, Right dataDecl))
        makeEffectDeclUpdates (symbol, (typeRefId, effectDecl)) = makeDeclAdds (symbol, (typeRefId, Left effectDecl))

        makeDeclAdds :: (Symbol, (TypeReferenceId, Decl Symbol Ann)) -> [(Path, Branch0 m -> Branch0 m)]
        makeDeclAdds (symbol, (typeRefId, decl)) =
          let insertTypeAction = BranchUtil.makeAddTypeName (splitVar symbol) (Reference.fromId typeRefId)
              insertTypeConstructorActions =
                zipWith
                  (\sym rid -> BranchUtil.makeAddTermName (splitVar sym) (Reference.fromId <$> rid))
                  (Decl.constructorVars (Decl.asDataDecl decl))
                  (Decl.declConstructorReferents typeRefId decl)
           in insertTypeAction : insertTypeConstructorActions

    termAdds :: [(Path, Branch0 m -> Branch0 m)]
    termAdds =
      tuf
        & UF.hashTermsId
        & Map.toList
        & mapMaybe \(var, (_, ref, wk, _, _)) -> do
          guard (WK.watchKindShouldBeStoredInDatabase wk)
          Just (BranchUtil.makeAddTermName (splitVar var) (Referent.fromTermReferenceId ref))

    splitVar :: Symbol -> Path.Split
    splitVar = Path.splitFromName . Name.unsafeParseVar

-- | get references from `names` that have the same names as in `defns`
-- For constructors, we get the type reference.
getExistingReferencesNamed :: DefnsF Set Name Name -> Names -> Set Reference
getExistingReferencesNamed defns names =
  bifoldMap fromTerms fromTypes defns
  where
    fromTerms :: Set Name -> Set Reference
    fromTerms =
      foldMap \name ->
        Set.map Referent.toReference (Relation.lookupDom name (Names.terms names))

    fromTypes :: Set Name -> Set TypeReference
    fromTypes =
      foldMap \name ->
        Relation.lookupDom name (Names.types names)

makeUnisonFile ::
  (forall void. Output -> Transaction void) ->
  Codebase IO Symbol Ann ->
  (Maybe Int -> Name -> Either Output.Output [Name]) ->
  DefnsF (Relation Name) TermReferenceId TypeReferenceId ->
  Transaction (UnisonFile Symbol Ann)
makeUnisonFile abort codebase doFindCtorNames defns = do
  file <- foldM addTermComponent UF.emptyUnisonFile (Set.map Reference.idToHash (Relation.ran defns.terms))
  foldM addDeclComponent file (Set.map Reference.idToHash (Relation.ran defns.types))
  where
    addTermComponent :: UnisonFile Symbol Ann -> Hash -> Transaction (UnisonFile Symbol Ann)
    addTermComponent uf h = do
      termComponent <- Codebase.unsafeGetTermComponent codebase h
      pure $ foldl' addTermElement uf (zip termComponent [0 ..])
      where
        addTermElement :: UnisonFile Symbol Ann -> ((Term Symbol Ann, Type Symbol Ann), Reference.Pos) -> UnisonFile Symbol Ann
        addTermElement uf ((tm, tp), i) = do
          let termNames = Relation.lookupRan (Reference.Id h i) defns.terms
          foldl' (addDefinition tm tp) uf termNames
        addDefinition :: Term Symbol Ann -> Type Symbol Ann -> UnisonFile Symbol Ann -> Name -> UnisonFile Symbol Ann
        addDefinition tm tp uf (Name.toVar -> v) =
          let prependTerm to = (v, Ann.External, tm) : to
           in if isTest tp
                then uf & #watches . Lens.at WK.TestWatch . Lens.non [] Lens.%~ prependTerm
                else uf & #terms Lens.%~ Map.insert v (Ann.External, tm)

    isTest = Typechecker.isEqual (Decls.testResultListType mempty)

    -- given a dependent hash, include that component in the scratch file
    -- todo: wundefined: cut off constructor name prefixes
    addDeclComponent :: UnisonFile Symbol Ann -> Hash -> Transaction (UnisonFile Symbol Ann)
    addDeclComponent uf h = do
      declComponent <- fromJust <$> Codebase.getDeclComponent h
      foldM addDeclElement uf (zip declComponent [0 ..])
      where
        -- for each name a decl has, update its constructor names according to what exists in the namespace
        addDeclElement :: UnisonFile Symbol Ann -> (Decl Symbol Ann, Reference.Pos) -> Transaction (UnisonFile Symbol Ann)
        addDeclElement uf (decl, i) = do
          let declNames = Relation.lookupRan (Reference.Id h i) defns.types
          -- look up names for this decl's constructor based on the decl's name, and embed them in the decl definition.
          foldM (addRebuiltDefinition decl) uf declNames
          where
            -- skip any definitions that already have names, we don't want to overwrite what the user has supplied
            addRebuiltDefinition :: Decl Symbol Ann -> UnisonFile Symbol Ann -> Name -> Transaction (UnisonFile Symbol Ann)
            addRebuiltDefinition decl uf name = case decl of
              Left ed ->
                overwriteConstructorNames name ed.toDataDecl <&> \ed' ->
                  uf
                    & #effectDeclarationsId
                      %~ Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, Decl.EffectDeclaration ed')
              Right dd ->
                overwriteConstructorNames name dd <&> \dd' ->
                  uf
                    & #dataDeclarationsId
                      %~ Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, dd')

        -- Constructor names are bogus when pulled from the database, so we set them to what they should be here
        overwriteConstructorNames :: Name -> DataDeclaration Symbol Ann -> Transaction (DataDeclaration Symbol Ann)
        overwriteConstructorNames name dd =
          let constructorNames :: Transaction [Symbol]
              constructorNames =
                case doFindCtorNames (Just $ Decl.constructorCount dd) name of
                  Left err -> abort err
                  Right array | all (isJust . Name.stripNamePrefix name) array -> pure (map Name.toVar array)
                  Right array -> do
                    traceM "I ran into a situation where a type's constructors didn't match its name,"
                    traceM "in a spot where I didn't expect to be discovering that.\n\n"
                    traceM "Type Name:"
                    traceM . Lazy.Text.unpack $ pShow name
                    traceM "Constructor Names:"
                    traceM . Lazy.Text.unpack $ pShow array
                    error "Sorry for crashing."

              swapConstructorNames oldCtors =
                let (annotations, _vars, types) = unzip3 oldCtors
                 in zip3 annotations <$> constructorNames <*> pure types
           in Lens.traverseOf Decl.constructors_ swapConstructorNames dd

-- | @addDefinitionsToUnisonFile abort codebase doFindCtorNames definitions file@ adds all @definitions@ to @file@,
-- avoiding overwriting anything already in @file@. Every definition is put into the file with every naming it has in
-- @names@ "on the left-hand-side of the equals" (but yes type decls don't really have a LHS).
--
-- TODO: find a better module for this function, as it's used in a couple places
addDefinitionsToUnisonFile ::
  (forall void. Output -> Transaction void) ->
  Codebase IO Symbol Ann ->
  (Maybe Int -> Name -> Either Output.Output [Name]) ->
  DefnsF (Relation Name) TermReferenceId TypeReferenceId ->
  UnisonFile Symbol Ann ->
  Transaction (UnisonFile Symbol Ann)
addDefinitionsToUnisonFile abort codebase doFindCtorNames newDefns oldUF = do
  newUF <- makeUnisonFile abort codebase doFindCtorNames newDefns
  pure (oldUF `UF.leftBiasedMerge` newUF)

-- | O(r + c * d) touches all the referents (r), and all the NameSegments (d) of all of the Con referents (c)
forwardCtorNames :: Names -> Map ForwardName (Referent, Name)
forwardCtorNames names =
  Map.fromList $
    [ (ForwardName.fromName name, (r, name))
      | (r@Referent.Con {}, rNames) <- Map.toList $ Relation.range names.terms,
        name <- Foldable.toList rNames
    ]

-- | given a decl name, find names for all of its constructors, in order.
--
-- Precondition: 'n' is an element of 'names'
findCtorNames :: Output.UpdateOrUpgrade -> Names -> Map ForwardName (Referent, Name) -> Maybe Int -> Name -> Either Output.Output [Name]
findCtorNames operation names forwardCtorNames ctorCount n =
  let declRef = case Set.lookupMin (Relation.lookupDom n names.types) of
        Nothing -> error "[findCtorNames] precondition violation: n is not an element of names"
        Just x -> x
      f = ForwardName.fromName n
      (_, centerRight) = Map.split f forwardCtorNames
      (center, _) = Map.split (incrementLastSegmentChar f) centerRight

      insertShortest :: Map ConstructorId Name -> (Referent, Name) -> Map ConstructorId Name
      insertShortest m (Referent.Con (ConstructorReference r cid) _ct, newName) | r == declRef =
        case Map.lookup cid m of
          Just existingName
            | length (Name.segments existingName) > length (Name.segments newName) ->
                Map.insert cid newName m
          Just {} -> m
          Nothing -> Map.insert cid newName m
      insertShortest m _ = m
      m = foldl' insertShortest mempty (Foldable.toList center)
      ctorCountGuess = fromMaybe (Map.size m) ctorCount
   in if Map.size m == ctorCountGuess && all (isJust . flip Map.lookup m . fromIntegral) [0 .. ctorCountGuess - 1]
        then Right $ Map.elems m
        else Left $ Output.UpdateIncompleteConstructorSet operation n m ctorCount

findCtorNamesMaybe ::
  Output.UpdateOrUpgrade ->
  Names ->
  Map ForwardName (Referent, Name) ->
  Maybe Int ->
  Name ->
  Either Output.Output (Maybe [Name])
findCtorNamesMaybe operation names forwardCtorNames ctorCount name =
  case Relation.memberDom name (Names.types names) of
    True -> Just <$> findCtorNames operation names forwardCtorNames ctorCount name
    False -> Right Nothing

-- Used by `findCtorNames` to filter `forwardCtorNames` to a narrow range which will be searched linearly.
-- >>> incrementLastSegmentChar $ ForwardName.fromName $ Name.unsafeFromText "foo.bar.quux"
-- ForwardName {toList = "foo" :| ["bar","quuy"]}
incrementLastSegmentChar :: ForwardName -> ForwardName
incrementLastSegmentChar (ForwardName segments) =
  let (initSegments, lastSegment) = (NonEmpty.init segments, NonEmpty.last segments)
      incrementedLastSegment = incrementLastCharInSegment lastSegment
   in ForwardName $ maybe (NonEmpty.singleton incrementedLastSegment) (|> incrementedLastSegment) (NonEmpty.nonEmpty initSegments)
  where
    incrementLastCharInSegment :: NameSegment -> NameSegment
    incrementLastCharInSegment (NameSegment text) =
      let incrementedText =
            if Text.null text
              then text
              else Text.init text `Text.append` Text.singleton (succ $ Text.last text)
       in NameSegment incrementedText

-- @getTermAndDeclNames file@ returns the names of the terms and decls defined in a typechecked Unison file.
getTermAndDeclNames :: (Var v) => TypecheckedUnisonFile v a -> DefnsF Set Name Name
getTermAndDeclNames tuf =
  Defns (terms <> effectCtors <> dataCtors) (effects <> datas)
  where
    terms =
      UF.hashTermsId tuf
        & Map.foldMapWithKey \var (_, _, wk, _, _) ->
          if WK.watchKindShouldBeStoredInDatabase wk
            then Set.singleton (Name.unsafeParseVar var)
            else Set.empty
    effects = keysToNames $ UF.effectDeclarationsId' tuf
    datas = keysToNames $ UF.dataDeclarationsId' tuf
    effectCtors = foldMap ctorsToNames $ fmap (Decl.toDataDecl . snd) $ UF.effectDeclarationsId' tuf
    dataCtors = foldMap ctorsToNames $ fmap snd $ UF.dataDeclarationsId' tuf
    keysToNames = Set.map Name.unsafeParseVar . Map.keysSet
    ctorsToNames = Set.fromList . map Name.unsafeParseVar . Decl.constructorVars

-- | Given a namespace and a set of dependencies, return the subset of the namespace that consists of only the
-- (transitive) dependents of the dependencies.
getNamespaceDependentsOf ::
  Names ->
  Set Reference ->
  Transaction (DefnsF (Relation Name) TermReferenceId TypeReferenceId)
getNamespaceDependentsOf names dependencies = do
  dependents <- Ops.transitiveDependentsWithinScope (Names.referenceIds names) dependencies
  pure (bimap (foldMap nameTerm) (foldMap nameType) dependents)
  where
    nameTerm :: TermReferenceId -> Relation Name TermReferenceId
    nameTerm ref =
      Relation.fromManyDom (Relation.lookupRan (Referent.fromTermReferenceId ref) (Names.terms names)) ref

    nameType :: TypeReferenceId -> Relation Name TypeReferenceId
    nameType ref =
      Relation.fromManyDom (Relation.lookupRan (Reference.fromId ref) (Names.types names)) ref

-- | A better version of the above that operates on BiMultimaps rather than Relations.
getNamespaceDependentsOf2 ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Set Reference ->
  Transaction (DefnsF (Map Name) TermReferenceId TypeReferenceId)
getNamespaceDependentsOf2 defns dependencies = do
  let toTermScope = Set.mapMaybe Referent.toReferenceId . BiMultimap.dom
  let toTypeScope = Set.mapMaybe Reference.toId . BiMultimap.dom
  let scope = bifoldMap toTermScope toTypeScope defns

  dependents <-
    Ops.transitiveDependentsWithinScope scope dependencies

  pure
    Defns
      { terms = Set.foldl' addTerms Map.empty dependents.terms,
        types = Set.foldl' addTypes Map.empty dependents.types
      }
  where
    addTerms :: Map Name TermReferenceId -> TermReferenceId -> Map Name TermReferenceId
    addTerms acc0 ref =
      let names = BiMultimap.lookupDom (Referent.fromTermReferenceId ref) defns.terms
       in Set.foldl' (\acc name -> Map.insert name ref acc) acc0 names

    addTypes :: Map Name TypeReferenceId -> TypeReferenceId -> Map Name TypeReferenceId
    addTypes acc0 ref =
      let names = BiMultimap.lookupDom (Reference.fromId ref) defns.types
       in Set.foldl' (\acc name -> Map.insert name ref acc) acc0 names

-- The big picture behind PPE building, though there are many details:
--
--   * We are updating old references to new references by rendering old references as names that are then parsed
--     back to resolve to new references (the world's weirdest implementation of AST substitution).
--
--   * We have to render names that refer to definitions in the file with a different suffixification strategy
--     (namely, "suffixify by name") than names that refer to things in the codebase.
--
--     This is because you *may* refer to aliases that share a suffix by that suffix for definitions in the
--     codebase, but not in the file.
--
--     For example, the following file will fail to parse:
--
--       one.foo = 10
--       two.foo = 10
--       hey = foo + foo -- "Which foo do you mean? There are two."
--
--     However, the following file will not fail to parse, if `one.foo` and `two.foo` are aliases in the codebase:
--
--       hey = foo + foo
makeComplicatedPPE ::
  Int ->
  Names ->
  Names ->
  DefnsF (Relation Name) TermReferenceId TypeReferenceId ->
  PrettyPrintEnvDecl
makeComplicatedPPE hashLen names initialFileNames dependents =
  PPED.makePPED (PPE.namer namesInTheFile) (PPE.suffixifyByName namesInTheFile)
    `PPED.addFallback` PPED.makePPED (PPE.hqNamer hashLen namesInTheNamespace) (PPE.suffixifyByHash namesInTheNamespace)
  where
    namesInTheFile =
      initialFileNames
        <> Names
          (Relation.mapRan Referent.fromTermReferenceId dependents.terms)
          (Relation.mapRan Reference.fromId dependents.types)
    namesInTheNamespace = Names.unionLeftName names initialFileNames
