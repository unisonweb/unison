{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,

    -- * Misc helpers to be organized later
    addDefinitionsToUnisonFile,
    findCtorNames,
    forwardCtorNames,
    makeParsingEnv,
    prettyParseTypecheck,
    typecheckedUnisonFileToBranchUpdates,
  )
where

import Control.Lens (over, (^.))
import Control.Lens qualified as Lens
import Control.Monad.RWS (ask)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra ((|>))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy.Text
import Text.Pretty.Simple (pShow)
import U.Codebase.Reference (Reference, ReferenceType)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Builtin.Decls qualified as Decls
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Branch.Type (Branch0)
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Type (Codebase)
import Unison.CommandLine.OutputMessages qualified as Output
import Unison.ConstructorReference (GConstructorReference (ConstructorReference))
import Unison.DataDeclaration (DataDeclaration, Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Debug qualified as Debug
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.HashQualified' qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Name.Forward (ForwardName (..))
import Unison.Name.Forward qualified as ForwardName
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory (NamesWithHistory (NamesWithHistory))
import Unison.NamesWithHistory qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Reference (TypeReferenceId)
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
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Nametree (Defns (..))
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

handleUpdate2 :: Cli ()
handleUpdate2 = do
  Cli.Env {codebase} <- ask
  tuf <- Cli.expectLatestTypecheckedFile

  -- - get add/updates from TUF
  let termAndDeclNames :: Defns (Set Name) (Set Name) = getTermAndDeclNames tuf

  currentPath <- Cli.getCurrentPath
  currentBranch0 <- Cli.getBranch0At currentPath
  let namesIncludingLibdeps = Branch.toNames currentBranch0
  let namesExcludingLibdeps = Branch.toNames (currentBranch0 & over Branch.children (Map.delete Name.libSegment))
  let ctorNames = forwardCtorNames namesExcludingLibdeps

  Cli.respond Output.UpdateLookingForDependents
  (pped, bigUf) <- Cli.runTransactionWithRollback \abort -> do
    dependents <-
      Ops.dependentsWithinScope
        (Names.referenceIds namesExcludingLibdeps)
        (getExistingReferencesNamed termAndDeclNames namesExcludingLibdeps)
    -- - construct PPE for printing UF* for typechecking (whatever data structure we decide to print)
    bigUf <- buildBigUnisonFile abort codebase tuf dependents namesExcludingLibdeps ctorNames
    pped <-
      ( \hlen ->
          shadowNames
            hlen
            (UF.typecheckedToNames tuf)
            (NamesWithHistory.fromCurrentNames namesIncludingLibdeps)
        )
        <$> Codebase.hashLength

    pure (pped, bigUf)

  -- If the new-unison-file-to-typecheck is the same as old-unison-file-that-we-already-typechecked, then don't bother
  -- typechecking again.
  secondTuf <- do
    let smallUf = UF.discardTypes tuf
    let noChanges =
          and
            [ Map.size (UF.dataDeclarations smallUf) == Map.size (UF.dataDeclarations bigUf),
              Map.size (UF.effectDeclarations smallUf) == Map.size (UF.effectDeclarations bigUf),
              length @[] (UF.terms smallUf) == length @[] (UF.terms bigUf),
              Map.size (UF.watches smallUf) == Map.size (UF.watches bigUf)
            ]
    if noChanges
      then pure tuf
      else do
        Cli.respond Output.UpdateStartTypechecking
        parsingEnv <- makeParsingEnv currentPath namesIncludingLibdeps
        secondTuf <-
          prettyParseTypecheck bigUf pped parsingEnv & onLeftM \prettyUf -> do
            Cli.Env {isTranscript} <- ask
            maybePath <- if isTranscript then pure Nothing else Just . fst <$> Cli.expectLatestFile
            Cli.respond (Output.DisplayDefinitionsString maybePath prettyUf)
            Cli.returnEarly Output.UpdateTypecheckingFailure
        Cli.respond Output.UpdateTypecheckingSuccess
        pure secondTuf

  saveTuf (findCtorNames namesExcludingLibdeps ctorNames Nothing) secondTuf
  Cli.respond Output.Success

-- TODO: find a better module for this function, as it's used in a couple places
prettyParseTypecheck ::
  UnisonFile Symbol Ann ->
  PrettyPrintEnvDecl ->
  Parser.ParsingEnv Transaction ->
  Cli (Either (Pretty Pretty.ColorText) (TypecheckedUnisonFile Symbol Ann))
prettyParseTypecheck bigUf pped parsingEnv = do
  Cli.Env {codebase} <- ask
  let prettyUf = Output.prettyUnisonFile pped bigUf
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
makeParsingEnv :: Path.Absolute -> Names -> Cli (Parser.ParsingEnv Transaction)
makeParsingEnv path names = do
  Cli.Env {generateUniqueName} <- ask
  uniqueName <- liftIO generateUniqueName
  pure do
    Parser.ParsingEnv
      { uniqueNames = uniqueName,
        uniqueTypeGuid = Cli.loadUniqueTypeGuid path,
        names = NamesWithHistory {currentNames = names, oldNames = mempty}
      }

-- save definitions and namespace
saveTuf :: (Name -> Either Output [Name]) -> TypecheckedUnisonFile Symbol Ann -> Cli ()
saveTuf getConstructors tuf = do
  Cli.Env {codebase} <- ask
  currentPath <- Cli.getCurrentPath
  branchUpdates <- Cli.runTransactionWithRollback \abort -> do
    Codebase.addDefsToCodebase codebase tuf
    typecheckedUnisonFileToBranchUpdates abort getConstructors tuf
  Cli.stepAt "update" (Path.unabsolute currentPath, Branch.batchUpdates branchUpdates)

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
  (Name -> Either Output [Name]) ->
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
          deleteConstructorActions <- case map (BranchUtil.makeAnnihilateTermName . Path.splitFromName) <$> getConstructors (Name.unsafeFromVar symbol) of
            Left err -> abort err
            Right actions -> pure actions
          let deleteTypeAction = BranchUtil.makeAnnihilateTypeName split
              split = splitVar symbol
              insertTypeAction = BranchUtil.makeAddTypeName split (Reference.fromId typeRefId) Map.empty
              insertTypeConstructorActions =
                let referentIdsWithNames = zip (Decl.constructorVars (Decl.asDataDecl decl)) (Decl.declConstructorReferents typeRefId decl)
                 in map
                      ( \(sym, rid) ->
                          let splitConName = splitVar sym
                           in BranchUtil.makeAddTermName splitConName (Reference.fromId <$> rid) Map.empty
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
        & foldMap \(var, (_, ref, _, _, _)) ->
          let split = splitVar var
           in [ BranchUtil.makeAnnihilateTermName split,
                BranchUtil.makeAddTermName split (Referent.fromTermReferenceId ref) Map.empty
              ]

    splitVar :: Symbol -> Path.Split
    splitVar = Path.splitFromName . Name.unsafeFromVar

-- | get references from `names` that have the same names as in `defns`
-- For constructors, we get the type reference.
getExistingReferencesNamed :: Defns (Set Name) (Set Name) -> Names -> Set Reference
getExistingReferencesNamed defns names = fromTerms <> fromTypes
  where
    fromTerms = foldMap (\n -> Set.map Referent.toReference $ Relation.lookupDom n $ Names.terms names) (defns ^. #terms)
    fromTypes = foldMap (\n -> Relation.lookupDom n $ Names.types names) (defns ^. #types)

buildBigUnisonFile ::
  (forall a. Output -> Transaction a) ->
  Codebase IO Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Map Reference.Id ReferenceType ->
  Names ->
  Map ForwardName (Referent, Name) ->
  Transaction (UnisonFile Symbol Ann)
buildBigUnisonFile abort c tuf dependents names ctorNames =
  addDefinitionsToUnisonFile abort c names ctorNames dependents (UF.discardTypes tuf)

-- | @addDefinitionsToUnisonFile abort codebase names ctorNames definitions file@ adds all @definitions@ to @file@, avoiding
-- overwriting anything already in @file@. Every definition is put into the file with every naming it has in @names@ "on
-- the left-hand-side of the equals" (but yes type decls don't really have a LHS).
--
-- TODO: find a better module for this function, as it's used in a couple places
addDefinitionsToUnisonFile ::
  (forall void. Output -> Transaction void) ->
  Codebase IO Symbol Ann ->
  Names ->
  Map ForwardName (Referent, Name) ->
  Map Reference.Id ReferenceType ->
  UnisonFile Symbol Ann ->
  Transaction (UnisonFile Symbol Ann)
addDefinitionsToUnisonFile abort c names ctorNames dependents initialUnisonFile =
  -- for each dependent, add its definition with all its names to the UnisonFile
  foldM addComponent initialUnisonFile (Map.toList dependents')
  where
    dependents' :: Map Hash ReferenceType = Map.mapKeys (\(Reference.Id h _pos) -> h) dependents
    addComponent :: UnisonFile Symbol Ann -> (Hash, ReferenceType) -> Transaction (UnisonFile Symbol Ann)
    addComponent uf (h, rt) = case rt of
      Reference.RtTerm -> addTermComponent h uf
      Reference.RtType -> addDeclComponent abort h uf
    addTermComponent :: Hash -> UnisonFile Symbol Ann -> Transaction (UnisonFile Symbol Ann)
    addTermComponent h uf = do
      termComponent <- Codebase.unsafeGetTermComponent c h
      pure $ foldl' addTermElement uf (zip termComponent [0 ..])
      where
        addTermElement :: UnisonFile Symbol Ann -> ((Term Symbol Ann, Type Symbol Ann), Reference.Pos) -> UnisonFile Symbol Ann
        addTermElement uf ((tm, tp), i) = do
          let r :: Referent = Referent.Ref $ Reference.Derived h i
              termNames = Relation.lookupRan r names.terms
          foldl' (addDefinition tm tp) uf termNames
        addDefinition :: Term Symbol Ann -> Type Symbol Ann -> UnisonFile Symbol Ann -> Name -> UnisonFile Symbol Ann
        addDefinition tm tp uf (Name.toVar -> v) =
          if Set.member v termNames
            then uf
            else
              let prependTerm to = (v, Ann.External, tm) : to
               in if isTest tp
                    then uf & #watches . Lens.at WK.TestWatch . Lens.non [] Lens.%~ prependTerm
                    else uf & #terms Lens.%~ prependTerm
        termNames =
          Set.fromList [v | (v, _, _) <- uf.terms]
            <> foldMap (\x -> Set.fromList [v | (v, _, _) <- x]) uf.watches
    isTest = Typechecker.isEqual (Decls.testResultType mempty)

    -- given a dependent hash, include that component in the scratch file
    -- todo: wundefined: cut off constructor name prefixes
    addDeclComponent :: (forall a. Output -> Transaction a) -> Hash -> UnisonFile Symbol Ann -> Transaction (UnisonFile Symbol Ann)
    addDeclComponent abort h uf = do
      declComponent <- fromJust <$> Codebase.getDeclComponent h
      foldM addDeclElement uf (zip declComponent [0 ..])
      where
        -- for each name a decl has, update its constructor names according to what exists in the namespace
        addDeclElement :: UnisonFile Symbol Ann -> (Decl Symbol Ann, Reference.Pos) -> Transaction (UnisonFile Symbol Ann)
        addDeclElement uf (decl, i) = do
          let declNames = Relation.lookupRan (Reference.Derived h i) names.types
          -- look up names for this decl's constructor based on the decl's name, and embed them in the decl definition.
          foldM (addRebuiltDefinition decl) uf declNames
          where
            -- skip any definitions that already have names, we don't want to overwrite what the user has supplied
            addRebuiltDefinition :: (Decl Symbol Ann) -> UnisonFile Symbol Ann -> Name -> Transaction (UnisonFile Symbol Ann)
            addRebuiltDefinition decl uf name = case decl of
              Left ed ->
                overwriteConstructorNames name ed.toDataDecl >>= \case
                  ed' -> pure uf {UF.effectDeclarationsId = Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, Decl.EffectDeclaration ed') uf.effectDeclarationsId}
              Right dd ->
                overwriteConstructorNames name dd >>= \case
                  dd' -> pure uf {UF.dataDeclarationsId = Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, dd') uf.dataDeclarationsId}
        overwriteConstructorNames :: Name -> DataDeclaration Symbol Ann -> Transaction (DataDeclaration Symbol Ann)
        overwriteConstructorNames name dd =
          let constructorNames :: Transaction [Symbol]
              constructorNames = case findCtorNames names ctorNames (Just $ Decl.constructorCount dd) name of
                Left err -> abort err
                Right array ->
                  case traverse (fmap Name.toVar . Name.stripNamePrefix name) array of
                    Just varArray -> pure varArray
                    Nothing -> do
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

-- | O(r + c * d) touches all the referents (r), and all the NameSegments (d) of all of the Con referents (c)
forwardCtorNames :: Names -> Map ForwardName (Referent, Name)
forwardCtorNames names =
  Map.fromList $
    [ (ForwardName.fromName name, (r, name))
      | (r@Referent.Con {}, rNames) <- Map.toList $ Relation.range names.terms,
        name <- Foldable.toList rNames
    ]

-- | given a decl name, find names for all of its constructors, in order.
findCtorNames :: Names -> Map ForwardName (Referent, Name) -> Maybe Int -> Name -> Either Output.Output [Name]
findCtorNames names forwardCtorNames ctorCount n =
  let declRef = Set.findMin $ Relation.lookupDom n names.types
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
        else Left $ Output.UpdateIncompleteConstructorSet n m ctorCount

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

getTermAndDeclNames :: (Var v) => TypecheckedUnisonFile v a -> Defns (Set Name) (Set Name)
getTermAndDeclNames tuf = Defns (terms <> effectCtors <> dataCtors) (effects <> datas)
  where
    terms = keysToNames $ UF.hashTermsId tuf
    effects = keysToNames $ UF.effectDeclarationsId' tuf
    datas = keysToNames $ UF.dataDeclarationsId' tuf
    effectCtors = foldMap ctorsToNames $ fmap (Decl.toDataDecl . snd) $ UF.effectDeclarationsId' tuf
    dataCtors = foldMap ctorsToNames $ fmap snd $ UF.dataDeclarationsId' tuf
    keysToNames = Set.map Name.unsafeFromVar . Map.keysSet
    ctorsToNames = Set.fromList . map Name.unsafeFromVar . Decl.constructorVars

-- | Combines 'n' and 'nwh' then creates a ppe, but all references to
-- any name in 'n' are printed unqualified.
--
-- This is useful with the current update strategy where, for all
-- updates @#old -> #new@ we want to print dependents of #old and
-- #new, and have all occurrences of #old and #new be printed with the
-- unqualified name.
--
-- For this usecase the names from the scratch file are passed as 'n'
-- and the names from the codebase are passed in 'nwh'.
shadowNames :: Int -> Names -> NamesWithHistory -> PrettyPrintEnvDecl
shadowNames hashLen n nwh =
  let PPED.PrettyPrintEnvDecl unsuffixified0 suffixified0 = PPE.fromNamesDecl hashLen (Names.NamesWithHistory n mempty <> nwh)
      unsuffixified = patchPrettyPrintEnv unsuffixified0
      suffixified = patchPrettyPrintEnv suffixified0
      patchPrettyPrintEnv :: PrettyPrintEnv -> PrettyPrintEnv
      patchPrettyPrintEnv PPE.PrettyPrintEnv {termNames, typeNames} =
        PPE.PrettyPrintEnv
          { termNames = patch shadowedTermRefs termNames,
            typeNames = patch shadowedTypeRefs typeNames
          }
      patch shadowed f ref =
        let res = f ref
         in case Set.member ref shadowed of
              True -> map (second stripHashQualified) res
              False -> res
      stripHashQualified = \case
        HQ'.HashQualified b _ -> HQ'.NameOnly b
        HQ'.NameOnly b -> HQ'.NameOnly b
      shadowedTermRefs =
        let names = Relation.dom (Names.terms n)
            NamesWithHistory otherNames _ = nwh
            otherTermNames = Names.terms otherNames
         in Relation.ran (Names.terms n) <> foldMap (\a -> Relation.lookupDom a otherTermNames) names
      shadowedTypeRefs =
        let names = Relation.dom (Names.types n)
            NamesWithHistory otherNames _ = nwh
            otherTypeNames = Names.types otherNames
         in Relation.ran (Names.types n) <> foldMap (\a -> Relation.lookupDom a otherTypeNames) names
   in PPED.PrettyPrintEnvDecl unsuffixified suffixified
