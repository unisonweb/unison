{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,
    addDefinitionsToUnisonFile,
    prettyParseTypecheck,
  )
where

import Control.Lens (over, (^.))
import Control.Monad.RWS (ask)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra ((|>))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Reference (Reference, ReferenceType)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Ops
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
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Name.Forward (ForwardName (..))
import Unison.Name.Forward qualified as ForwardName
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Reference qualified as Reference (fromId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Server.Backend qualified as Backend
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.Util.Nametree (Defns (..))
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)

handleUpdate2 :: Cli ()
handleUpdate2 = do
  Cli.Env {codebase} <- ask
  tuf <- Cli.expectLatestTypecheckedFile

  -- - get add/updates from TUF
  let termAndDeclNames :: Defns (Set Name) (Set Name) = getTermAndDeclNames tuf

  currentBranch0 <- Cli.getCurrentBranch0
  let namesIncludingLibdeps = Branch.toNames currentBranch0
  let namesExcludingLibdeps = Branch.toNames (currentBranch0 & over Branch.children (Map.delete Name.libSegment))

  let ctorNames = forwardCtorNames namesExcludingLibdeps

  (pped, bigUf) <- Cli.runTransaction do
    dependents <-
      Ops.dependentsWithinScope
        (Names.referenceIds namesExcludingLibdeps)
        (getExistingReferencesNamed termAndDeclNames namesExcludingLibdeps)
    -- - construct PPE for printing UF* for typechecking (whatever data structure we decide to print)
    pped <- Codebase.hashLength <&> (`PPE.fromNamesDecl` (NamesWithHistory.fromCurrentNames namesIncludingLibdeps))
    bigUf <- buildBigUnisonFile codebase tuf dependents namesExcludingLibdeps
    let tufPped = PPE.fromNamesDecl 8 (Names.NamesWithHistory (UF.typecheckedToNames tuf) mempty)

    pure (pped `PPED.addFallback` tufPped, bigUf)

  -- - typecheck it
  prettyParseTypecheck bigUf pped >>= \case
    Left prettyUf -> do
      Cli.Env {isTranscript} <- ask
      maybePath <- if isTranscript then pure Nothing else Just . fst <$> Cli.expectLatestFile
      Cli.respond (Output.DisplayDefinitionsString maybePath prettyUf)
      Cli.respond Output.UpdateTypecheckingFailure
    Right tuf -> do
      Cli.respond Output.UpdateTypecheckingSuccess
      saveTuf (findCtorNames namesExcludingLibdeps ctorNames Nothing) tuf
      Cli.respond Output.Success

-- TODO: find a better module for this function, as it's used in a couple places
prettyParseTypecheck ::
  UnisonFile Symbol Ann ->
  PrettyPrintEnvDecl ->
  Cli (Either (Pretty Pretty.ColorText) (TypecheckedUnisonFile Symbol Ann))
prettyParseTypecheck bigUf pped = do
  typecheck <- mkTypecheckFnCli
  let prettyUf = Output.prettyUnisonFile pped bigUf
  let stringUf = Pretty.toPlain 80 prettyUf
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute currentPath)) rootBranch
  Cli.Env {generateUniqueName} <- ask
  uniqueName <- liftIO generateUniqueName
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = uniqueName,
            uniqueTypeGuid = Cli.loadUniqueTypeGuid currentPath,
            names = parseNames
          }
  Debug.whenDebug Debug.Update do
    liftIO do
      putStrLn "--- Scratch ---"
      putStrLn stringUf
  Cli.runTransaction do
    Parsers.parseFile "<update>" stringUf parsingEnv >>= \case
      Left {} -> pure $ Left prettyUf
      Right reparsedUf ->
        typecheck reparsedUf <&> \case
          Just reparsedTuf -> Right reparsedTuf
          Nothing -> Left prettyUf

mkTypecheckFnCli :: Cli (UnisonFile Symbol Ann -> Transaction (Maybe (TypecheckedUnisonFile Symbol Ann)))
mkTypecheckFnCli = do
  Cli.Env {codebase, generateUniqueName} <- ask
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute currentPath)) rootBranch
  pure (mkTypecheckFn codebase generateUniqueName currentPath parseNames)

mkTypecheckFn ::
  Codebase.Codebase IO Symbol Ann ->
  IO Parser.UniqueName ->
  Path.Absolute ->
  NamesWithHistory.NamesWithHistory ->
  UnisonFile Symbol Ann ->
  Transaction (Maybe (TypecheckedUnisonFile Symbol Ann))
mkTypecheckFn codebase generateUniqueName currentPath parseNames unisonFile = do
  uniqueName <- Sqlite.unsafeIO generateUniqueName
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = uniqueName,
            uniqueTypeGuid = Cli.loadUniqueTypeGuid currentPath,
            names = parseNames
          }
  typecheckingEnv <-
    computeTypecheckingEnvironment (FileParsers.ShouldUseTndr'Yes parsingEnv) codebase [] unisonFile
  let Result.Result _notes maybeTypecheckedUnisonFile = FileParsers.synthesizeFile typecheckingEnv unisonFile
  pure maybeTypecheckedUnisonFile

-- save definitions and namespace
saveTuf :: (Name -> [Name]) -> TypecheckedUnisonFile Symbol Ann -> Cli ()
saveTuf getConstructors tuf = do
  Cli.Env {codebase} <- ask
  currentPath <- Cli.getCurrentPath
  Cli.runTransaction $ Codebase.addDefsToCodebase codebase tuf
  Cli.stepAt
    "update"
    ( Path.unabsolute currentPath,
      Branch.batchUpdates (typecheckedUnisonFileToBranchUpdates getConstructors tuf)
    )

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
  (Name -> [Name]) ->
  TypecheckedUnisonFile Symbol a ->
  [(Path, Branch0 m -> Branch0 m)]
typecheckedUnisonFileToBranchUpdates getConstructors tuf =
  declUpdates ++ termUpdates
  where
    declUpdates :: [(Path, Branch0 m -> Branch0 m)]
    declUpdates =
      fold
        [ foldMap makeDataDeclUpdates (Map.toList $ UF.dataDeclarationsId' tuf),
          foldMap makeEffectDeclUpdates (Map.toList $ UF.effectDeclarationsId' tuf)
        ]
      where
        makeDataDeclUpdates (symbol, (typeRefId, dataDecl)) = makeDeclUpdates (symbol, (typeRefId, Right dataDecl))
        makeEffectDeclUpdates (symbol, (typeRefId, effectDecl)) = makeDeclUpdates (symbol, (typeRefId, Left effectDecl))
        makeDeclUpdates (symbol, (typeRefId, decl)) =
          let deleteTypeAction = BranchUtil.makeAnnihilateTypeName split
              -- some decls will be deleted, we want to delete their
              -- constructors as well
              deleteConstructorActions =
                map
                  (BranchUtil.makeAnnihilateTermName . Path.splitFromName)
                  (getConstructors (Name.unsafeFromVar symbol))
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
           in deleteStuff ++ addStuff

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
  Codebase IO Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Map Reference.Id ReferenceType ->
  Names ->
  Transaction (UnisonFile Symbol Ann)
buildBigUnisonFile c tuf dependents names =
  addDefinitionsToUnisonFile c names dependents (UF.discardTypes tuf)

-- | @addDefinitionsToUnisonFile codebase names definitions file@ adds all @definitions@ to @file@, avoiding overwriting
-- anything already in @file@. Every definition is put into the file with every naming it has in @names@ "on the
-- left-hand-side of the equals" (but yes type decls don't really have a LHS).
--
-- TODO: find a better module for this function, as it's used in a couple places
addDefinitionsToUnisonFile ::
  Codebase IO Symbol Ann ->
  Names ->
  Map Reference.Id ReferenceType ->
  UnisonFile Symbol Ann ->
  Transaction (UnisonFile Symbol Ann)
addDefinitionsToUnisonFile c names dependents initialUnisonFile =
  -- for each dependent, add its definition with all its names to the UnisonFile
  foldM addComponent initialUnisonFile (Map.toList dependents')
  where
    dependents' :: Map Hash ReferenceType = Map.mapKeys (\(Reference.Id h _pos) -> h) dependents
    addComponent :: UnisonFile Symbol Ann -> (Hash, ReferenceType) -> Transaction (UnisonFile Symbol Ann)
    addComponent uf (h, rt) = case rt of
      Reference.RtTerm -> addTermComponent h uf
      Reference.RtType -> addDeclComponent h uf
    ctorNames = forwardCtorNames names
    addTermComponent :: Hash -> UnisonFile Symbol Ann -> Transaction (UnisonFile Symbol Ann)
    addTermComponent h uf = do
      termComponent <- Codebase.unsafeGetTermComponent c h
      pure $ foldl' addTermElement uf (zip termComponent [0 ..])
      where
        addTermElement :: UnisonFile Symbol Ann -> ((Term Symbol Ann, Type Symbol Ann), Reference.Pos) -> UnisonFile Symbol Ann
        addTermElement uf ((tm, _tp), i) = do
          let r :: Referent = Referent.Ref $ Reference.Derived h i
              termNames = Relation.lookupRan r names.terms
          foldl' (addDefinition tm) uf termNames
        addDefinition :: Term Symbol Ann -> UnisonFile Symbol Ann -> Name -> UnisonFile Symbol Ann
        addDefinition tm uf (Name.toVar -> v) =
          if Set.member v termNames
            then uf
            else uf {UF.terms = (v, Ann.External, tm) : uf.terms}
        termNames = Set.fromList [v | (v, _, _) <- uf.terms]

    -- given a dependent hash, include that component in the scratch file
    -- todo: wundefined: cut off constructor name prefixes
    addDeclComponent :: Hash -> UnisonFile Symbol Ann -> Transaction (UnisonFile Symbol Ann)
    addDeclComponent h uf = do
      declComponent <- fromJust <$> Codebase.getDeclComponent h
      pure $ foldl' addDeclElement uf (zip declComponent [0 ..])
      where
        -- for each name a decl has, update its constructor names according to what exists in the namespace
        addDeclElement :: UnisonFile Symbol Ann -> (Decl Symbol Ann, Reference.Pos) -> UnisonFile Symbol Ann
        addDeclElement uf (decl, i) = do
          let declNames = Relation.lookupRan (Reference.Derived h i) names.types
          -- look up names for this decl's constructor based on the decl's name, and embed them in the decl definition.
          foldl' (addRebuiltDefinition decl) uf declNames
          where
            -- skip any definitions that already have names, we don't want to overwrite what the user has supplied
            addRebuiltDefinition decl uf name = case decl of
              Left ed -> uf {UF.effectDeclarationsId = Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, Decl.EffectDeclaration $ overwriteConstructorNames name ed.toDataDecl) uf.effectDeclarationsId}
              Right dd -> uf {UF.dataDeclarationsId = Map.insertWith (\_new old -> old) (Name.toVar name) (Reference.Id h i, overwriteConstructorNames name dd) uf.dataDeclarationsId}
        overwriteConstructorNames :: Name -> DataDeclaration Symbol Ann -> DataDeclaration Symbol Ann
        overwriteConstructorNames name dd =
          let constructorNames :: [Symbol]
              constructorNames =
                Name.toVar . fromJust . Name.stripNamePrefix name
                  <$> findCtorNames names ctorNames (Just $ Decl.constructorCount dd) name
              swapConstructorNames oldCtors =
                let (annotations, _vars, types) = unzip3 oldCtors
                 in zip3 annotations constructorNames types
           in over Decl.constructors_ swapConstructorNames dd

-- | O(r + c * d) touches all the referents (r), and all the NameSegments (d) of all of the Con referents (c)
forwardCtorNames :: Names -> Map ForwardName (Referent, Name)
forwardCtorNames names =
  Map.fromList $
    [ (ForwardName.fromName name, (r, name))
      | (r@Referent.Con {}, rNames) <- Map.toList $ Relation.range names.terms,
        name <- Foldable.toList rNames
    ]

-- | given a decl name, find names for all of its constructors, in order.
findCtorNames :: Names -> Map ForwardName (Referent, Name) -> Maybe Int -> Name -> [Name]
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
   in if Map.size m == ctorCountGuess && all (isJust . flip Map.lookup m) [0 .. fromIntegral ctorCountGuess - 1]
        then Map.elems m
        else error $ "incomplete constructor mapping for " ++ show n ++ ": " ++ show (Map.keys m) ++ " out of " ++ show ctorCountGuess

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

getTermAndDeclNames :: Var v => TypecheckedUnisonFile v a -> Defns (Set Name) (Set Name)
getTermAndDeclNames tuf = Defns (terms <> effectCtors <> dataCtors) (effects <> datas)
  where
    terms = keysToNames $ UF.hashTermsId tuf
    effects = keysToNames $ UF.effectDeclarationsId' tuf
    datas = keysToNames $ UF.dataDeclarationsId' tuf
    effectCtors = foldMap ctorsToNames $ fmap (Decl.toDataDecl . snd) $ UF.effectDeclarationsId' tuf
    dataCtors = foldMap ctorsToNames $ fmap snd $ UF.dataDeclarationsId' tuf
    keysToNames = Set.map Name.unsafeFromVar . Map.keysSet
    ctorsToNames = Set.fromList . map Name.unsafeFromVar . Decl.constructorVars

-- namespace:
-- type Foo = Bar Nat
-- baz = 4
-- qux = baz + 1

-- unison file:
-- Foo.Bar = 3
-- baz = 5
