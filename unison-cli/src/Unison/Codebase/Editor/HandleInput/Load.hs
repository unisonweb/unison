module Unison.Codebase.Editor.HandleInput.Load
  ( handleLoad,
    loadUnisonFile,
    EvalMode (..),
    evalUnisonFile,
    parseAndTypecheckUnisonFile,
    pulledInDefinitions,
    pulledInStarts,
    PulledIn (..),
    insertAboveFold,
    pulledInHeader,
  )
where

import Control.Lens ((.=), (?=))
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict qualified as State
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import System.Directory qualified as Directory
import System.Environment (withArgs)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.ABT qualified as ABT
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
import Unison.Codebase.Editor.HandleInput.EditNamespace (getNamesForEdit)
import Unison.Codebase.Editor.HandleInput.RuntimeUtils (EvalMode (..), modeProfSpec)
import Unison.Codebase.Editor.HandleInput.RuntimeUtils qualified as RuntimeUtils
import Unison.Codebase.Editor.HandleInput.ShowDefinition (renderToFile)
import Unison.Codebase.Editor.Input (RelativeToFold (AboveFold))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.SlurpResult (TermSlurp (..), TypeSlurp (..))
import Unison.Codebase.Execute qualified as Codebase
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.Runtime.Profile (ProfileSpec (NoProf))
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.DataDeclaration (DeclOrBuiltin)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.FileParsers qualified as FileParsers
import Unison.Merge (GUpdated (..))
import Unison.Name (Name)
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.OrBuiltin (OrBuiltin (..))
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Runtime (Error)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.TypeError qualified as TypeError
import Unison.Typed (Typed (..))
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.UnisonFile.Names qualified as UFN
import Unison.Util.Defns (Defns (..))
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Timing qualified as Timing
import Unison.Var qualified as Var
import Unison.WatchKind (WatchKind)
import Unison.WatchKind qualified as WatchKind

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
  let newPpe = PPED.suffixifiedPPE (PPED.makePPED (PPE.hqNamer 10 newNames) (PPE.suffixifyByHash newNames))
  pp <- Cli.getCurrentProjectPath

  -- If we're on an update, upgrade, or merge branch, we have special logic to render changes relative to the parent
  -- branch (in a merge branch, that's the branch we're merging into). This is because, on these branches, dependents
  -- have been pulled out of the underlying namespace and put into the scratch file (so that we can support deleting
  -- things). Without this special logic, all of these things would be classified as new definitions.

  maybeSpecialBranchParentCausalHash <-
    if pp.branch.isUpdate || pp.branch.isUpgrade || pp.branch.isMerge
      then case pp.branch.parentBranchId of
        Nothing -> pure Nothing
        Just parentBranchId ->
          Just <$> Cli.runTransaction (Queries.expectProjectBranchHeadHash pp.project.projectId parentBranchId)
      else pure Nothing

  (oldPpe, slurpEntries, existingTerms) <-
    case maybeSpecialBranchParentCausalHash of
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

        pure
          ( PPED.suffixifiedPPE (PPED.makePPED (PPE.hqNamer 10 oldNames) (PPE.suffixifyByHash oldNames)),
            slurpEntries,
            oldNames.terms
          )
      Just specialBranchParentCausalHash -> do
        specialBranchParent <- liftIO (Codebase.expectBranchForHash env.codebase specialBranchParentCausalHash)
        let specialBranchParent0 = Branch.head specialBranchParent
        let specialBranchParentNames = Branch.toNames specialBranchParent0
        let specialBranchParentLocalNames = Branch.toNames (Branch.deleteLibdeps specialBranchParent0)
        let specialBranchLocalNames =
              Names.shadowing unisonFileNames (Branch.toNames (Branch.deleteLibdeps oldBranch0))

        slurpEntries <-
          Cli.runTransaction do
            terms <-
              slurpTerms
                env.codebase
                unisonFile
                True
                (Relation.domain specialBranchParentLocalNames.terms)
                (Relation.domain specialBranchLocalNames.terms)
            types <-
              slurpTypes
                env.codebase
                unisonFile
                False
                (Relation.domain specialBranchParentLocalNames.types)
                (Relation.domain specialBranchLocalNames.types)
            pure Defns {terms, types}

        pure
          ( PPED.suffixifiedPPE $
              PPED.makePPED
                (PPE.hqNamer 10 specialBranchParentNames)
                (PPE.suffixifyByHash specialBranchParentNames),
            slurpEntries,
            specialBranchParentNames.terms
          )

  let aliases :: Map Referent (NESet Name)
      aliases =
        getTermAliases existingTerms slurpEntries.terms

  Cli.respond (Output.Typechecked oldPpe newPpe slurpEntries aliases pp.branch.isMerge)

  when (not . null $ UF.watchComponents unisonFile) do
    Timing.time "evaluating watches" do
      evalUnisonFile (Permissive NoProf) newPpe unisonFile [] >>= \case
        Right (bindings, e) -> do
          when (not (null e)) do
            let f (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
            Cli.respond $ Output.Evaluated text newPpe bindings (Map.map f e)
        Left err -> Cli.respond (Output.EvaluationFailure id err)

  #latestTypecheckedFile .= Just (Right unisonFile)

slurpTerms ::
  Codebase m Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Bool ->
  Map Name (Set Referent) ->
  Map Name (Set Referent) ->
  Sqlite.Transaction (Map Name TermSlurp)
slurpTerms codebase unisonFile isUpdate =
  Map.mergeA
    ( if isUpdate
        then Map.traverseMaybeMissing \_ refs ->
          case Set.findMin refs of
            Referent.Ref ref -> do
              ty <- Codebase.expectTypeOfTerm codebase ref
              pure (Just (TermSlurp'Delete (Typed ref ty)))
            Referent.Con _ _ -> pure Nothing
        else Map.dropMissing
    )
    ( Map.traverseMaybeMissing \name refs ->
        case Set.findMin refs of
          Referent.Ref ref -> do
            ty <- getNewRefType name ref
            pure (Just (TermSlurp'Add (Typed ref ty)))
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
                    pure (Just (TermSlurp'Update (Updated (Typed oldRef oldType) (Typed newRef newType))))
              (Referent.Con oldRef1 _, Referent.Ref newRef1) -> do
                oldType <- Codebase.expectTypeOfConstructor codebase oldRef1
                newType <- getNewRefType name newRef1
                pure (Just (TermSlurp'Update (Updated (Typed oldRef oldType) (Typed newRef newType))))
              (Referent.Ref oldRef1, Referent.Con newRef1 _) -> do
                oldType <- Codebase.expectTypeOfTerm codebase oldRef1
                newType <- getNewConType name newRef1
                pure (Just (TermSlurp'Update (Updated (Typed oldRef oldType) (Typed newRef newType))))
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
        Nothing ->
          -- This is super unlikely in practice (but has been observed in a transcript) - the term name matches an
          -- unnamed test watch's generated name. In this case, the map lookup above (by Name.toVar name) won't find the
          -- unnamed test watch, as it has a var type of UnnamedWatch, not User.
          case Map.lookup name unnamedTestWatchesByName of
            Nothing -> Codebase.expectTypeOfTerm codebase ref
            Just ty -> pure ty

    unnamedTestWatchesByName :: Map Name (Type Symbol Ann)
    unnamedTestWatchesByName =
      foldr f Map.empty unisonFile.watchComponents
      where
        f ::
          (WatchKind, [(Symbol, Ann, Term Symbol Ann, Type Symbol Ann)]) ->
          Map Name (Type Symbol Ann) ->
          Map Name (Type Symbol Ann)
        f (WatchKind.TestWatch, component) acc = foldr g acc component
        f _ acc = acc

        g ::
          (Symbol, Ann, Term Symbol Ann, Type Symbol Ann) ->
          Map Name (Type Symbol Ann) ->
          Map Name (Type Symbol Ann)
        g (var, _, _, ty) acc =
          case Var.typeOf var of
            Var.UnnamedWatch _ _ -> Map.insert (Name.unsafeParseVar var) ty acc
            _ -> acc

slurpTypes ::
  Codebase m Symbol Ann ->
  TypecheckedUnisonFile Symbol Ann ->
  Bool ->
  Map Name (Set TypeReference) ->
  Map Name (Set TypeReference) ->
  Sqlite.Transaction (Map Name TypeSlurp)
slurpTypes codebase unisonFile isUpdate =
  Map.mergeA
    ( if isUpdate
        then Map.traverseMissing \_ -> fmap TypeSlurp'Delete . getOldDecl . Set.findMin
        else Map.dropMissing
    )
    (Map.traverseMissing \name -> fmap TypeSlurp'Add . getNewDecl name . Set.findMin)
    ( Map.zipWithMaybeAMatched \name oldRefs newRefs ->
        let oldRef = Set.findMin oldRefs
            newRef = Set.findMin newRefs
         in if oldRef /= newRef
              then fmap Just do
                oldDecl <- getOldDecl oldRef
                newDecl <- getNewDecl name newRef
                pure (TypeSlurp'Update (Updated oldDecl newDecl))
              else
                pure
                  if isUpdate
                    then case UF.lookupDecl (Name.toVar name) unisonFile of
                      Nothing -> Nothing
                      Just _ -> Just TypeSlurp'Unchanged
                    else Just TypeSlurp'Unchanged
    )
  where
    getOldDecl :: TypeReference -> Sqlite.Transaction (DeclOrBuiltin Symbol Ann)
    getOldDecl = \case
      Reference.DerivedId ref -> NotBuiltin <$> Codebase.unsafeGetTypeDeclaration codebase ref
      Reference.Builtin builtin -> pure (Builtin (Builtin.expectBuiltinConstructorType builtin))
    getNewDecl :: Name -> TypeReference -> Sqlite.Transaction (DeclOrBuiltin Symbol Ann)
    getNewDecl name = \case
      Reference.DerivedId ref ->
        case UF.lookupDecl (Name.toVar name) unisonFile of
          Just (_, decl) -> pure (NotBuiltin decl)
          Nothing -> NotBuiltin <$> Codebase.unsafeGetTypeDeclaration codebase ref
      Reference.Builtin builtin -> pure (Builtin (Builtin.expectBuiltinConstructorType builtin))

getTermAliases :: Relation Name Referent -> Map Name TermSlurp -> Map Referent (NESet Name)
getTermAliases existingTerms slurpTerms =
  -- For the purpose of identifying aliases to call out, we omit names that are changing by this update.
  let (changedNames, changedRefs) =
        Map.foldlWithKey'
          ( \ ~acc@(names, refs) name -> \case
              TermSlurp'Add (Typed ref _) ->
                let !names1 = Set.insert name names
                    !refs1 = Set.insert (Referent.Ref ref) refs
                 in (names1, refs1)
              TermSlurp'Delete (Typed ref _) ->
                let !names1 = Set.insert name names
                    !refs1 = Set.insert (Referent.Ref ref) refs
                 in (names1, refs1)
              TermSlurp'Update (Updated (Typed old _) (Typed new _)) ->
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

-- | A definition of the codebase that the file uses, and that itself uses something the file redefines, is made part of
-- the file, unchanged (its source is added at the end), so that the file typechecks against one version of what it
-- redefines and `update` saves that definition rebuilt. Without this, the file sees two types of the same name: its own,
-- and the codebase's through that definition. The same goes for what those definitions use, transitively, as long as
-- it depends on the redefinition.
pullInDependentsOfRedefinitions ::
  Codebase IO Symbol Ann ->
  Names ->
  Parser.ParsingEnv Sqlite.Transaction ->
  Text ->
  Text ->
  UF.UnisonFile Symbol Ann ->
  -- | The text and the file as typechecked, and what was pulled in
  Cli (Text, UF.UnisonFile Symbol Ann, Maybe PulledIn)
pullInDependentsOfRedefinitions codebase names parsingEnv sourceName text uf =
  liftIO (pulledInDefinitions codebase names uf) >>= \case
    Nothing -> pure (text, uf, Nothing)
    Just (pulledNames, redefinedNames, source) -> do
      let (text', firstLine, lastLine) = insertAboveFold text (pulledInHeader redefinedNames <> source)
      uf' <-
        Cli.runTransaction (Parsers.parseFile (Text.unpack sourceName) (Text.unpack text') parsingEnv)
          & onLeftM \err -> Cli.returnEarly (Output.ParseErrors text' [err])
      -- When the source is a real file, the definitions go into it, so that errors point at lines the user can see and
      -- the user edits them like the rest. A `unison` block of a transcript has no file to write.
      env <- ask
      let path = Text.unpack sourceName
      written <- liftIO (Directory.doesFileExist path)
      when written do
        liftIO do
          Directory.removeFile path
          env.writeSource sourceName text' False
        -- The file watcher will report this write; it isn't a change of the user's.
        #latestFile ?= (path, True)
      Cli.respond (Output.PulledInDependents pulledNames written)
      pure (text', uf', Just PulledIn {names = pulledNames, starts = pulledInStarts pulledNames uf', firstLine, lastLine, written})

-- | The comment that goes above pulled-in definitions in a file, naming what the file redefines.
pulledInHeader :: [Name] -> Text
pulledInHeader redefined =
  Text.unlines
    [ "-- The definitions below are from the codebase. `load` added them here because they use "
        <> Text.intercalate ", " (map (\n -> "`" <> Name.toText n <> "`") redefined)
        <> ", which this file",
      "-- redefines, so they are typechecked with the file and `update` saves them rebuilt. Change them if the new definition",
      "-- doesn't fit the way they use it; they are yours to edit, like the rest of the file.",
      ""
    ]

-- | What 'pullInDependentsOfRedefinitions' added to a file: the names, where each starts in the text as typechecked
-- (1-based lines), and the first and last line of the added region.
data PulledIn = PulledIn
  { names :: [Name],
    starts :: [(Int, Name)],
    firstLine :: Int,
    lastLine :: Int,
    -- | Whether the definitions were written into the file, so that its lines are the typechecked text's
    written :: Bool
  }

-- | The file's text with source added where Unison still reads it: before the first fold line (a line starting with
-- `---`, which hides the rest of the file), or else at the end. Also the first and last line (1-based) of the added
-- source in the result.
insertAboveFold :: Text -> Text -> (Text, Int, Int)
insertAboveFold text source =
  let (above, below) = break (Text.isPrefixOf "---" . Text.stripStart) (Text.lines text)
      firstLine = length above + 2
      sourceLines = Text.lines source
   in ( Text.unlines (above ++ [""] ++ sourceLines ++ [""] ++ below),
        firstLine,
        firstLine + max 0 (length sourceLines - 1)
      )

-- | The line (1-based) on which each of the named definitions starts in a parsed file, in order. For the definitions
-- 'pulledInDefinitions' added, which come after the file's own lines, this says which definition an error line is in.
pulledInStarts :: [Name] -> UF.UnisonFile Symbol Ann -> [(Int, Name)]
pulledInStarts names uf =
  sortOn fst [(line, name) | name <- names, Just line <- [start (Name.toVar name)]]
  where
    start v =
      (Map.lookup v (UF.terms uf) >>= Ann.startingLine . fst)
        <|> (Map.lookup v (UF.dataDeclarationsId uf) >>= Ann.startingLine . DataDeclaration.annotation . snd)
        <|> (Map.lookup v (UF.effectDeclarationsId uf) >>= Ann.startingLine . DataDeclaration.annotation . DataDeclaration.toDataDecl . snd)

-- | The name of the pulled-in definition that a line of the typechecked text is in, if it is in the pulled-in region.
pulledInDefinitionAt :: PulledIn -> Int -> Maybe Name
pulledInDefinitionAt pulled line
  | line < pulled.firstLine || line > pulled.lastLine = Nothing
  | otherwise = snd <$> lastMay (takeWhile ((<= line) . fst) pulled.starts)

-- | The line a type error points at, when it points somewhere.
typeErrorLine :: Context.ErrorNote Symbol Ann -> Maybe Int
typeErrorLine note =
  site (TypeError.typeErrorFromNote note) >>= Ann.startingLine
  where
    site = \case
      TypeError.Mismatch {mismatchSite} -> Just (ABT.annotation mismatchSite)
      TypeError.BooleanMismatch {mismatchSite} -> Just (ABT.annotation mismatchSite)
      TypeError.ExistentialMismatch {mismatchSite} -> Just (ABT.annotation mismatchSite)
      TypeError.ActionRestrictionFailure {mismatchSite} -> Just (ABT.annotation mismatchSite)
      TypeError.FunctionUnderApplied {mismatchSite} -> Just (ABT.annotation mismatchSite)
      TypeError.FunctionApplication {f} -> Just (ABT.annotation f)
      TypeError.NotFunctionApplication {f} -> Just (ABT.annotation f)
      TypeError.AbilityCheckFailure {abilityCheckFailureSite} -> Just abilityCheckFailureSite
      TypeError.AbilitySubtypeFailure {abilityCheckFailureSite} -> Just abilityCheckFailureSite
      TypeError.AbilityEqFailure {abilityCheckFailureSite} -> Just abilityCheckFailureSite
      TypeError.UnknownType {typeSite} -> Just typeSite
      TypeError.UnknownTerm {termSite} -> Just termSite
      TypeError.RedundantPattern loc -> Just loc
      TypeError.UncoveredPatterns loc _ -> Just loc
      _ -> Nothing

-- | The definitions of the codebase to make part of a parsed file (see 'pullInDependentsOfRedefinitions'): their names,
-- and their source, rendered with the given names. The LSP uses this too, on the file as it is being edited.
pulledInDefinitions :: Codebase IO Symbol Ann -> Names -> UF.UnisonFile Symbol Ann -> IO (Maybe ([Name], [Name], Text))
pulledInDefinitions codebase names uf = do
  let fileTermNames = Set.map Name.unsafeParseVar (UFN.toTermAndWatchNames uf)
      fileTypeNames = Set.map Name.unsafeParseVar (Map.keysSet (UF.dataDeclarationsId uf) <> Map.keysSet (UF.effectDeclarationsId uf))
      redefined =
        Defns
          { terms = foldMap (Names.refTermsNamed names) fileTermNames,
            types = foldMap (Names.typesNamed names) fileTypeNames
          }
  if Set.null redefined.terms && Set.null redefined.types
    then pure Nothing
    else do
      let scope =
            Defns
              (Set.fromList [id | Referent.Ref ref <- Set.toList (Relation.ran names.terms), Just id <- [Reference.toId ref]])
              (Set.fromList (mapMaybe Reference.toId (Set.toList (Relation.ran names.types))))
      (pulledNames, types, terms) <-
        Codebase.runTransaction codebase do
          dependents <- Operations.transitiveDependentsWithinScope scope redefined
          let -- The dependents that the file doesn't define itself (its own version of a dependent is the one to use).
              affected :: Defns (Set TermReference) (Set TypeReference)
              affected =
                Defns
                  (Set.map Reference.fromId dependents.terms `Set.difference` redefined.terms)
                  (Set.map Reference.fromId dependents.types `Set.difference` redefined.types)
              used = UF.dependencies uf
              start = Defns (Set.intersection used.terms affected.terms) (Set.intersection used.types affected.types)
              -- What the pulled-in definitions use, as long as it is affected too.
              close acc = do
                termDeps <- for (Set.toList acc.terms) \ref ->
                  maybe mempty Term.dependencies <$> maybe (pure Nothing) (Codebase.getTerm codebase) (Reference.toId ref)
                typeDeps <- for (Set.toList acc.types) \ref ->
                  maybe mempty (Defns Set.empty . DataDeclaration.typeDependencies . DataDeclaration.asDataDecl)
                    <$> maybe (pure Nothing) (Codebase.getTypeDeclaration codebase) (Reference.toId ref)
                let more = fold termDeps <> fold typeDeps
                    acc' =
                      Defns
                        (Set.union acc.terms (Set.intersection more.terms affected.terms))
                        (Set.union acc.types (Set.intersection more.types affected.types))
                if acc' == acc then pure acc else close acc'
          pulled <- close start
          let pulledNames =
                Names
                  { terms = Relation.restrictDom (Set.map Referent.Ref pulled.terms) (Relation.swap names.terms) & Relation.swap,
                    types = Relation.restrictDom pulled.types (Relation.swap names.types) & Relation.swap
                  }
          (types, terms) <- getNamesForEdit codebase ppe pulledNames
          pure (Set.toList (Relation.dom pulledNames.terms <> Relation.dom pulledNames.types), types, terms)
      if null pulledNames
        then pure Nothing
        else do
          rendered <- newIORef Nothing
          _ <- renderToFile codebase (const True) (\_ txt _ -> writeIORef rendered (Just txt)) Nothing "pulled" AboveFold ppe terms types
          let redefinedNames =
                Set.toList (Set.filter (not . Set.null . Names.refTermsNamed names) fileTermNames)
                  <> Set.toList (Set.filter (not . Set.null . Names.typesNamed names) fileTypeNames)
          fmap (pulledNames,redefinedNames,) <$> readIORef rendered
  where
    ppe = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHashName names)

parseAndTypecheckUnisonFile ::
  Names ->
  Text ->
  Text ->
  Cli (TypecheckedUnisonFile Symbol Ann)
parseAndTypecheckUnisonFile names sourceName text0 = do
  let text = text0
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
  unisonFile0 <-
    Cli.runTransaction (Parsers.parseFile (Text.unpack sourceName) (Text.unpack text) parsingEnv)
      & onLeftM \err -> Cli.returnEarly (Output.ParseErrors text [err])
  (text, unisonFile, pulledIn) <- pullInDependentsOfRedefinitions codebase names parsingEnv sourceName text unisonFile0
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
              -- A definition can still mention something the file shadowed. Name it by its name in the codebase, with
              -- the hash to tell it apart from the file's definition of that name.
              `PPED.addFallback` PPED.makePPED (PPE.alwaysHqNamer 10 names) (PPE.suffixifyByHash names)

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
      -- An error in a definition that was pulled in is in code the user didn't write: say which definition it is in.
      for_ pulledIn \pulled -> when (not pulled.written) do
        let inPulledIn = Set.toList (Set.fromList (mapMaybe (typeErrorLine >=> pulledInDefinitionAt pulled) tes))
        when (not (null inPulledIn)) do
          Cli.respond (Output.TypeErrorsInPulledIn pulled.firstLine pulled.lastLine inPulledIn)
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
        Error
        ( [(Symbol, Term Symbol ())],
          Map Symbol (Ann, WatchKind, Reference.Id, Term Symbol (), Term Symbol (), Bool)
        )
    )
evalUnisonFile mode ppe unisonFile args = do
  env <- ask

  let theRuntime = case mode of
        Sandboxed -> env.sandboxedRuntime
        Permissive _ -> env.runtime
      prof = modeProfSpec mode

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction env.codebase (Codebase.lookupWatchCache env.codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  Cli.with_ (withArgs args) do
    let codeLookup = Codebase.codebaseToCodeLookup env.codebase
    liftIO (Runtime.evaluateWatches codeLookup ppe prof watchCache theRuntime unisonFile) >>= \case
      Right (nts, resp, map) -> do
        cache <- case resp of
          Runtime.DecompErrs errs
            | not $ null errs ->
                False <$ RuntimeUtils.displayDecompileErrors errs
          Runtime.Profile prof ->
            True <$ Cli.respond (Output.Literal prof)
          _ -> pure True
        for_ (Map.elems map) \(_loc, kind, hash, _src, value, isHit) -> do
          -- only update the watch cache when there are no errors
          when (not isHit && cache) do
            let value' = Term.amap (\() -> Ann.External) value
            Cli.runTransaction (Codebase.putWatch kind hash value')
        pure (Right (nts, map))
      Left err -> pure (Left err)
