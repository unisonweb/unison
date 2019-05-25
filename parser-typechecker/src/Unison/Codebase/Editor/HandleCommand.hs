{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# OPTIONS_GHC -Wno-unused-matches #-}

-- {-# LANGUAGE DeriveAnyClass,StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.HandleCommand where

import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.RemoteRepo

-- import Debug.Trace

import           Data.Functor                   ( void )
import           Data.Text                      ( Text
                                                )
import           Unison.Codebase2               ( Codebase )
import qualified Unison.Codebase.Classes       as CC
import qualified Unison.Codebase2              as Codebase
import           Unison.Codebase.Branch2         ( Branch
                                                 )
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.Names                  as OldNames
import           Unison.Parser                  ( Ann )
import qualified Unison.Parser                 as Parser
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import qualified Unison.Codebase.Runtime       as Runtime
import           Unison.Codebase.Runtime       (Runtime)
import qualified Unison.Type                   as Type
import qualified Unison.UnisonFile             as UF
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Var                     ( Var )

-- instance Semigroup NameChangeResult where (<>) = mappend
-- instance Monoid NameChangeResult where
--   mempty = NameChangeResult mempty mempty mempty
--   NameChangeResult a1 a2 a3 `mappend` NameChangeResult b1 b2 b3 =
--     NameChangeResult (a1 <> b1) (a2 <> b2) (a3 <> b3)
--
-- -- Function for handling name collisions during a file slurp into the codebase.
-- -- Returns `True` if the given symbol may be treated as an update.
-- type OkToUpdate = Bool
-- type CollisionHandler = Name -> OkToUpdate
--
-- -- All collisions get treated as updates and are added to successful.
-- updateCollisionHandler :: CollisionHandler
-- updateCollisionHandler = const True
--
-- -- All collisions get left alone and are not added to successes.
-- addCollisionHandler :: CollisionHandler
-- addCollisionHandler = const False

data SearchMode = FuzzySearch | ExactSearch

-- -- Edits stuff:
--   Todo :: Edits -> Branch -> Command m i v (TodoOutput v Ann)
--
--   Propagate :: Edits -> Branch -> Command m i v (Branch m)


-- may need to be different for private repo?
loadGithubRootBranch :: Text -> Text -> Text -> m (Branch m)
loadGithubRootBranch _user _repo _treeish = error "todo: loadGithubRootBranch"

syncGithubRootBranch :: Text -> Text -> Text -> Branch m -> m ()
syncGithubRootBranch _user _repo _ghbranch _b = error "todo: syncGithubRootBranch"

-- data Outcome
--   -- New definition that was added to the branch
--   = Added
--   -- A name collision that was treated as a replacement
--   | Updated
--   -- A name collision that couldn't be treated as a replacement (use `update`)
--   | CouldntUpdate
--   -- A name that couldn't be updated because it's currently conflicted
--   | CouldntUpdateConflicted
--   -- Skipped because it already exist in the branch (with same name)
--   | AlreadyExists
--   -- Skipped because it already exist in the branch (with different name(s))
--   | RequiresAlias [Name]
--   -- Skipped terms because they share a name with existing constructor
--   | TermExistingConstructorCollision
--   -- Skipped types because one or more constructors collides with an existing term
--   -- (This could include the constructors of another type)
--   | ConstructorExistingTermCollision [Referent]
--   -- Skipped because at least 1 of its dependencies couldn't be added
--   -- or doesn't already exist in branch
--   | CouldntAddDependencies (Set Reference)
--
-- blocksDependent :: Outcome -> Bool
-- blocksDependent = \case
--   Added -> False
--   Updated -> False
--   AlreadyExists -> False
--   RequiresAlias _ -> False
--   _ -> True
--
-- outcomes :: Var v
--          => CollisionHandler
--          -> Branch0
--          -> UF.TypecheckedUnisonFile v Ann
--          -> [(Either Reference Reference, Outcome)]
-- outcomes okToUpdate b file = let
--   namesForTermOrType r b =
--     Branch.namesForType r b <> Branch.namesForTerm (Referent.Ref r) b
--   outcome0 n r0 ctorNames = let r = either id id r0 in
--     if Branch.contains b r then -- Already exists
--       case toList $ namesForTermOrType r b of
--         -- note: this will only return names from appropriate namespace
--         ns | n `elem` ns -> (r0, AlreadyExists)
--            | otherwise   -> (r0, RequiresAlias ns)
--     else case r0 of -- this doesn't exist in the branch
--       -- It's a term
--       Right _ -> case toList $ Branch.termsNamed n b of
--         [] -> (r0, Added)
--         referents ->
--           if not (okToUpdate n) then (r0, CouldntUpdate)
--           else if length referents > 1
--           then (r0, CouldntUpdateConflicted)
--           else if any Referent.isConstructor referents
--           then (r0, TermExistingConstructorCollision)
--           else (r0, Updated)
--       -- It's a type
--       Left _ -> let
--         ctorNameCollisions :: Set Referent
--         ctorNameCollisions = Set.unions $
--           map (`Branch.termsNamed` b) ctorNames
--         in case toList $ Branch.typesNamed n b of
--           [] -> -- no type name collisions
--             if null ctorNameCollisions
--             then (r0, Added) -- and no term collisions
--             else (r0, ConstructorExistingTermCollision $ toList ctorNameCollisions)
--           _refs | not (okToUpdate n) -> (r0, CouldntUpdate)
--           [oldref] -> let
--             conflicted = toList ctorNameCollisions >>= \r2 -> case r2 of
--               Referent.Ref _ -> [r2]
--               -- note - it doesn't count as a collision if the name
--               -- collision is on a ctor of the type we're replacing
--               -- of the type we will be replacing
--               Referent.Con r _ -> if r == oldref then [] else [r2]
--             in if null conflicted then (r0, Updated)
--                else (r0, ConstructorExistingTermCollision conflicted)
--           _otherwise -> (r0, CouldntUpdateConflicted) -- come back to this
--
--   outcomes0terms = map termOutcome (Map.toList $ UF.hashTerms file)
--   termOutcome (v, (r, _, _)) = outcome0 (Name.unsafeFromVar v) (Right r) []
--   outcomes0types =
--     map typeOutcome (Map.toList . fmap (second Right) $ UF.dataDeclarations' file)
--       ++ map typeOutcome
--              (Map.toList . fmap (second Left) $ UF.effectDeclarations' file)
--   typeOutcome (v, (r, dd)) =
--     outcome0 (Name.unsafeFromVar v) (Left r) $ ctorNames v r dd
--   ctorNames v r (Left e) =
--     Map.keys $ Names.termNames (DD.effectDeclToNames v r e)
--   ctorNames v r (Right dd) =
--     Map.keys $ Names.termNames (DD.dataDeclToNames v r dd)
--   outcomes0 = outcomes0terms ++ outcomes0types
--   in removeTransitive (UF.dependencies' file) outcomes0
--
-- -- Converts outcomes to CouldntAddDependencies if it is a successful outcome
-- -- which depends (directly or indirectly) on a Reference with an unsuccessful
-- -- outcome.
-- removeTransitive
--   :: Relation Reference Reference
--   -> [(Either Reference Reference, Outcome)]
--   -> [(Either Reference Reference, Outcome)]
-- removeTransitive dependencies outcomes0 = let
--   ref r = either id id r
--   -- `Set Reference` that have been removed
--   removed0 = Set.fromList [ ref r | (r, o) <- outcomes0, blocksDependent o ]
--   trim :: Set Reference
--        -> [(Either Reference Reference, Outcome)]
--        -> [(Either Reference Reference, Outcome)]
--   trim removedAlready outcomes = let
--     outcomes' = map stepOutcome outcomes
--     stepOutcome (r, o) =
--       let
--         -- dependencies of r which have already been marked for removal
--         removedDeps =
--           Set.intersection removedAlready (R.lookupDom (ref r) dependencies)
--       in
--         -- if r's outcome is already a sort of failure, then keep that outcome
--         if blocksDependent o then (r, o)
--         -- or if none of r's dependencies are removed, then don't change r's outcome
--         else if Set.null removedDeps then (r, o)
--         -- else some of r's deps block r
--         else (r, CouldntAddDependencies removedDeps)
--     removed = Set.fromList [ ref r | (r, o) <- outcomes', blocksDependent o ]
--     in if Set.size removed == Set.size removedAlready then outcomes
--        else trim removed outcomes'
--   in trim removed0 outcomes0
--
-- -- Handles add/update command for a typechecked file
-- fileToBranch
--   :: forall m v
--   .  (Var v, Monad m)
--   => CollisionHandler
--   -> Codebase m v Ann
--   -> Branch
--   -> UF.TypecheckedUnisonFile v Ann
--   -> m (SlurpResult v)
-- fileToBranch handleCollisions codebase branch uf = do
--   -- Write out all the successful outcomes to the codebase
--   forM_ outcomes0 $ \(r, o) ->
--     case o of
--       Added -> writeDefinition r
--       Updated -> writeDefinition r
--       _ -> pure ()
--   -- Accumulate the final slurp result and the updated Branch,
--   -- by folding over the outcomes.
--   (result, b0) <- foldM addOutcome
--     (SlurpResult uf branch mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty, Branch.head branch) outcomes'
--   -- todo: be a little smarter about avoiding needless propagation
--   b0 <- Codebase.propagate codebase b0
--   pure $ result { updatedBranch = Branch.cons b0 branch }
--   where
--     b0 = Branch.head branch
--     outcomes0 = outcomes handleCollisions b0 uf
--     outcomes' = map addV outcomes0 where
--       addV (r, o) = case r of
--         Left tr -> case Map.lookup tr declsByRef of
--           Nothing -> error "Panic. Unknown type in fileToBranch."
--           Just (v, dd) -> (v, bimap (,dd) id r, o)
--         Right er -> case Map.lookup er termsByRef of
--           Nothing -> error "Panic. Unknown term in fileToBranch"
--           Just (v, _, _) -> (v, Right er, o)
--     -- converts a term or type reference, r, to a SlurpComponent
--     sc r v = case r of
--       Left _ -> SlurpComponent (Set.singleton v) mempty
--       Right _ -> SlurpComponent mempty (Set.singleton v)
--     -- The folding function: state is the SlurpResult and the accumulated Branch0,
--     -- And each outcome is used to update this state.
--     addOutcome (result, b) (v, r, o) = case o of
--       Added -> pure $ case r of
--         Left (r, dd) ->
--           ( result { adds = adds result <> SlurpComponent (Set.singleton v) mempty }
--           , Branch.fromDeclaration v r dd <> b )
--         Right r ->
--           ( result { adds = adds result <> SlurpComponent mempty (Set.singleton v) }
--           , Branch.addTermName (Referent.Ref r) (Name.unsafeFromVar v) b )
--       Updated -> do
--         let result' = result { updates = updates result <> sc r v }
--             name = Name.unsafeFromVar v
--         case r of
--           Left (r', dd) -> case toList (Branch.typesNamed name b0) of
--             [r0] -> pure (result', Branch.fromDeclaration v r' dd <> Branch.replaceType r0 r' b)
--             _ -> error "Panic. Tried to replace a type that's conflicted."
--           Right r' -> case toList (Branch.termsNamed name b0) of
--             [Referent.Ref r0] -> do
--               Just type1 <- Codebase.getTypeOfTerm codebase r0
--               let Just (_, _, type2) = Map.lookup r' termsByRef
--               let typing =
--                     if Typechecker.isEqual type1 type2 then TermEdit.Same
--                     else if Typechecker.isSubtype type2 type1 then TermEdit.Subtype
--                     else TermEdit.Different
--               pure (result', Branch.addTermName (Referent.Ref r') name $
--                              Branch.replaceTerm r0 r' typing b)
--             _ -> error $ "Panic. Tried to replace a term that's conflicted." ++ show v
--       AlreadyExists -> pure (result { duplicates = duplicates result <> sc r v }, b)
--       CouldntUpdate -> pure (result { collisions = collisions result <> sc r v }, b)
--       CouldntUpdateConflicted ->
--         pure (result { conflicts = conflicts result <> sc r v }, b)
--       RequiresAlias ns -> let
--         name = Name.unsafeFromVar v
--         rcs = case r of
--           Left _ -> Branch.RefCollisions mempty (R.fromList $ (name,) <$> ns)
--           Right _ -> Branch.RefCollisions (R.fromList $ (name,) <$> ns) mempty
--         in pure (result { needsAlias = needsAlias result <> rcs }, b)
--       TermExistingConstructorCollision ->
--         pure (result {
--           termExistingConstructorCollisions =
--             termExistingConstructorCollisions result <>
--             pick (toList $ Branch.constructorsNamed (Name.unsafeFromVar v) b0) }, b)
--         where
--           pick [] = error "Panic. Incorrectly determined a conflict."
--           pick (h:_) = Map.fromList [(v, h)]
--       ConstructorExistingTermCollision rs ->
--         pure (result { constructorExistingTermCollisions = constructorExistingTermCollisions result <> Map.fromList [(v, rs)] }, b)
--       CouldntAddDependencies rs -> case r of
--         Left _ -> pure (result { typesWithBlockedDependencies =
--           typesWithBlockedDependencies result <> Map.fromList [(v, rs)] }, b)
--         Right _ -> pure (
--           result { termsWithBlockedDependencies = termsWithBlockedDependencies result <>
--                    Map.fromList [(v, rs)] }, b)
--     declsByRef :: Map Reference (v, Decl v Ann)
--     declsByRef = Map.fromList $
--       [ (r, (v, d)) | (v, (r,d)) <- mconcat [
--           Map.toList . fmap (second Right) $ UF.dataDeclarations' uf
--         , Map.toList . fmap (second Left) $ UF.effectDeclarations' uf ]]
--     termsByRef = Map.fromList
--       [ (r, (v, tm, typ))
--       | (v, (r, tm, typ)) <- Map.toList $ UF.hashTerms uf ]
--     prepTerm = Term.amap (const Parser.External)
--     prepDecl :: Decl v Ann -> Decl v Ann
--     prepDecl = bimap ex ex
--     ex :: Functor f => f a -> f Ann
--     ex = fmap (const Parser.External)
--     writeDefinition = \case
--       Left typeRef@(DerivedId d) -> let
--         Just (_, dd) = Map.lookup typeRef declsByRef
--         in Codebase.putTypeDeclaration codebase d (prepDecl dd)
--       Right termRef@(DerivedId d) -> let
--         Just (_, tm, typ) = Map.lookup termRef termsByRef
--         in Codebase.putTerm codebase d (prepTerm tm) (ex typ)
--       r -> error $ "Panic. Hashing produced a builtin Reference: " ++ show r

typecheck
  :: (Monad m, Var v)
  => [Type.AnnotatedType v Ann]
  -> Codebase m v Ann
  -> Parser.ParsingEnv
  -> SourceName
  -> Text
  -> m (TypecheckingResult v)
typecheck ambient codebase names sourceName src =
  error "todo: update to use Names2 instead of Names"
  ambient codebase names sourceName src
  -- Result.getResult $ parseAndSynthesizeFile ambient
  --   (((<> B.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
  --   names
  --   (unpack sourceName)
  --   src

-- -- Contains all the builtins
-- builtinBranch :: Branch
-- builtinBranch = Branch.one builtinBranch0
--
-- builtinBranch0 :: Branch0
-- builtinBranch0 =
--   (  Branch.fromNames B.names
--   <> Branch.fromTypecheckedFile IOSource.typecheckedFile
--   )
--
-- newBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
-- newBranch codebase branch branchName = forkBranch codebase branch branchName
--
-- forkBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
-- forkBranch codebase branch branchName = do
--   ifM (Codebase.branchExists codebase branchName)
--       (pure False)
--       ((branch ==) <$> Codebase.syncBranch codebase branchName branch)
--
-- syncBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
-- syncBranch codebase branch branchName = ifM
--   (Codebase.branchExists codebase branchName)
--   (Codebase.syncBranch codebase branchName branch *> pure True)
--   (pure False)

commandLine
  :: forall i v a
   . Var v
  => IO i
  -> Runtime v
  -> (Output v -> IO ())
  -> Codebase IO v Ann
  -> Free (Command IO i v) a
  -> IO a
commandLine awaitInput rt notifyUser codebase command = Free.fold go command
 where
  go :: forall x . Command IO i v x -> IO x
  go = \case
    -- Wait until we get either user input or a unison file update
    Eval m        -> m
    Input         -> awaitInput
    Notify output -> notifyUser output
--    AddDefsToCodebase handler branch unisonFile -> error "todo"
--      fileToBranch handler codebase branch unisonFile
    Typecheck ambient names sourceName source -> do
      -- todo: if guids are being shown to users, not ideal to generate new guid every time
      namegen <- Parser.uniqueBase58Namegen
      typecheck ambient codebase (namegen, OldNames.fromNames2 names) sourceName source
    Evaluate unisonFile -> evalUnisonFile unisonFile
    LoadLocalRootBranch -> Codebase.getRootBranch codebase
    SyncLocalRootBranch branch -> Codebase.putRootBranch codebase branch
    LoadRemoteRootBranch Github{..} -> error "todo"
    SyncRemoteRootBranch Github{..} _branch -> error "todo"
    RetrieveHashes Github{..} _types _terms -> error "todo"
    LoadTerm r -> CC.getTerm codebase r
    LoadType r -> CC.getTypeDeclaration codebase r
    LoadSearchResults results -> loadSearchResults codebase results

--    Todo b -> doTodo codebase (Branch.head b)
--    Propagate b -> do
--      b0 <- Codebase.propagate codebase (Branch.head b)
--      pure $ Branch.append b0 b
    Execute uf -> void $ evalUnisonFile uf
  evalUnisonFile :: UF.TypecheckedUnisonFile v Ann -> _
  evalUnisonFile (UF.discardTypes -> unisonFile) = do
    let codeLookup = Codebase.toCodeLookup codebase
    selfContained <- Codebase.makeSelfContained' codeLookup unisonFile
    let noCache = const (pure Nothing)
    Runtime.evaluateWatches codeLookup noCache rt selfContained

-- doTodo :: Monad m => Codebase m v a -> Branch0 -> m (TodoOutput v a)
-- doTodo code b = do
--   -- traceM $ "edited terms: " ++ show (Branch.editedTerms b)
--   f <- Codebase.frontier code b
--   let dirty = R.dom f
--       frontier = R.ran f
--       ppe = Branch.prettyPrintEnv b
--   (frontierTerms, frontierTypes) <- loadDefinitions code frontier
--   (dirtyTerms, dirtyTypes) <- loadDefinitions code dirty
--   -- todo: something more intelligent here?
--   scoreFn <- pure $ const 1
--   remainingTransitive <- Codebase.frontierTransitiveDependents code b frontier
--   let
--     addTermNames terms = [(PPE.termName ppe (Referent.Ref r), r, t) | (r,t) <- terms ]
--     addTypeNames types = [(PPE.typeName ppe r, r, d) | (r,d) <- types ]
--     frontierTermsNamed = addTermNames frontierTerms
--     frontierTypesNamed = addTypeNames frontierTypes
--     dirtyTermsNamed = sortOn (\(s,_,_,_) -> s) $
--       [ (scoreFn r, n, r, t) | (n,r,t) <- addTermNames dirtyTerms ]
--     dirtyTypesNamed = sortOn (\(s,_,_,_) -> s) $
--       [ (scoreFn r, n, r, t) | (n,r,t) <- addTypeNames dirtyTypes ]
--   pure $
--     TodoOutput_
--       (Set.size remainingTransitive)
--       (frontierTermsNamed, frontierTypesNamed)
--       (dirtyTermsNamed, dirtyTypesNamed)
--       (Branch.conflicts' b)

loadSearchResults :: (Monad m, Var v) =>
  Codebase m v a -> [SR.SearchResult] -> m [SearchResult' v a]
loadSearchResults code = traverse loadSearchResult
  where
  loadSearchResult = \case
    SR.Tm (SR.TermResult name r aliases) -> do
      typ <- case r of
        Referent.Ref r -> Codebase.getTypeOfTerm code r
        Referent.Con r cid -> Codebase.getTypeOfConstructor code r cid
      pure $ Tm name typ r aliases
    SR.Tp (SR.TypeResult name r aliases) -> do
      dt <- case r of
        Reference.Builtin _ -> pure BuiltinThing
        Reference.DerivedId id ->
          maybe (MissingThing id) RegularThing <$>
            Codebase.getTypeDeclaration code id
      pure $ Tp name dt r aliases

-- loadDefinitions :: Monad m => Codebase m v a -> Set Reference
--                 -> m ( [(Reference, Maybe (Type v a))],
--                        [(Reference, DisplayThing (Decl v a))] )
-- loadDefinitions code refs = do
--   termRefs <- filterM (Codebase.isTerm code) (toList refs)
--   terms <- forM termRefs $ \r -> (r,) <$> Codebase.getTypeOfTerm code r
--   typeRefs <- filterM (Codebase.isType code) (toList refs)
--   types <- forM typeRefs $ \r -> do
--     case r of
--       Reference.Builtin _ -> pure (r, BuiltinThing)
--       Reference.DerivedId id -> do
--         decl <- Codebase.getTypeDeclaration code id
--         case decl of
--           Nothing -> pure (r, MissingThing id)
--           Just d -> pure (r, RegularThing d)
--   pure (terms, types)
--
-- -- | Write all of the builtins into the codebase
-- initializeCodebase :: forall m . Monad m => Codebase m Symbol Ann -> m ()
-- initializeCodebase c = do
--   traverse_ (go Right) B.builtinDataDecls
--   traverse_ (go Left)  B.builtinEffectDecls
--   void $ fileToBranch updateCollisionHandler c mempty IOSource.typecheckedFile
--  where
--   go :: (t -> Decl Symbol Ann) -> (a, (Reference.Reference, t)) -> m ()
--   go f (_, (ref, decl)) = case ref of
--     Reference.DerivedId id -> Codebase.putTypeDeclaration c id (f decl)
--     _                      -> pure ()
--
-- -- todo: probably don't use this anywhere
-- nameDistance :: Name -> Name -> Maybe Int
-- nameDistance (Name.toString -> q) (Name.toString -> n) =
--   if q == n                              then Just 0-- exact match is top choice
--   else if map toLower q == map toLower n then Just 1-- ignore case
--   else if q `isSuffixOf` n               then Just 2-- matching suffix is p.good
--   else if q `isPrefixOf` n               then Just 3-- matching prefix
--   else Nothing
