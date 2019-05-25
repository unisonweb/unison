{-# OPTIONS_GHC -Wwarn #-} -- todo: remove me later

{-# LANGUAGE TypeApplications #-}

module Unison.Codebase2 where

import           Control.Monad                  ( foldM
                                                , forM
                                                )
import           Data.Foldable                  ( toList
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                , catMaybes
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Unison.ABT                    as ABT
import           Unison.Codebase.Branch2         ( Branch )
import qualified Unison.Codebase.Classes       as CC
import qualified Unison.Codebase.CodeLookup    as CL
import qualified Unison.DataDeclaration        as DD
import           Unison.Hash                    ( Hash )
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import           Unison.Typechecker.TypeLookup  (TypeLookup(TypeLookup))
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile             as UF
import qualified Unison.Var                    as Var
import           Unison.Var                     ( Var )

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a

type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a
type Decl v a = TL.Decl v a

data Codebase m v a =
  Codebase { getTerm            :: Reference.Id -> m (Maybe (Term v a))
           , getTypeOfTerm      :: Reference -> m (Maybe (Type v a))
           , getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))

           , putTerm            :: Reference.Id -> Term v a -> Type v a -> m ()
           , putTypeDeclarationImpl :: Reference.Id -> Decl v a -> m ()

           , getRootBranch      :: m (Branch m)
           , putRootBranch      :: Branch m -> m ()
           , rootBranchUpdates  :: m (m (), m (Set Hash))

           , dependentsImpl     :: Reference -> m (Set Reference.Id)
           }

getTypeOfConstructor ::
  (Monad m, Ord v) => Codebase m v a -> Reference -> Int -> m (Maybe (Type v a))
getTypeOfConstructor codebase (Reference.DerivedId r) cid = do
  maybeDecl <- getTypeDeclaration codebase r
  pure $ case maybeDecl of
    Nothing -> Nothing
    Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
getTypeOfConstructor _ r cid =
  error $ "Don't know how to getTypeOfConstructor " ++ show r ++ " " ++ show cid

-- typecheckingEnvironment' :: (Monad m, Ord v) => Codebase m v a -> Term v a -> m (Typechecker.Env v a)
-- typecheckingEnvironment' code term = do
--   tl <- typecheckingEnvironment code term
--   pure $ Typechecker.Env (builtinLoc code) [] tl mempty
--
-- -- Scan the term for all its dependencies and pull out the `ReadRefs` that
-- -- gives info for all its dependencies, using the provided codebase.
-- typecheckingEnvironment
--   :: (Monad m, Ord v) => Codebase m v a -> Term v a -> m (TypeLookup v a)
-- typecheckingEnvironment code t = do
--   let deps = Term.dependencies t
--   termTypes0 <- forM (toList deps) $ \r -> (r, ) <$> getTypeOfTerm code r
--   let termTypes = Map.fromList [ (r, t) | (r, Just t) <- termTypes0 ]
--   let rids      = [ (r0, r) | r0@(Reference.DerivedId r) <- toList deps ]
--   decls0 <- forM rids $ \(r0, r) -> (r0, ) <$> getTypeDeclaration code r
--   let allDecls         = Map.fromList [ (r, d) | (r, Just d) <- decls0 ]
--       (datas, effects) = foldl' go (mempty, mempty) (Map.toList allDecls)
--       go (datas, effects) (r, d) = case d of
--         Left  e -> (datas, Map.insert r e effects)
--         Right d -> (Map.insert r d datas, effects)
--   pure $ TL.TypeLookup termTypes datas effects
--
-- prettyTypeSource :: (Monad m, Var v) => Codebase m v a -> Name -> Reference -> Branch -> m (Maybe (Pretty ColorText))
-- prettyTypeSource = error "todo"
--
-- listReferences
--   :: (Var v, Monad m) => Codebase m v a -> Branch0 -> [Reference] -> m String
-- listReferences code branch refs = do
--   let ppe = Branch.prettyPrintEnv branch
--   terms0 <- forM refs $ \r -> do
--     otyp <- getTypeOfTerm code r
--     pure $ (PPE.termName ppe (Referent.Ref r), otyp)
--   let terms = [ (name, t) | (name, Just t) <- terms0 ]
--   let typeRefs0 = Branch.allNamedTypes branch
--       typeRefs  = filter (`Set.member` typeRefs0) refs
--   _decls <- fmap catMaybes . forM typeRefs $ \r -> case r of
--     Reference.DerivedId id -> do
--       d <- getTypeDeclaration code id
--       pure $ fmap (PPE.typeName ppe r, ) d
--     _ -> pure Nothing
--   let termsPP = TypePrinter.prettySignatures ppe (sortOn fst terms)
--   -- todo: type decls also
--   pure (PP.toPlain 80 termsPP)

-- putTermComponent :: (Monad m, Ord v)
--                  => Codebase m v a
--                  -> Map v (Reference, Term v a, Type v a)
--                  -> m ()
-- putTermComponent code m = forM_ (toList m) $ \(ref, tm, typ) -> case ref of
--   Reference.DerivedId id -> putTerm code id tm typ
--   _ -> pure ()
--
-- putTypeDeclaration
--   :: (Monad m, Ord v) => Codebase m v a -> Reference.Id -> Decl v a -> m ()
-- putTypeDeclaration c rid decl = do
--   putTypeDeclarationImpl c rid decl
--   traverse_ go $ case decl of
--     Left  ed -> DD.effectConstructorTerms rid ed
--     Right dd -> DD.dataConstructorTerms rid dd
--   where go (r, tm, typ) = putTerm c r tm typ
--
-- prettyBinding
--   :: (Var.Var v, Monad m)
--   => Codebase m v a
--   -> HashQualified
--   -> Referent
--   -> Branch0
--   -> m (Maybe (Pretty ColorText))
-- prettyBinding _ _ (Referent.Ref (Reference.Builtin _)) _ = pure Nothing
-- prettyBinding cb name r0@(Referent.Ref r1@(Reference.DerivedId r)) b =
--   go =<< getTerm cb r
--  where
--   go Nothing = pure Nothing
--   go (Just tm) =
--     let
-- -- We force the `(r0,name)` association since if this is a recursive
-- -- fn whose body also mentions `r`, want name to be the same as the binding.
--         ppEnv = PPE.assignTermName r0 name $ Branch.prettyPrintEnv b
--     in  case tm of
--           Term.Ann' _ _ ->
--             pure $ Just (TermPrinter.prettyBinding ppEnv name tm)
--           _ -> do
--             Just typ <- getTypeOfTerm cb r1
--             pure . Just $ TermPrinter.prettyBinding
--               ppEnv
--               name
--               (Term.ann (ABT.annotation tm) tm typ)
-- prettyBinding _ _ r _ = error $ "unpossible " ++ show r
--
-- prettyBindings :: (Var.Var v, Monad m)
--   => Codebase m v a -> [(HashQualified,Referent)] -> Branch0 -> m (Pretty ColorText)
-- prettyBindings cb tms b = do
--   ds <- catMaybes <$> (forM tms $ \(name,r) -> prettyBinding cb name r b)
--   pure $ PP.linesSpaced ds

typeLookupForDependencies
  :: Monad m => Codebase m v a -> Set Reference -> m (TL.TypeLookup v a)
typeLookupForDependencies codebase refs = foldM go mempty refs
 where
--  go ::
  go tl ref@(Reference.DerivedId id) = fmap (tl <>) $ do
    getTypeOfTerm codebase ref >>= \case
      Just typ -> pure $ TypeLookup (Map.singleton ref typ) mempty mempty
      Nothing  -> getTypeDeclaration codebase id >>= \case
        Just (Left ed) ->
          pure $ TypeLookup mempty mempty (Map.singleton ref ed)
        Just (Right dd) ->
          pure $ TypeLookup mempty (Map.singleton ref dd) mempty
        Nothing -> pure mempty
  go tl _builtin = pure tl -- codebase isn't consulted for builtins

-- todo: can this be implemented in terms of TransitiveClosure.transitiveClosure?
-- todo: add some tests on this guy?
transitiveDependencies
  :: (Monad m, Var v)
  => CL.CodeLookup v m a
  -> Set Reference
  -> Reference
  -> m (Set Reference)
transitiveDependencies code seen0 r = if Set.member r seen0
  then pure seen0
  else
    let seen = Set.insert r seen0
    in
      case r of
        Reference.DerivedId id -> do
          t <- CL.getTerm code id
          case t of
            Just t ->
              foldM (transitiveDependencies code) seen (Term.dependencies t)
            Nothing -> do
              t <- CL.getTypeDeclaration code id
              case t of
                Nothing        -> pure seen
                Just (Left ed) -> foldM (transitiveDependencies code)
                                        seen
                                        (DD.dependencies (DD.toDataDecl ed))
                Just (Right dd) -> foldM (transitiveDependencies code)
                                         seen
                                         (DD.dependencies dd)
        _ -> pure seen

toCodeLookup :: Codebase m v a -> CL.CodeLookup v m a
toCodeLookup c = CL.CodeLookup (getTerm c) (getTypeDeclaration c)

 -- Like the other `makeSelfContained`, but takes and returns a `UnisonFile`.
 -- Any watches in the input `UnisonFile` will be watches in the returned
 -- `UnisonFile`.
-- Like the other `makeSelfContained`, but takes and returns a `UnisonFile`.
-- Any watches in the input `UnisonFile` will be watches in the returned
-- `UnisonFile`.
makeSelfContained'
  :: forall m v a . (Monad m, Monoid a, Var v)
  => CL.CodeLookup v m a
  -> UF.UnisonFile v a
  -> m (UF.UnisonFile v a)
makeSelfContained' code uf = do
  let deps0 = Term.dependencies . snd <$> (UF.allWatches uf <> UF.terms uf)
  deps <- foldM (transitiveDependencies code) Set.empty (Set.unions deps0)
  let refVar r = Var.typed (Var.RefNamed r)
--  let termName r = PPE.termName pp (Referent.Ref r)
--      typeName r = PPE.typeName pp r
  decls <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) -> fmap (r, ) <$> CL.getTypeDeclaration code rid
    _                           -> pure Nothing
  termsByRef <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) ->
      fmap (r, refVar r, ) <$> CL.getTerm code rid
    _ -> pure Nothing
  let
    unref :: Term v a -> Term v a
    unref t = ABT.visitPure go t
     where
      go t@(Term.Ref' (r@(Reference.DerivedId _))) =
        Just (Term.var (ABT.annotation t) (refVar r))
      go _ = Nothing
    datas1 = Map.fromList
      [ (r, (v, dd)) | (r, Right dd) <- decls, v <- [refVar r] ]
    effects1 = Map.fromList
      [ (r, (v, ed)) | (r, Left ed) <- decls, v <- [refVar r] ]
    ds0 = Map.fromList [ (r, (v, dd)) | (v, (r, dd)) <-
            Map.toList $ UF.dataDeclarations uf ]
    es0 = Map.fromList [ (r, (v, ed)) | (v, (r, ed)) <-
            Map.toList $ UF.effectDeclarations uf ]
    bindings = [ (v, unref t) | (_, v, t) <- termsByRef ]
    (datas', effects') = (Map.union ds0 datas1, Map.union es0 effects1)
    unrefb bs = [ (v, unref b) | (v, b) <- bs ]
    uf' = UF.UnisonFile
      (Map.fromList [ (v, (r,dd)) | (r, (v,dd)) <- Map.toList datas' ])
      (Map.fromList [ (v, (r,dd)) | (r, (v,dd)) <- Map.toList effects' ])
      (bindings ++ unrefb (UF.terms uf))
      (unrefb <$> UF.watches uf)
  pure $ uf'

-- -- Creates a self-contained `UnisonFile` which bakes in
-- -- all transitive dependencies
-- makeSelfContained
--   :: forall m v a . (Monad m, Monoid a, Var v)
--   => CL.CodeLookup v m a
--   -> PrettyPrintEnv
--   -> Term v a
--   -> m (UF.UnisonFile v a)
-- makeSelfContained code pp term = do
--   deps <- foldM (transitiveDependencies code) Set.empty (Term.dependencies term)
--   let termName r = PPE.termName pp (Referent.Ref r)
--       typeName r = PPE.typeName pp r
--   decls <- fmap catMaybes . forM (toList deps) $ \case
--     r@(Reference.DerivedId rid) -> fmap (r, ) <$> CL.getTypeDeclaration code rid
--     _                           -> pure Nothing
--   termsByRef <- fmap catMaybes . forM (toList deps) $ \case
--     r@(Reference.DerivedId rid) ->
--       fmap (r, HQ.toVar @v (termName r), ) <$> CL.getTerm code rid
--     _ -> pure Nothing
--   let
--     unref :: Term v a -> Term v a
--     unref t = ABT.visitPure go t
--      where
--       go t@(Term.Ref' (r@(Reference.DerivedId _))) =
--         Just (Term.var (ABT.annotation t) (HQ.toVar $ termName r))
--       go _ = Nothing
--     datas = Map.fromList
--       [ (v, (r, dd)) | (r, Right dd) <- decls, v <- [HQ.toVar (typeName r)] ]
--     effects = Map.fromList
--       [ (v, (r, ed)) | (r, Left ed) <- decls, v <- [HQ.toVar (typeName r)] ]
--     bindings = [ (v, unref t) | (_, v, t) <- termsByRef ]
--   pure $ UF.UnisonFile datas effects bindings [] -- no watches in the resulting file
--
-- -- Predicate of Relation a b here is "a depends on b".
-- -- Dependents are in the domain and dependencies in the range.
-- type DependencyGraph = R.Relation Reference Reference
--
-- dependencyGraph
--   :: (Ord v, Monad m) => Codebase m v a -> Branch0 -> m DependencyGraph
-- dependencyGraph c b = do
--   termDeps <-
--     for [ r | Reference.DerivedId r <- Referent.toReference <$> toList terms ]
--       $ \r -> do
--           mayTerm <- getTerm c r
--           case mayTerm of
--             Nothing -> fail $ "Missing term reference " <> show r
--             Just t  -> pure (Reference.DerivedId r, Term.dependencies t)
--   typeDeps <- for [ r | Reference.DerivedId r <- toList types ] $ \r -> do
--     mayType <- getTypeDeclaration c r
--     case mayType of
--       Nothing -> fail $ "Missing type reference " <> show r
--       Just t  -> pure
--         (Reference.DerivedId r, DD.dependencies . either DD.toDataDecl id $ t)
--   pure $ on R.union (R.fromMultimap . Map.fromList) termDeps typeDeps
--  where
--   terms = Branch.allTerms b
--   types = Branch.allTypes b
--
-- isTerm :: Functor m => Codebase m v a -> Reference -> m Bool
-- isTerm code = fmap isJust . getTypeOfTerm code
--
-- isType :: Applicative m => Codebase m v a -> Reference -> m Bool
-- isType c r = case r of
--   Reference.Builtin b -> pure (Name.unsafeFromText b `Set.member` Builtin.builtinTypeNames)
--   Reference.DerivedId r -> isJust <$> getTypeDeclaration c r
--
-- dependents :: Functor m => Codebase m v a -> Reference -> m (Set Reference)
-- dependents c r
--     = Set.union (Builtin.builtinTypeDependents r)
--     . Set.map Reference.DerivedId
--   <$> dependentsImpl c r
--
-- -- Gets the dependents of a whole component (cycle), topologically sorted,
-- -- meaning that if X depends on Y, Y appears before X in this list.
-- -- If X and Y depend on each other, they will appear adjacent in
-- -- arbitrary order.
-- componentDependents
--   :: (Monad m, Ord v) => Codebase m v a -> Reference -> m [Reference]
-- componentDependents c r = do
--   dependents <-
--     fmap (toList . Set.unions)
--     . traverse (dependents c)
--     . toList
--     . Reference.members
--     $ Reference.componentFor r
--   withDependencies <- for dependents
--     $ \r -> fmap (r, ) $ Branch.dependencies refOps r
--   pure . fmap fst . join $ Components.components id withDependencies
--   where refOps = referenceOps c
--
-- -- Turns a cycle of references into a term with free vars that we can edit
-- -- and hash again.
-- unhashComponent
--   :: forall m v a . (Monad m, Var v)
--   => Codebase m v a
--   -> Branch0
--   -> Reference
--   -> m (Maybe (Map v (Reference, Term v a, Type v a)))
-- unhashComponent code b ref = do
--   let component = Reference.members $ Reference.componentFor ref
--       ppe = Branch.prettyPrintEnv b
--   isTerm <- isTerm code ref
--   isType <- isType code ref
--   if isTerm then do
--     let
--       termInfo :: Reference -> m (v, (Reference, Term v a, Type v a))
--       termInfo termRef = do
--         tpm <- getTypeOfTerm code termRef
--         tp  <- maybe (fail $ "Missing type for term " <> show termRef) pure tpm
--         case termRef of
--           Reference.DerivedId id -> do
--             mtm <- getTerm code id
--             tm <- maybe (fail $ "Missing term with id " <> show id) pure mtm
--             pure (HQ.toVar $ PPE.termName ppe (Referent.Ref termRef), (termRef, tm, tp))
--           _ -> fail $ "Cannot unhashComponent for a builtin: " ++ show termRef
--       unhash m =
--         let f (ref,_oldTm,oldTyp) (_ref,newTm) = (ref,newTm,oldTyp)
--             dropType (r,tm,_tp) = (r,tm)
--         in Map.intersectionWith f m (Term.unhashComponent (dropType <$> m))
--     Just . unhash . Map.fromList <$> traverse termInfo (toList component)
--   else if isType then pure Nothing
--   else fail $ "Invalid reference: " <> show ref
--
-- propagate :: (Monad m, Var v, Ord a, Monoid a) => Codebase m v a -> Branch0 -> m Branch0
-- propagate code b = do
--   fs <- R.ran <$> frontier code b
--   propagate' code fs b
--
-- -- For any `Reference` in the frontier which has a type edit, do no propagation.
-- -- (for now, until we have a richer type edit algebra).
-- --
-- -- For any `Reference` in the frontier which has an unconflicted, type-preserving
-- -- term edit, `old -> new`, replace `old` with `new` in dependents of the
-- -- frontier, and call `propagate'` recursively on the new frontier.
-- --
-- -- If the term is `Typing.Same`, the dependents don't need to be typechecked.
-- -- If the term is `Typing.Subtype`, and the dependent only has inferred type,
-- -- it should be re-typechecked, and the new inferred type should be used.
-- --
-- -- This will create a whole bunch of new terms in the codebase and move the
-- -- names onto those new terms. Uses `Term.updateDependencies` to perform
-- -- the substitutions.
--
-- propagate'
--   :: forall m v a
--    . (Monad m, Var v, Ord a, Monoid a)
--   => Codebase m v a
--   -> Set Reference
--   -> Branch0
--   -> m Branch0
-- propagate' code frontier b = go edits b =<< dirty
--  where
--   dirty =
--     Set.toList . Set.unions <$> traverse (dependents code) (Set.toList frontier)
--   edits =
--     Map.fromList
--       .    R.toList
--       .    R.filterRan (TermEdit.isTypePreserving)
--       $    frontier
--       R.<| Branch.editedTerms b
--   update edits =
--     Term.updateDependencies (Map.mapMaybe TermEdit.toReference edits)
--   go :: Map Reference TermEdit -> Branch0 -> [Reference] -> m Branch0
--   go edits b dirty = case dirty of
--     [] -> pure b
--     -- Skip over things already visited (in edits) and things not in the branch
--     r@(Reference.DerivedId _id) : rs | not (Map.member r edits)
--                                     && Branch.contains b r -> do
--       comp <- unhashComponent code b r
--       case comp of
--         Nothing -> go edits b rs
--         Just terms -> do
--           let
--             updatedTerms       = over _2 (update edits) <$> terms
--             updatedHashedTerms = Term.hashComponents (view _2 <$> updatedTerms)
--             p (_, _, typ) (hash, tm) = (hash, tm, typ)
--             newTerms = Map.intersectionWith p updatedTerms updatedHashedTerms
--             deps = toList
--               $ Set.unions (Term.dependencies . view _2 <$> Map.elems terms)
--             tedits = [ edit | d <- deps, edit <- toList (Map.lookup d edits) ]
--             allSame = all TermEdit.isSame tedits
--           replacements <-
--             if allSame
--             then do
--               putTermComponent code newTerms
--               let newEdits = Map.fromList . Map.elems $ Map.intersectionWith
--                     (,)
--                     (view _1 <$> terms)
--                     (view _1 <$> newTerms)
--               pure $ (`TermEdit.Replace` TermEdit.Same) <$> newEdits
--             else do
--               -- We need to redo typechecking to figure out the typing
--               retypechecked <-
--                 typecheckTerms code
--                                [ (v, tm) | (v, (_, tm, _)) <-
--                                              Map.toList updatedTerms ]
--               let p (ref, tm, _) typ = (ref, tm, typ)
--                   newTerms' = Map.intersectionWith p newTerms retypechecked
--               putTermComponent code newTerms'
--               pure $ let
--                 go (ref, _tm, typ) typ'
--                   | Typechecker.isEqual typ typ' =
--                       TermEdit.Replace ref TermEdit.Same
--                   | Typechecker.isSubtype typ' typ =
--                       TermEdit.Replace ref TermEdit.Subtype
--                   | otherwise =
--                       error $ "replacement yielded a different type: "
--                             ++ show (typ, typ')
--                 varToEdit :: Map v TermEdit
--                 varToEdit = Map.intersectionWith go newTerms retypechecked
--                 in Map.fromList .
--                    Map.elems $
--                    Map.intersectionWith (,) (view _1 <$> terms) varToEdit
--           case tedits of
--             [] -> go edits b rs
--             _  -> do
--               let b' = foldl' step b $ Map.toList replacements
--                   step b (old, replacement) = case replacement of
--                     TermEdit.Replace new typing ->
--                       Branch.replaceTerm old new typing b
--                     _ -> b
--               dirtyComponents <- componentDependents code r
--               -- This order traverses the dependency graph depth-first
--               go (replacements <> edits) b' (dirtyComponents <> rs)
--     (_ : rs) -> go edits b rs
--
-- typecheckTerms :: (Monad m, Var v, Ord a, Monoid a)
--                => Codebase m v a
--                -> [(v, Term v a)]
--                -> m (Map v (Type v a))
-- typecheckTerms code bindings = do
--   let tm = Term.letRec' True bindings $ DD.unitTerm mempty
--   env <- typecheckingEnvironment' code tm
--   (o, notes) <- Result.runResultT $ Typechecker.synthesize env tm
--   -- todo: assert that the output map has a type for all variables in the input
--   case o of
--     Nothing -> fail $ "A typechecking error occurred - this indicates a bug in Unison"
--     Just _ -> pure $
--       Map.fromList [ (v, typ) | Context.TopLevelComponent c <- toList (Typechecker.infos notes)
--                               , (v, typ, _) <- c ]
--
-- -- The range of the returned relation is the frontier, and the domain is
-- -- the set of dirty references.
-- frontier :: Monad m => Codebase m v a -> Branch0 -> m (R.Relation Reference Reference)
-- frontier code = frontier' (dependents code)
--
-- -- (d, f) when d is "dirty" (needs update),
-- --             f is in the frontier,
-- --         and d depends of f
-- -- a ⋖ b = a depends on b (with no intermediate dependencies)
-- -- dirty(d) ∧ frontier(f) <=> not(edited(d)) ∧ edited(f) ∧ d ⋖ f
-- --
-- -- The range of this relation is the frontier, and the domain is
-- -- the set of dirty references.
-- frontier' :: forall m . Monad m
--          => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
--          -> Branch0
--          -> m (R.Relation Reference Reference)
-- frontier' getDependents b = let
--   edited :: Set Reference
--   edited = R.dom (Branch.editedTerms b) <> R.dom (Branch.editedTypes b)
--   addDependents :: R.Relation Reference Reference -> Reference -> m (R.Relation Reference Reference)
--   addDependents dependents ref =
--     (\ds -> R.insertManyDom ds ref dependents) . Set.filter (Branch.contains b)
--       <$> getDependents ref
--   in do
--     -- (r,r2) ∈ dependsOn if r depends on r2
--     dependsOn <- foldM addDependents R.empty edited
--     -- Dirty is everything that `dependsOn` Frontier, minus already edited defns
--     pure $ R.filterDom (not . flip Set.member edited) dependsOn
--
-- frontierTransitiveDependents ::
--   Monad m => Codebase m v a -> Branch0 -> Set Reference -> m (Set Reference)
-- frontierTransitiveDependents c b rs = do
--   let branchDependents r = Set.filter (Branch.contains b) <$> dependents c r
--   tdeps <- transitiveClosure branchDependents rs
--   -- we don't want the frontier in the result
--   pure $ tdeps `Set.difference` rs
--
-- referenceOps
--   :: (Ord v, Applicative m) => Codebase m v a -> Branch.ReferenceOps m
-- referenceOps c = Branch.ReferenceOps (isTerm c) (isType c) dependencies dependents'
--  where
--   dependencies r = case r of
--     Reference.DerivedId r ->
--       fromMaybe Set.empty . fmap Term.dependencies <$> getTerm c r
--     _ -> pure $ R.lookupDom r Builtin.builtinDependencies
--   dependents' = dependents c

instance Functor m => CC.GetDecls (Codebase m v a) m v a where
  getTerm = Unison.Codebase2.getTerm
  getTypeOfTerm = Unison.Codebase2.getTypeOfTerm
  getTypeDeclaration = Unison.Codebase2.getTypeDeclaration

  -- these could be more efficient in FileCodebase
  hasTerm d id = isJust <$> Unison.Codebase2.getTerm d id
  hasType d id = isJust <$> Unison.Codebase2.getTypeDeclaration d id

instance CC.PutDecls (Codebase m v a) m v a where
  putTerm = Unison.Codebase2.putTerm
  putTypeDeclarationImpl = Unison.Codebase2.putTypeDeclarationImpl

instance CC.GetBranch (Codebase m v a) m where
  getRootBranch = Unison.Codebase2.getRootBranch

instance CC.PutBranch (Codebase m v a) m where
  putRootBranch = Unison.Codebase2.putRootBranch

instance CC.GetDependents (Codebase m v a) m where
  dependentsImpl = Unison.Codebase2.dependentsImpl
