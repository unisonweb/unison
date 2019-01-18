{-# LANGUAGE DeriveAnyClass,StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Unison.Codebase.Editor where

-- import Debug.Trace
import           Control.Monad                  ( forM_, foldM)
import           Control.Monad.Extra            ( ifM )
import Data.Foldable (toList)
import           Data.Bifunctor                 ( bimap, second )
import           Data.List.Extra                ( nubOrd )
import qualified Data.Map                      as Map
import Data.Map (Map)
import           Data.Sequence                  ( Seq )
import           Data.Set                       ( Set )
import qualified Data.Set as Set
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Unison.Builtin                as B
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase.Branch         ( Branch
                                                , Branch0
                                                )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.DataDeclaration        as DD
import           Unison.FileParsers             ( parseAndSynthesizeFile )
import           Unison.Names                   ( Name
                                                , Names
                                                , NameTarget
                                                )
import qualified Unison.Names as Names
import           Unison.Parser                  ( Ann )
import qualified Unison.Parser                 as Parser
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference, pattern DerivedId )
import qualified Unison.Reference              as Reference
import           Unison.Result                  ( Note
                                                , Result
                                                )
import qualified Unison.Result                 as Result
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.Util.Relation          (Relation)
import qualified Unison.Util.Relation as R
import qualified Unison.Codebase.Runtime       as Runtime
import           Unison.Codebase.Runtime       (Runtime)
import qualified Unison.Codebase.TermEdit      as TermEdit
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Typechecker            as Typechecker
import qualified Unison.Typechecker.Context    as Context
import           Unison.Typechecker.TypeLookup  ( Decl )
import qualified Unison.UnisonFile             as UF
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Var                     ( Var )
import qualified Unison.Var as Var

data Event
  = UnisonFileChanged SourceName Text
  | UnisonBranchChanged (Set Name)

type BranchName = Name
type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type TypecheckingResult v =
  Result (Seq (Note v Ann))
         (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile' v Ann))
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data SlurpComponent v =
  SlurpComponent { implicatedTypes :: Set v, implicatedTerms :: Set v }
  deriving (Show)

instance Ord v => Semigroup (SlurpComponent v) where
  (<>) = mappend
instance Ord v => Monoid (SlurpComponent v) where
  mempty = SlurpComponent mempty mempty
  c1 `mappend` c2 = SlurpComponent (implicatedTypes c1 <> implicatedTypes c2)
                                   (implicatedTerms c1 <> implicatedTerms c2)

data SlurpResult v = SlurpResult {
  -- The file that we tried to add from
    originalFile :: UF.TypecheckedUnisonFile v Ann
  -- The branch after adding everything
  , updatedBranch :: Branch
  -- Previously existed only in the file; now added to the codebase.
  , adds :: SlurpComponent v
  -- Exists in the branch and the file, with the same name and contents.
  , duplicates :: SlurpComponent v
  -- Not added to codebase due to the name already existing
  -- in the branch with a different definition.
  , collisions :: SlurpComponent v
  -- Not added to codebase due to the name existing
  -- in the branch with a conflict (two or more definitions).
  , conflicts :: SlurpComponent v
  -- Names that already exist in the branch, but whose definitions
  -- in `originalFile` are treated as updates.
  , updates :: SlurpComponent v
  -- Names of terms in `originalFile` that couldn't be updated because
  -- they refer to existing constructors. (User should instead do a find/replace,
  -- a constructor rename, or refactor the type that the name comes from).
  , termExistingConstructorCollisions :: Map v Referent
  , constructorExistingTermCollisions :: Map v [Referent]
  -- Already defined in the branch, but with a different name.
  , needsAlias :: Branch.RefCollisions
  , termsWithBlockedDependencies :: Map v (Set Reference)
  , typesWithBlockedDependencies :: Map v (Set Reference)
  } deriving (Show)

data Input
  -- high-level manipulation of names
  = AliasUnconflictedI (Set NameTarget) Name Name
  | RenameUnconflictedI (Set NameTarget) Name Name
  | UnnameAllI NameTarget Name
  -- low-level manipulation of names
  | AddTermNameI Referent Name
  | AddTypeNameI Reference Name
  | RemoveTermNameI Referent Name
  | RemoveTypeNameI Reference Name
  -- resolving naming conflicts
  | ChooseTermForNameI Referent Name
  | ChooseTypeForNameI Reference Name
  -- create and remove update directives
  | ListAllUpdatesI
  -- clear updates for a term or type
  | RemoveAllTermUpdatesI Referent
  | RemoveAllTypeUpdatesI Reference
  -- resolve update conflicts
  | ChooseUpdateForTermI Referent Referent
  | ChooseUpdateForTypeI Reference Reference
  -- other
  | SlurpFileI AllowUpdates
  | ListBranchesI
  | SearchByNameI [String]
  | SwitchBranchI BranchName
  | ForkBranchI BranchName
  | MergeBranchI BranchName
  | ShowDefinitionI [String]
  | TodoI
  | QuitI
  deriving (Show)

-- Whether or not updates are allowed during file slurping
type AllowUpdates = Bool

data DisplayThing a = BuiltinThing | MissingThing Reference.Id | RegularThing a
  deriving (Eq, Ord, Show)

data Output v
  = Success Input
  | NoUnisonFile
  | UnknownBranch BranchName
  | RenameOutput Name Name NameChangeResult
  | AliasOutput Name Name NameChangeResult
  -- todo: probably remove these eventually
  | UnknownName BranchName NameTarget Name
  | NameAlreadyExists BranchName NameTarget Name
  -- `name` refers to more than one `nameTarget`
  | ConflictedName BranchName NameTarget Name
  | BranchAlreadyExists BranchName
  | ListOfBranches BranchName [BranchName]
  | ListOfDefinitions Branch
      [(Name, Referent, Maybe (Type v Ann))]
      [(Name, Reference, DisplayThing (Decl v Ann))]
  | SlurpOutput (SlurpResult v)
  -- Original source, followed by the errors:
  | ParseErrors Text [Parser.Err v]
  | TypeErrors Text PPE.PrettyPrintEnv [Context.ErrorNote v Ann]
  | DisplayConflicts Branch0
  | Evaluated Names ([(Text, Term v ())], Term v ())
  | Typechecked SourceName PPE.PrettyPrintEnv (UF.TypecheckedUnisonFile' v Ann)
  | FileChangeEvent SourceName Text
  | DisplayDefinitions PPE.PrettyPrintEnv
                       [(Reference, DisplayThing (Term v Ann))]
                       [(Reference, DisplayThing (Decl v Ann))]
  | TodoOutput Branch (TodoOutput v)
  deriving (Show)

type Score = Int

data TodoOutput v
  = TodoOutput_ {
      todoScore :: Int,
      todoFrontier ::
        ( [(Name, Reference, Maybe (Type v Ann))]
        , [(Name, Reference, DisplayThing (Decl v Ann))]),
      todoFrontierDependents ::
        ( [(Score, Name, Reference, Maybe (Type v Ann))]
        , [(Score, Name, Reference, DisplayThing (Decl v Ann))]),
      todoConflicts :: Branch0
    } deriving (Show)

data NameChangeResult = NameChangeResult
  { oldNameConflicted :: Set NameTarget
  , newNameAlreadyExists :: Set NameTarget
  , changedSuccessfully :: Set NameTarget
  } deriving (Eq, Ord, Show)

instance Semigroup NameChangeResult where (<>) = mappend
instance Monoid NameChangeResult where
  mempty = NameChangeResult mempty mempty mempty
  NameChangeResult a1 a2 a3 `mappend` NameChangeResult b1 b2 b3 =
    NameChangeResult (a1 <> b1) (a2 <> b2) (a3 <> b3)

-- Function for handling name collisions during a file slurp into the codebase.
-- Returns `True` if the given symbol may be treated as an update.
type OkToUpdate = Bool
type CollisionHandler = Name -> OkToUpdate

-- All collisions get treated as updates and are added to successful.
updateCollisionHandler :: CollisionHandler
updateCollisionHandler = const True

-- All collisions get left alone and are not added to successes.
addCollisionHandler :: CollisionHandler
addCollisionHandler = const False

data Command i v a where
  Input :: Command i v i

  -- Presents some output to the user
  Notify :: Output v -> Command i v ()

  -- This doesn't actually write the branch to the codebase,
  -- only the definitions from the file.
  -- It reads from the codebase and does some branch munging.
  -- Call `MergeBranch` after to actually save your work.
  SlurpFile
    :: CollisionHandler
    -> Branch
    -> UF.TypecheckedUnisonFile v Ann
    -> Command i v (SlurpResult v)

  Typecheck :: Branch
            -> SourceName
            -> Source
            -> Command i v (TypecheckingResult v)

  -- Evaluate a UnisonFile and return the result and the result of
  -- any watched expressions (which are just labeled with `Text`)
  Evaluate :: Branch
           -> UF.UnisonFile v Ann
           -> Command i v ([(Text, Term v ())], Term v ())

  -- Load definitions from codebase:
  -- option 1:
      -- LoadTerm :: Reference -> Command i v (Maybe (Term v Ann))
      -- LoadTypeOfTerm :: Reference -> Command i v (Maybe (Type v Ann))
      -- LoadDataDeclaration :: Reference -> Command i v (Maybe (DataDeclaration' v Ann))
      -- LoadEffectDeclaration :: Reference -> Command i v (Maybe (EffectDeclaration' v Ann))
  -- option 2:
      -- LoadTerm :: Reference -> Command i v (Maybe (Term v Ann))
      -- LoadTypeOfTerm :: Reference -> Command i v (Maybe (Type v Ann))
      -- LoadTypeDecl :: Reference -> Command i v (Maybe (TypeLookup.Decl v Ann))
  -- option 3:
      -- TypeLookup :: [Reference] -> Command i v (TypeLookup.TypeLookup)

  ListBranches :: Command i v [BranchName]

  -- Loads a branch by name from the codebase, returning `Nothing` if not found.
  LoadBranch :: BranchName -> Command i v (Maybe Branch)

  -- Switches the app state to the given branch.
  SwitchBranch :: Branch -> BranchName -> Command i v ()

  -- Returns `False` if a branch by that name already exists.
  NewBranch :: BranchName -> Command i v Bool

  -- Create a new branch which is a copy of the given branch, and assign the
  -- forked branch the given name. Returns `False` if the forked branch name
  -- already exists.
  ForkBranch :: Branch -> BranchName -> Command i v Bool

  -- Merges the branch with the existing branch with the given name. Returns
  -- `False` if no branch with that name exists, `True` otherwise.
  MergeBranch :: BranchName -> Branch -> Command i v Bool

  -- Return the subset of the branch tip which is in a conflicted state.
  -- A conflict is:
  -- * A name with more than one referent.
  -- *
  GetConflicts :: Branch -> Command i v Branch0

  -- List work remaining in the current branch to complete a refactoring.
  RemainingWork :: Branch -> Command i v (Set Branch.RemainingWork)

  -- Return a list of terms whose names match the given queries.
  SearchTerms :: Branch
              -> [String]
              -> Command i v [(Name, Referent, Maybe (Type v Ann))]

  -- Return a list of types whose names match the given queries.
  SearchTypes :: Branch
              -> [String]
              -> Command i v [(Name, Reference)] -- todo: can add Kind later

  LoadTerm :: Reference.Id -> Command i v (Maybe (Term v Ann))

  LoadType :: Reference.Id -> Command i v (Maybe (Decl v Ann))

  Todo :: Branch -> Command i v (TodoOutput v)

data Outcome
  -- New definition that was added to the branch
  = Added
  -- A name collision that was treated as a replacement
  | Updated
  -- A name collision that couldn't be treated as a replacement (use `update`)
  | CouldntUpdate
  -- A name that couldn't be updated because it's currently conflicted
  | CouldntUpdateConflicted
  -- Skipped because it already exist in the branch (with same name)
  | AlreadyExists
  -- Skipped because it already exist in the branch (with different name(s))
  | RequiresAlias [Name]
  -- Skipped terms because they share a name with existing constructor
  | TermExistingConstructorCollision
  -- Skipped types because one or more constructors collides with an existing term
  -- (This could include the constructors of another type)
  | ConstructorExistingTermCollision [Referent]
  -- Skipped because at least 1 of its dependencies couldn't be added
  -- or doesn't already exist in branch
  | CouldntAddDependencies (Set Reference)

blocksDependent :: Outcome -> Bool
blocksDependent = \case
  Added -> False
  Updated -> False
  AlreadyExists -> False
  RequiresAlias _ -> False
  _ -> True

outcomes :: Var v
         => CollisionHandler
         -> Branch0
         -> UF.TypecheckedUnisonFile v Ann
         -> [(Either Reference Reference, Outcome)]
outcomes okToUpdate b file = let
  namesForTermOrType r b =
    Branch.namesForType r b <> Branch.namesForTerm (Referent.Ref r) b
  outcome0 n r0 ctorNames = let r = either id id r0 in
    if Branch.contains b r then -- Already exists
      case toList $ namesForTermOrType r b of
        -- note: this will only return names from appropriate namespace
        ns | n `elem` ns -> (r0, AlreadyExists)
           | otherwise   -> (r0, RequiresAlias ns)
    else case r0 of -- this doesn't exist in the branch
      -- It's a term
      Right _ -> case toList $ Branch.termsNamed n b of
        [] -> (r0, Added)
        referents ->
          if not (okToUpdate n) then (r0, CouldntUpdate)
          else if length referents > 1 then (r0, CouldntUpdateConflicted)
          else if any Referent.isConstructor referents then (r0, TermExistingConstructorCollision)
          else (r0, Updated)
      -- It's a type
      Left _ -> let
        ctorNameCollisions :: Set Referent
        ctorNameCollisions = Set.unions $
          map (`Branch.termsNamed` b) ctorNames
        in case toList $ Branch.typesNamed n b of
          [] -> -- no type name collisions
            if null ctorNameCollisions
            then (r0, Added) -- and no term collisions
            else (r0, ConstructorExistingTermCollision $ toList ctorNameCollisions)
          _refs | not (okToUpdate n) -> (r0, CouldntUpdate)
          [oldref] -> let
            conflicted = toList ctorNameCollisions >>= \r2 -> case r2 of
              Referent.Ref _ -> [r2]
              -- note - it doesn't count as a collision if the name
              -- collision is on a ctor of the type we're replacing
              -- of the type we will be replacing
              Referent.Req r _ -> if r == oldref then [] else [r2]
              Referent.Con r _ -> if r == oldref then [] else [r2]
            in if null conflicted then (r0, Updated)
               else (r0, ConstructorExistingTermCollision conflicted)
          _otherwise -> (r0, CouldntUpdateConflicted) -- come back to this

  outcomes0terms = map termOutcome (Map.toList $ UF.hashTerms file)
  termOutcome (v, (r, _, _)) = outcome0 (Var.name v) (Right r) []
  outcomes0types
     = map typeOutcome (Map.toList . fmap (second Right) $ UF.dataDeclarations' file)
    ++ map typeOutcome (Map.toList . fmap (second Left) $ UF.effectDeclarations' file)
  typeOutcome (v, (r, dd)) = outcome0 (Var.name v) (Left r) $ ctorNames v r dd
  ctorNames v r (Left e) = Map.keys $ Names.termNames (DD.effectDeclToNames v r e)
  ctorNames v r (Right dd) = Map.keys $ Names.termNames (DD.dataDeclToNames v r dd)
  outcomes0 = outcomes0terms ++ outcomes0types
  in removeTransitive (UF.dependencies' file) outcomes0

-- Converts outcomes to CouldntAddDependencies if it is a successful outcome
-- which depends (directly or indirectly) on a Reference with an unsuccessful
-- outcome.
removeTransitive
  :: Relation Reference Reference
  -> [(Either Reference Reference, Outcome)]
  -> [(Either Reference Reference, Outcome)]
removeTransitive dependencies outcomes0 = let
  ref r = either id id r
  -- `Set Reference` that have been removed
  removed0 = Set.fromList [ ref r | (r, o) <- outcomes0, blocksDependent o ]
  trim :: Set Reference
       -> [(Either Reference Reference, Outcome)]
       -> [(Either Reference Reference, Outcome)]
  trim removedAlready outcomes = let
    outcomes' = map stepOutcome outcomes
    stepOutcome (r, o) =
      let
        -- dependencies of r which have already been marked for removal
        removedDeps =
          Set.intersection removedAlready (R.lookupDom (ref r) dependencies)
      in
        -- if r's outcome is already a sort of failure, then keep that outcome
        if blocksDependent o then (r, o)
        -- or if none of r's dependencies are removed, then don't change r's outcome
        else if Set.null removedDeps then (r, o)
        -- else some of r's deps block r
        else (r, CouldntAddDependencies removedDeps)
    removed = Set.fromList [ ref r | (r, o) <- outcomes', blocksDependent o ]
    in if Set.size removed == Set.size removedAlready then outcomes
       else trim removed outcomes'
  in trim removed0 outcomes0

fileToBranch
  :: forall m v
  .  (Var v, Monad m)
  => CollisionHandler
  -> Codebase m v Ann
  -> Branch
  -> UF.TypecheckedUnisonFile v Ann
  -> m (SlurpResult v)
fileToBranch handleCollisions codebase branch uf = do
  -- Write out all the successful outcomes to the codebase
  forM_ outcomes0 $ \(r, o) ->
    case o of
      Added -> writeDefinition r
      Updated -> writeDefinition r
      _ -> pure ()
  -- Accumulate the final slurp result and the updated Branch,
  -- by folding over the outcomes.
  (result, b0) <- foldM addOutcome
    (SlurpResult uf branch mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty, Branch.head branch) outcomes'
  pure $ result { updatedBranch = Branch.cons b0 branch }
  where
    b0 = Branch.head branch
    outcomes0 = outcomes handleCollisions b0 uf
    outcomes' = map addV outcomes0 where
      addV (r, o) = case r of
        Left tr -> case Map.lookup tr declsByRef of
          Nothing -> error "Panic. Unknown type in fileToBranch."
          Just (v, dd) -> (v, bimap (,dd) id r, o)
        Right er -> case Map.lookup er termsByRef of
          Nothing -> error "Panic. Unknown term in fileToBranch"
          Just (v, _, _) -> (v, Right er, o)
    -- converts a term or type reference, r, to a SlurpComponent
    sc r v = case r of
      Left _ -> SlurpComponent (Set.singleton v) mempty
      Right _ -> SlurpComponent mempty (Set.singleton v)
    -- The folding function: state is the SlurpResult and the accumulated Branch0,
    -- And each outcome is used to update this state.
    addOutcome (result, b) (v, r, o) = case o of
      Added -> pure $ case r of
        Left (r, dd) ->
          ( result { adds = adds result <> SlurpComponent (Set.singleton v) mempty }
          , Branch.fromDeclaration v r dd <> b )
        Right r ->
          ( result { adds = adds result <> SlurpComponent mempty (Set.singleton v) }
          , Branch.addTermName (Referent.Ref r) (Var.name v) b )
      Updated -> do
        let result' = result { updates = updates result <> sc r v }
        case r of
          Left (r', dd) -> case toList (Branch.typesNamed (Var.name v) b0) of
            [r0] -> pure (result', Branch.fromDeclaration v r' dd <> Branch.replaceType r0 r' b)
            _ -> error "Panic. Tried to replace a type that's conflicted."
          Right r' -> case toList (Branch.termsNamed (Var.name v) b0) of
            [Referent.Ref r0] -> do
              Just type1 <- Codebase.getTypeOfTerm codebase r0
              let Just (_, _, type2) = Map.lookup r' termsByRef
              let typing =
                    if Typechecker.isEqual type1 type2 then TermEdit.Same
                    else if Typechecker.isSubtype type2 type1 then TermEdit.Subtype
                    else TermEdit.Different
              pure (result', Branch.addTermName (Referent.Ref r') (Var.name v) $
                             Branch.replaceTerm r0 r' typing b)
            _ -> error $ "Panic. Tried to replace a term that's conflicted." ++ show v
      AlreadyExists -> pure (result { duplicates = duplicates result <> sc r v }, b)
      CouldntUpdate -> pure (result { collisions = collisions result <> sc r v }, b)
      CouldntUpdateConflicted ->
        pure (result { conflicts = conflicts result <> sc r v }, b)
      RequiresAlias ns -> let
        rcs = case r of
          Left _ -> Branch.RefCollisions mempty (R.fromList $ (Var.name v,) <$> ns)
          Right _ -> Branch.RefCollisions (R.fromList $ (Var.name v,) <$> ns) mempty
        in pure (result { needsAlias = needsAlias result <> rcs }, b)
      TermExistingConstructorCollision ->
        pure (result {
          termExistingConstructorCollisions =
            termExistingConstructorCollisions result <>
            pick (toList $ Branch.constructorsNamed (Var.name v) b0) }, b)
        where
          pick [] = error "Panic. Incorrectly determined a conflict."
          pick (h:_) = Map.fromList [(v, h)]
      ConstructorExistingTermCollision rs ->
        pure (result { constructorExistingTermCollisions = constructorExistingTermCollisions result <> Map.fromList [(v, rs)] }, b)
      CouldntAddDependencies rs -> case r of
        Left _ -> pure (result { typesWithBlockedDependencies =
          typesWithBlockedDependencies result <> Map.fromList [(v, rs)] }, b)
        Right _ -> pure (
          result { termsWithBlockedDependencies = termsWithBlockedDependencies result <>
                   Map.fromList [(v, rs)] }, b)
    declsByRef :: Map Reference (v, Decl v Ann)
    declsByRef = Map.fromList $
      [ (r, (v, d)) | (v, (r,d)) <- mconcat [
          Map.toList . fmap (second Right) $ UF.dataDeclarations' uf
        , Map.toList . fmap (second Left) $ UF.effectDeclarations' uf ]]
    termsByRef = Map.fromList
      [ (r, (v, tm, typ))
      | (v, (r, tm, typ)) <- Map.toList $ UF.hashTerms uf ]
    prepTerm = Term.amap (const Parser.External)
    prepDecl :: Decl v Ann -> Decl v Ann
    prepDecl = bimap ex ex
    ex :: Functor f => f a -> f Ann
    ex = fmap (const Parser.External)
    writeDefinition = \case
      Left typeRef@(DerivedId d) -> let
        Just (_, dd) = Map.lookup typeRef declsByRef
        in Codebase.putTypeDeclaration codebase d (prepDecl dd)
      Right termRef@(DerivedId d) -> let
        Just (_, tm, typ) = Map.lookup termRef termsByRef
        in Codebase.putTerm codebase d (prepTerm tm) (ex typ)
      r -> error $ "Panic. Hashing produced a builtin Reference: " ++ show r

typecheck
  :: (Monad m, Var v)
  => Codebase m v Ann
  -> Names
  -> SourceName
  -> Text
  -> m (TypecheckingResult v)
typecheck codebase names sourceName src =
  Result.getResult $ parseAndSynthesizeFile
    (((<> B.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
    names
    (unpack sourceName)
    src

builtinBranch :: Branch
builtinBranch = Branch.append (Branch.fromNames B.names) mempty

newBranch :: Monad m => Codebase m v a -> BranchName -> m Bool
newBranch codebase branchName = forkBranch codebase builtinBranch branchName

forkBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
forkBranch codebase branch branchName = do
  ifM (Codebase.branchExists codebase branchName)
      (pure False)
      ((branch ==) <$> Codebase.mergeBranch codebase branchName branch)

mergeBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
mergeBranch codebase branch branchName = ifM
  (Codebase.branchExists codebase branchName)
  (Codebase.mergeBranch codebase branchName branch *> pure True)
  (pure False)

-- Returns terms and types, respectively. For terms that are
-- constructors, turns them into their data types.
collateReferences
  :: [Referent] -- terms requested, including ctors
  -> [Reference] -- types requested
  -> ([Reference], [Reference])
collateReferences terms types =
  let terms' = [ r | Referent.Ref r <- terms ]
      types' = terms >>= \case
        Referent.Con r _ -> [r]
        Referent.Req r _ -> [r]
        _                -> []
  in  (terms', nubOrd $ types' <> types)

commandLine
  :: forall i v a
   . Var v
  => IO i
  -> Runtime v
  -> (Branch -> BranchName -> IO ())
  -> (Output v -> IO ())
  -> Codebase IO v Ann
  -> Free (Command i v) a
  -> IO a
commandLine awaitInput rt branchChange notifyUser codebase command = do
  Free.fold go command
 where
  go :: forall x . Command i v x -> IO x
  go = \case
    -- Wait until we get either user input or a unison file update
    Input         -> awaitInput
    Notify output -> notifyUser output
    SlurpFile handler branch unisonFile ->
      fileToBranch handler codebase branch unisonFile
    Typecheck branch sourceName source ->
      typecheck codebase (Branch.toNames branch) sourceName source
    Evaluate branch unisonFile -> do
      selfContained <- Codebase.makeSelfContained codebase
                                                  (Branch.head branch)
                                                  unisonFile
      Runtime.evaluate rt selfContained codebase
    ListBranches                      -> Codebase.branches codebase
    LoadBranch branchName             -> Codebase.getBranch codebase branchName
    NewBranch  branchName             -> newBranch codebase branchName
    ForkBranch  branch     branchName -> forkBranch codebase branch branchName
    MergeBranch branchName branch     -> mergeBranch codebase branch branchName
    GetConflicts branch -> pure $ Branch.conflicts' (Branch.head branch)
    SwitchBranch branch branchName    -> branchChange branch branchName
    SearchTerms branch queries ->
      Codebase.fuzzyFindTermTypes codebase branch queries
    SearchTypes branch queries ->
      pure $ Codebase.fuzzyFindTypes' branch queries
    LoadTerm r -> Codebase.getTerm codebase r
    LoadType r -> Codebase.getTypeDeclaration codebase r
    RemainingWork b ->
      Branch.remaining (Codebase.referenceOps codebase) (Branch.head b)
    Todo b -> doTodo codebase (Branch.head b)

doTodo :: Monad m => Codebase m v a -> Branch0 -> m (TodoOutput v)
doTodo code b = do
  f <- frontier (Codebase.dependents code) b
  let dirty = R.dom f
      frontier = R.ran f
  error "todo"

-- (f, d) when d is "dirty" (needs update),
--             f is in the frontier,
--         and d depends of f
-- a ⋖ b = a depends on b (with no intermediate dependencies)
-- dirty(D) ∧ frontier(F) <=> not(edited(D)) ∧ edited(F) ∧ D ⋖ F
--
-- The range of this relation is the frontier, and the domain is
-- the set of dirty references.
frontier :: forall m . Monad m
         => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
         -> Branch0
         -> m (Relation Reference Reference)
frontier getDependents b = let
  edited :: Set Reference
  edited = R.dom (Branch.editedTerms b) <> R.dom (Branch.editedTypes b)
  addDependents :: Relation Reference Reference -> Reference -> m (Relation Reference Reference)
  addDependents dependents ref =
    (\ds -> R.insertManyDom ds ref dependents) <$> getDependents ref
  in do
    dependsOn <- foldM addDependents R.empty edited
    pure $ R.filterRan (not . flip Set.member edited) dependsOn
