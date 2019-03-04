{-# LANGUAGE DeriveAnyClass,StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor where

-- import Debug.Trace

import           Data.Char                      ( toLower )
import Data.List (sortOn, isSuffixOf, isPrefixOf)
import           Control.Monad                  ( forM_, forM, foldM, filterM)
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
import qualified Text.Regex.TDFA               as RE
import qualified Unison.Builtin                as B
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase.Branch         ( Branch
                                                , Branch0
                                                )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.DataDeclaration        as DD
import           Unison.FileParsers             ( parseAndSynthesizeFile )
import           Unison.HashQualified           ( HashQualified )
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.Names                   ( Names
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
import qualified Unison.Util.Find              as Find
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Var                     ( Var )

data Event
  = UnisonFileChanged SourceName Text
  | UnisonBranchChanged (Set BranchName)

type BranchName = Text
type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type TypecheckingResult v =
  Result (Seq (Note v Ann))
         (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile v Ann))
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
  -- execute an IO object with arguments
  -- | ExecuteI Name [String]
  -- other
  | SlurpFileI AllowUpdates
  | ListBranchesI
  | SearchByNameI [String]
  | SwitchBranchI BranchName
  | ForkBranchI BranchName
  | MergeBranchI BranchName
  | ShowDefinitionI OutputLocation [String]
  | TodoI
  | PropagateI
  | QuitI
  deriving (Show)

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation
  | FileLocation FilePath deriving Show
  -- ClipboardLocation


-- Whether or not updates are allowed during file slurping
type AllowUpdates = Bool

data DisplayThing a = BuiltinThing | MissingThing Reference.Id | RegularThing a
  deriving (Eq, Ord, Show)

data SearchResult' v a
  = Tm'' (TermResult' v a)
  | Tp'' (TypeResult' v a)
  deriving (Eq, Show)
data TermResult' v a =
  TermResult'' HashQualified (Maybe (Type v a)) Referent (Set HashQualified)
  deriving (Eq, Show)
data TypeResult' v a =
  TypeResult'' HashQualified (DisplayThing (Decl v a)) Reference (Set HashQualified)
  deriving (Eq, Show)
pattern Tm h t r as = Tm'' (TermResult'' h t r as)
pattern Tp h t r as = Tp'' (TypeResult'' h t r as)

searchResult' :: (TermResult' v a -> b) -> (TypeResult' v a -> b) -> SearchResult' v a -> b
searchResult' f g = \case
  Tm'' tm -> f tm
  Tp'' tp -> g tp

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
  | ListOfDefinitions Branch [SearchResult' v Ann]
  | SlurpOutput (SlurpResult v)
  -- Original source, followed by the errors:
  | ParseErrors Text [Parser.Err v]
  | TypeErrors Text PPE.PrettyPrintEnv [Context.ErrorNote v Ann]
  | DisplayConflicts Branch0
  | Evaluated SourceFileContents
              PPE.PrettyPrintEnv
              [(v, Term v ())]
              (Map v (Ann, Term v (), Runtime.IsCacheHit))
  | Typechecked SourceName PPE.PrettyPrintEnv (UF.TypecheckedUnisonFile v Ann)
  | FileChangeEvent SourceName Text
  | DisplayDefinitions (Maybe FilePath) PPE.PrettyPrintEnv
                       [(Reference, DisplayThing (Term v Ann))]
                       [(Reference, DisplayThing (Decl v Ann))]
  | TodoOutput Branch (TodoOutput v Ann)
  deriving (Show)

type SourceFileContents = Text
type Score = Int

data TodoOutput v a
  = TodoOutput_ {
      todoScore :: Int,
      todoFrontier ::
        ( [(HashQualified, Reference, Maybe (Type v a))]
        , [(HashQualified, Reference, DisplayThing (Decl v a))]),
      todoFrontierDependents ::
        ( [(Score, HashQualified, Reference, Maybe (Type v a))]
        , [(Score, HashQualified, Reference, DisplayThing (Decl v a))]),
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

  -- Evaluate all watched expressions in a UnisonFile and return
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
  Evaluate :: Branch
           -> UF.UnisonFile v Ann
           -> Command i v ([(v, Term v ())], Map v
                (Ann, Reference, Term v (), Term v (), Runtime.IsCacheHit))

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
  SyncBranch :: BranchName -> Branch -> Command i v Bool

  -- Return the subset of the branch tip which is in a conflicted state.
  -- A conflict is:
  -- * A name with more than one referent.
  -- *
  GetConflicts :: Branch -> Command i v Branch0

  -- Return a list of definitions whose names match the given queries.
  SearchBranch :: Branch -> [HashQualified] -> Command i v [SearchResult' v Ann]

  LoadTerm :: Reference.Id -> Command i v (Maybe (Term v Ann))

  LoadType :: Reference.Id -> Command i v (Maybe (Decl v Ann))

  Todo :: Branch -> Command i v (TodoOutput v Ann)

  Propagate :: Branch -> Command i v Branch

  -- Execute :: Reference.Id -> Command i v (IO ())

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
          else if length referents > 1
          then (r0, CouldntUpdateConflicted)
          else if any Referent.isConstructor referents
          then (r0, TermExistingConstructorCollision)
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
              Referent.Con r _ -> if r == oldref then [] else [r2]
            in if null conflicted then (r0, Updated)
               else (r0, ConstructorExistingTermCollision conflicted)
          _otherwise -> (r0, CouldntUpdateConflicted) -- come back to this

  outcomes0terms = map termOutcome (Map.toList $ UF.hashTerms file)
  termOutcome (v, (r, _, _)) = outcome0 (Name.unsafeFromVar v) (Right r) []
  outcomes0types =
    map typeOutcome (Map.toList . fmap (second Right) $ UF.dataDeclarations' file)
      ++ map typeOutcome
             (Map.toList . fmap (second Left) $ UF.effectDeclarations' file)
  typeOutcome (v, (r, dd)) =
    outcome0 (Name.unsafeFromVar v) (Left r) $ ctorNames v r dd
  ctorNames v r (Left e) =
    Map.keys $ Names.termNames (DD.effectDeclToNames v r e)
  ctorNames v r (Right dd) =
    Map.keys $ Names.termNames (DD.dataDeclToNames v r dd)
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

-- Handles add/update command for a typechecked file
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
  -- todo: be a little smarter about avoiding needless propagation
  b0 <- Codebase.propagate codebase b0
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
          , Branch.addTermName (Referent.Ref r) (Name.unsafeFromVar v) b )
      Updated -> do
        let result' = result { updates = updates result <> sc r v }
            name = Name.unsafeFromVar v
        case r of
          Left (r', dd) -> case toList (Branch.typesNamed name b0) of
            [r0] -> pure (result', Branch.fromDeclaration v r' dd <> Branch.replaceType r0 r' b)
            _ -> error "Panic. Tried to replace a type that's conflicted."
          Right r' -> case toList (Branch.termsNamed name b0) of
            [Referent.Ref r0] -> do
              Just type1 <- Codebase.getTypeOfTerm codebase r0
              let Just (_, _, type2) = Map.lookup r' termsByRef
              let typing =
                    if Typechecker.isEqual type1 type2 then TermEdit.Same
                    else if Typechecker.isSubtype type2 type1 then TermEdit.Subtype
                    else TermEdit.Different
              pure (result', Branch.addTermName (Referent.Ref r') name $
                             Branch.replaceTerm r0 r' typing b)
            _ -> error $ "Panic. Tried to replace a term that's conflicted." ++ show v
      AlreadyExists -> pure (result { duplicates = duplicates result <> sc r v }, b)
      CouldntUpdate -> pure (result { collisions = collisions result <> sc r v }, b)
      CouldntUpdateConflicted ->
        pure (result { conflicts = conflicts result <> sc r v }, b)
      RequiresAlias ns -> let
        name = Name.unsafeFromVar v
        rcs = case r of
          Left _ -> Branch.RefCollisions mempty (R.fromList $ (name,) <$> ns)
          Right _ -> Branch.RefCollisions (R.fromList $ (name,) <$> ns) mempty
        in pure (result { needsAlias = needsAlias result <> rcs }, b)
      TermExistingConstructorCollision ->
        pure (result {
          termExistingConstructorCollisions =
            termExistingConstructorCollisions result <>
            pick (toList $ Branch.constructorsNamed (Name.unsafeFromVar v) b0) }, b)
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
      ((branch ==) <$> Codebase.syncBranch codebase branchName branch)

syncBranch :: Monad m => Codebase m v a -> Branch -> BranchName -> m Bool
syncBranch codebase branch branchName = ifM
  (Codebase.branchExists codebase branchName)
  (Codebase.syncBranch codebase branchName branch *> pure True)
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
      let codeLookup = Codebase.toCodeLookup codebase

      selfContained <- Codebase.makeSelfContained'
        codeLookup
        (Branch.head branch)
        unisonFile
      let noCache = const (pure Nothing)
      Runtime.evaluateWatches codeLookup noCache rt selfContained
    ListBranches                      -> Codebase.branches codebase
    LoadBranch branchName             -> Codebase.getBranch codebase branchName
    NewBranch  branchName             -> newBranch codebase branchName
    ForkBranch  branch     branchName -> forkBranch codebase branch branchName
    SyncBranch branchName branch      -> syncBranch codebase branch branchName
    GetConflicts branch -> pure $ Branch.conflicts' (Branch.head branch)
    SwitchBranch branch branchName    -> branchChange branch branchName
    SearchBranch (Branch.head -> branch) queries -> do
      let termResults =
            Branch.searchTermNamespace branch fuzzyNameDistance queries
          typeResults =
            Branch.searchTypeNamespace branch fuzzyNameDistance queries
      loadSearchResults codebase . fmap snd . toList $ typeResults <> termResults
    LoadTerm r -> Codebase.getTerm codebase r
    LoadType r -> Codebase.getTypeDeclaration codebase r
    Todo b -> doTodo codebase (Branch.head b)
    Propagate b -> do
      b0 <- Codebase.propagate codebase (Branch.head b)
      pure $ Branch.append b0 b

doTodo :: Monad m => Codebase m v a -> Branch0 -> m (TodoOutput v a)
doTodo code b = do
  -- traceM $ "edited terms: " ++ show (Branch.editedTerms b)
  f <- Codebase.frontier code b
  let dirty = R.dom f
      frontier = R.ran f
      ppe = Branch.prettyPrintEnv b
  (frontierTerms, frontierTypes) <- loadDefinitions code frontier
  (dirtyTerms, dirtyTypes) <- loadDefinitions code dirty
  -- todo: something more intelligent here?
  scoreFn <- pure $ const 1
  remainingTransitive <- Codebase.frontierTransitiveDependents code b frontier
  let
    addTermNames terms = [(PPE.termName ppe (Referent.Ref r), r, t) | (r,t) <- terms ]
    addTypeNames types = [(PPE.typeName ppe r, r, d) | (r,d) <- types ]
    frontierTermsNamed = addTermNames frontierTerms
    frontierTypesNamed = addTypeNames frontierTypes
    dirtyTermsNamed = sortOn (\(s,_,_,_) -> s) $
      [ (scoreFn r, n, r, t) | (n,r,t) <- addTermNames dirtyTerms ]
    dirtyTypesNamed = sortOn (\(s,_,_,_) -> s) $
      [ (scoreFn r, n, r, t) | (n,r,t) <- addTypeNames dirtyTypes ]
  pure $
    TodoOutput_
      (Set.size remainingTransitive)
      (frontierTermsNamed, frontierTypesNamed)
      (dirtyTermsNamed, dirtyTypesNamed)
      (Branch.conflicts' b)

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

loadDefinitions :: Monad m => Codebase m v a -> Set Reference
                -> m ( [(Reference, Maybe (Type v a))],
                       [(Reference, DisplayThing (Decl v a))] )
loadDefinitions code refs = do
  termRefs <- filterM (Codebase.isTerm code) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> Codebase.getTypeOfTerm code r
  typeRefs <- filterM (Codebase.isType code) (toList refs)
  types <- forM typeRefs $ \r -> do
    case r of
      Reference.Builtin _ -> pure (r, BuiltinThing)
      Reference.DerivedId id -> do
        decl <- Codebase.getTypeDeclaration code id
        case decl of
          Nothing -> pure (r, MissingThing id)
          Just d -> pure (r, RegularThing d)
  pure (terms, types)


fuzzyNameDistance :: Name -> Name -> Maybe RE.MatchArray
fuzzyNameDistance (Name.toString -> q) (Name.toString -> n) =
  case Find.fuzzyFindMatchArray q [n] id of
    [] -> Nothing
    (m, _) : _ -> Just m

-- todo: probably don't use this anywhere
nameDistance :: Name -> Name -> Maybe Int
nameDistance (Name.toString -> q) (Name.toString -> n) =
  if q == n                              then Just 0-- exact match is top choice
  else if map toLower q == map toLower n then Just 1-- ignore case
  else if q `isSuffixOf` n               then Just 2-- matching suffix is p.good
  else if q `isPrefixOf` n               then Just 3-- matching prefix
  else Nothing
