{-# OPTIONS_GHC -Wwarn #-} -- todo: remove me later

-- {-# LANGUAGE DeriveAnyClass,StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
--
module Unison.Codebase.Editor2 where

import qualified Unison.Codebase.Branch2 as Branch2

-- import Debug.Trace

import           Data.Char                      ( toLower )
import Data.List (sortOn, isSuffixOf, isPrefixOf)
import           Control.Monad                  ( forM_, forM, foldM, filterM, void)
import           Control.Monad.Extra            ( ifM )
import           Data.Foldable                  ( toList
                                                , traverse_
                                                )
import           Data.Bifunctor                 ( bimap, second )
import           Data.List.Extra                ( nubOrd )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Sequence                  ( Seq )
import           Data.Set                       ( Set )
import qualified Data.Set as Set
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Unison.Builtin                as B
import           Unison.Codebase2               ( Codebase )
import qualified Unison.Codebase.Classes       as CC
import qualified Unison.Codebase2              as Codebase
import qualified Unison.Codebase.CodeLookup    as CL
import           Unison.Codebase.Branch2         ( Branch
                                                 , Branch0
                                                 )
import qualified Unison.Codebase.Branch2        as Branch
import qualified Unison.Codebase.OldBranch      as OldBranch
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.DataDeclaration        as DD
import           Unison.FileParsers             ( parseAndSynthesizeFile )
import           Unison.HashQualified           ( HashQualified )
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.Names2                  ( Names )
import qualified Unison.Names2                 as Names
import           Unison.Codebase.Path           ( Path )
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
import qualified Unison.Runtime.IOSource       as IOSource
import           Unison.Symbol                  ( Symbol )
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

data Event
  = UnisonFileChanged SourceName Text
  | IncomingRootBranch (Set Branch.Hash)

type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type TypecheckingResult v =
  Result (Seq (Note v Ann))
         (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile v Ann))
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

type BranchPath = Path
type EditsPath = Path
type TermPath = Path
type TypePath = Path

data NameTarget = TermName | TypeName | EditsName deriving (Eq, Ord, Show)

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

data RepoRef
  = Local
  | Github { username :: Text, repo :: Text, commit :: Text }
  deriving (Eq, Ord, Show)

data RepoLink a = RepoLink RepoRef a
  deriving (Eq, Ord, Show)

data Input
  -- names stuff:
    -- directory ops
    -- `Link` must describe a repo and a source path within that repo.
    -- clone w/o merge, error if would clobber
    = ForkBranchI (RepoLink BranchPath) BranchPath
    -- merge first causal into destination
    | MergeBranchI (RepoLink BranchPath) BranchPath
    -- Question: How should we distinguish "move as rename" vs
    -- "move within" mv foo bar vs mv foo bar/ ?
    -- Answer: use `mv foo bar/foo` if that's what you want.
    | RenameBranchI BranchPath BranchPath
    --
    | DeleteBranchI [BranchPath]
    | SwitchBranchI BranchPath -- cd
    -- definition naming
      -- if HashQualified is unique, create alias
    | AliasAnyI HashQualified Path
    | AliasTermI HashQualified TermPath
    | AliasTypeI HashQualified TypePath
    | RenameAnyI HashQualified Path
    | RenameTermI HashQualified TermPath
    | RenameTypeI HashQualified TypePath
    | RenameEditsI EditsPath EditsPath
    | RemoveTermNameI TermPath
    | RemoveTypeNameI TypePath
    -- deletes all the supplied names.  because names (and even hash-qualified
    -- names) may correspond to multiple definitions, this command will delete
    -- all matching entries.
    | UnnameAllI (Set HashQualified)
    -- resolving naming conflicts within `branchpath`
      -- Add the specified name after deleting all others for a given reference
      -- within a given branch.
      | ResolveTermNameI BranchPath Referent Name
      | ResolveTypeNameI BranchPath Reference Name
  -- edits stuff:
    | TodoI EditsPath BranchPath
    | PropagateI EditsPath BranchPath
    -- -- create and remove update directives
    -- | CreateEditsI EditGuid -- implies SetEdits?
    -- | SetEditsI EditGuid
    -- | ClearEdits -- don't record (don't allow?) term edits
    -- | ListEditsI EditGuid
    -- | ReplaceTermI EditGuid Reference Reference
    -- | ReplaceTypeI EditGuid Reference Reference
    -- -- clear updates for a term or type
    -- | RemoveAllTermUpdatesI EditGuid Reference
    -- | RemoveAllTypeUpdatesI EditGuid Reference
    -- -- resolve update conflicts
    -- | ChooseUpdateForTermI EditGuid Reference Reference
    -- | ChooseUpdateForTypeI EditGuid Reference Reference
  -- execute an IO object with arguments
  | ExecuteI String
  -- other
  | SlurpFileI -- todo (with Edits): AllowUpdates
  | SearchByNameI [String]
  | ShowDefinitionI OutputLocation [String]
  | UpdateBuiltinsI
  | QuitI
  deriving (Eq, Show)

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation
  | FileLocation FilePath
  -- ClipboardLocation
  deriving (Eq, Show)


-- Whether or not updates are allowed during file slurping
type AllowUpdates = Bool

data DisplayThing a = BuiltinThing | MissingThing Reference.Id | RegularThing a
  deriving (Eq, Ord, Show)

data SearchResult' v a
  = Tm' (TermResult' v a)
  | Tp' (TypeResult' v a)
  deriving (Eq, Show)
data TermResult' v a =
  TermResult' HashQualified (Maybe (Type v a)) Referent (Set HashQualified)
  deriving (Eq, Show)
data TypeResult' v a =
  TypeResult' HashQualified (DisplayThing (Decl v a)) Reference (Set HashQualified)
  deriving (Eq, Show)
pattern Tm n t r as = Tm' (TermResult' n t r as)
pattern Tp n t r as = Tp' (TypeResult' n t r as)

foldResult' :: (TermResult' v a -> b) -> (TypeResult' v a -> b) -> SearchResult' v a -> b
foldResult' f g = \case
  Tm' tm -> f tm
  Tp' tp -> g tp

type ListDetailed = Bool

data Output v
  -- Generic Success response; we might consider deleting this.
  -- I had put the `Input` field here in case we wanted the success message
  -- to vary based on the command the user submitted.
  = Success Input
  -- User did `add` or `update` before typechecking a file?
  | NoUnisonFile
  | RenameOutput Name Name NameChangeResult
  | AliasOutput Name Name NameChangeResult
  -- ask confirmation before deleting the last branch that contains some defns
  -- `Path` is one of the paths the user has requested to delete, and is paired
  -- with whatever named definitions would not have any remaining names if
  -- the path is deleted.
  | DeleteBranchConfirmation
      [(Path, (PPE.PrettyPrintEnv, [SearchResult' v Ann]))]
  -- list of all the definitions within this branch
  | ListOfDefinitions PPE.PrettyPrintEnv ListDetailed [SearchResult' v Ann]
  -- show the result of add/update
  | SlurpOutput (SlurpResult v)
  -- Original source, followed by the errors:
  | ParseErrors Text [Parser.Err v]
  | TypeErrors Text PPE.PrettyPrintEnv [Context.ErrorNote v Ann]
  | DisplayConflicts (Relation Name Referent) (Relation Name Reference)
  | Evaluated SourceFileContents
              PPE.PrettyPrintEnv
              [(v, Term v ())]
              (Map v (Ann, Term v (), Runtime.IsCacheHit))
  | Typechecked SourceName PPE.PrettyPrintEnv (UF.TypecheckedUnisonFile v Ann)
  | FileChangeEvent SourceName Text
  -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
  | DisplayDefinitions (Maybe FilePath)
                       PPE.PrettyPrintEnv
                       [(Reference, DisplayThing (Term v Ann))]
                       [(Reference, DisplayThing (Decl v Ann))]
  | TodoOutput PPE.PrettyPrintEnv (TodoOutput v Ann)
  -- | ListEdits Edits Branch0

  -- new/unrepresented references followed by old/removed
  -- todo: eventually replace these sets with [SearchResult' v Ann]
  -- and a nicer render.
  | BustedBuiltins (Set Reference) (Set Reference)
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
      todoConflicts :: OldBranch.Branch0
    } deriving (Show)

-- todo: do we want something here for nonexistent old name?
data NameChangeResult = NameChangeResult
  { oldNameConflicted :: Set NameTarget
  , newNameAlreadyExists :: Set NameTarget
  , changedSuccessfully :: Set NameTarget
  } deriving (Eq, Ord, Show)

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
type AmbientAbilities v = [Type.AnnotatedType v Ann]

data Command i v a where
  Input :: Command i v i

  -- Presents some output to the user
  Notify :: Output v -> Command i v ()

  -- This will load the namespace from the provided link, and
  -- give warnings about name conflicts and the like.
  -- If there are no warnings, or if the `CollisionHandler` specifies to ignore
  -- them, then this also writes the supplied definitions to `terms/`, `types/`.
  -- It does not write any namespace stuff.  (Maybe it should?)
  -- It may complain if you are trying to write definitions into a remote link,
  -- and suggest that you can convert the link to a fork if you want.

--  AddDefsToCodebase
--    :: -- CollisionHandler -> (todo)
--       Path
--    -> UF.TypecheckedUnisonFile v Ann
--    -> Command i v (Branch (Command i v), SlurpResult v)

  -- Arya: Do we need this?
  -- -- Load one level of a namespace.  It may involve reading from disk,
  -- -- or from http into a cache.
  -- GetBranch :: RepoLink Path -> Command i v Branch

  -- Typecheck a unison file relative to a particular link.
  -- If we want to be able to resolve relative names (seems unnecessary,
  -- at least in M1), we can keep a map from Link to parent in memory.
  Typecheck :: (AmbientAbilities v)
            -> RepoLink Path
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

  Evaluate :: UF.TypecheckedUnisonFile v Ann
           -> Command i v ([(v, Term v ())], Map v
                (Ann, UF.WatchKind, Reference, Term v (), Term v (), Runtime.IsCacheHit))


  -- Loads a root branch from some codebase, returning `Nothing` if not found.
  LoadRootBranch :: RepoRef -> Command i v (Maybe (Branch (Command i v)))
  LoadBranch :: RepoLink Branch.Hash -> Command i v (Maybe (Branch (Command i v)))

  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  SyncRootBranch :: RepoRef -> Branch (Command i v) -> Command i v ()
  -- e.g.
  --   /Lib/Arya/Public/SuperML> push github:aryairani/superML
  --   SynchRootBranch (Github "aryairani" "superML" "master")
  --                   (Branch at /Lib/Arya/Public/SuperML)

  LoadTerm :: Reference.Id -> Command i v (Maybe (Term v Ann))

  LoadType :: Reference.Id -> Command i v (Maybe (Decl v Ann))

  -- Loads some metadata for prettier search result display
  LoadSearchResults :: [SR.SearchResult] -> Command i v [SearchResult' v Ann]

  -- Execute a UnisonFile for its IO effects
  -- todo: Execute should do some evaluation?
  Execute :: UF.TypecheckedUnisonFile v Ann -> Command i v ()

-- -- Edits stuff:
--   Todo :: Edits -> Branch -> Command i v (TodoOutput v Ann)
--
--   Propagate :: Edits -> Branch -> Command i v Branch
--
--   -- copies an edit; needs some more ux design
--   PullEdits :: EditLink -> Command i v Bool
--   PushEdits :: EditLink -> Command i v Bool

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
--
-- typecheck
--   :: (Monad m, Var v)
--   => [Type.AnnotatedType v Ann]
--   -> Codebase m v Ann
--   -> Names
--   -> SourceName
--   -> Text
--   -> m (TypecheckingResult v)
-- typecheck ambient codebase names sourceName src =
--   Result.getResult $ parseAndSynthesizeFile ambient
--     (((<> B.typeLookup) <$>) . Codebase.typeLookupForDependencies codebase)
--     names
--     (unpack sourceName)
--     src
--
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

-- Separates type references from term references and returns terms and types,
-- respectively. For terms that are constructors, turns them into their data
-- types.
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
  -> (Output v -> IO ())
  -> Codebase IO v Ann
  -> Free (Command i v) a
  -> IO a
commandLine awaitInput rt notifyUser codebase command = do
  Free.fold go command
 where
  go :: forall x . Command i v x -> IO x
  go = \case
    -- Wait until we get either user input or a unison file update
    Input         -> awaitInput
    Notify output -> notifyUser output
--    AddDefsToCodebase handler branch unisonFile -> error "todo"
--      fileToBranch handler codebase branch unisonFile
    Typecheck ambient branch sourceName source -> error "todo"
--      typecheck ambient codebase (Branch.toNames branch) sourceName source
    Evaluate unisonFile -> evalUnisonFile unisonFile
    LoadBranch h -> error "todo"
    LoadRootBranch repo -> error "todo"
    SyncRootBranch repo branch -> error "todo"
    LoadTerm r -> CC.getTerm codebase r
    LoadType r -> CC.getTypeDeclaration codebase r
    LoadSearchResults results -> error "todo"
      -- loadSearchResults codebase results

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
--
-- loadSearchResults :: (Monad m, Var v) =>
--   Codebase m v a -> [SR.SearchResult] -> m [SearchResult' v a]
-- loadSearchResults code = traverse loadSearchResult
--   where
--   loadSearchResult = \case
--     SR.Tm (SR.TermResult name r aliases) -> do
--       typ <- case r of
--         Referent.Ref r -> Codebase.getTypeOfTerm code r
--         Referent.Con r cid -> Codebase.getTypeOfConstructor code r cid
--       pure $ Tm name typ r aliases
--     SR.Tp (SR.TypeResult name r aliases) -> do
--       dt <- case r of
--         Reference.Builtin _ -> pure BuiltinThing
--         Reference.DerivedId id ->
--           maybe (MissingThing id) RegularThing <$>
--             Codebase.getTypeDeclaration code id
--       pure $ Tp name dt r aliases
--
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
