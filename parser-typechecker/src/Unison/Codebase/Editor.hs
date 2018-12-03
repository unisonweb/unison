{-# LANGUAGE GADTs #-}

module Unison.Codebase.Editor where

import           Data.Sequence              (Seq)
import           Data.Text                  (Text)
import           Unison.Codebase            (Codebase)
import           Unison.Codebase.Branch     (Branch, Branch0)
-- import           Unison.DataDeclaration     (DataDeclaration', EffectDeclaration')
import           Unison.Names               (Name, NameTarget, Referent)
import           Unison.Parser              (Ann)
import qualified Unison.Parser              as Parser
import qualified Unison.PrettyPrintEnv      as PPE
import           Unison.Reference           (Reference)
import           Unison.Result              (Note, Result)
import qualified Unison.Term                as Term
import qualified Unison.Type                as Type
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile          as UF
import           Unison.Util.Free           (Free)
import qualified Unison.Util.Free           as Free

type BranchName = Name
type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type TypecheckingResult v = Result (Seq (Note v Ann))  (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile' v Ann))
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data AddOutput v
  = NothingToAdd
  | Added { -- Previously existed only in the file; now added to the codebase.
            successful :: UF.TypecheckedUnisonFile' v Ann
          -- Exists in the branch and the file, with the same name and contents.
          , duplicates :: UF.TypecheckedUnisonFile' v Ann
          -- Has a colliding name but a different definition than the codebase.
          , collisions :: UF.TypecheckedUnisonFile' v Ann }

data SearchType = Exact | Fuzzy

data Input
  -- high-level manipulation of names
  = AliasUnconflictedI NameTarget Name Name
  | RenameUnconflictedI NameTarget Name Name
  | UnnameAllI NameTarget Name
  -- low-level manipulation of names
  | AddTermNameI Referent Name
  | AddTypeNameI Reference Name
  | AddPatternNameI Reference Int Name
  | RemoveTermNameI Referent Name
  | RemoveTypeNameI Reference Name
  | RemovePatternNameI Reference Int Name
  -- resolving naming conflicts
  | ChooseTermForNameI Referent Name
  | ChooseTypeForNameI Reference Name
  | ChoosePatternForNameI Reference Int Name
  -- create and remove update directives
  | ListAllUpdatesI
    -- update a term or type, error if it would produce a conflict
  | UpdateTermI Referent Referent
  | UpdateTypeI Reference Reference
    -- clear updates for a term or type
  | RemoveAllTermUpdatesI Referent
  | RemoveAllTypeUpdatesI Reference
  -- resolve update conflicts
  | ChooseUpdateForTermI Referent Referent
  | ChooseUpdateForTypeI Reference Reference
  -- other
  | AddI -- [Name]
  | ListBranchesI
  | SearchByNameI SearchType String
  | SwitchBranchI BranchName
  | ForkBranchI BranchName
  | MergeBranchI BranchName
  | QuitI

data Output v
  = Success Input
  | NoUnisonFile
  | UnknownBranch BranchName
  | UnknownName BranchName NameTarget Name
  | NameAlreadyExists BranchName NameTarget Name
  -- `name` refers to more than one `nameTarget`
  | ConflictedName BranchName NameTarget Name
  | BranchAlreadyExists BranchName
  | ListOfBranches [BranchName]
  | SearchResult BranchName SearchType String [(Name, Referent, Type v Ann)] [(Name, Reference {-, Kind -})] [(Name, Reference, Int, Type v Ann)]
  | AddOutput (AddOutput v)
  | ParseErrors [Parser.Err v]
  | TypeErrors PPE.PrettyPrintEnv [Context.ErrorNote v Ann]

data Command i v a where
  Input :: Command i v (Either (TypecheckingResult v) i)

  -- Presents some output to the user
  Notify :: Output v -> Command i v ()

  Add :: BranchName -> UF.TypecheckedUnisonFile' v Ann -> Command i v (AddOutput v)

  Typecheck :: SourceName -> Source -> Command i v (TypecheckingResult v)

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

  -- Returns `Nothing` if a branch by that name already exists.
  NewBranch :: BranchName -> Command i v Bool

  -- Create a new branch which is a copy of the given branch, and assign the
  -- forked branch the given name. Returns `False` if the forked branch name
  -- already exists.
  ForkBranch :: Branch -> BranchName -> Command i v Bool

  -- Merges the branch with the existing branch with the given name. Returns
  -- `Nothing` if no branch with that name exists.
  MergeBranch :: BranchName -> Branch -> Command i v Bool

  -- Return the subset of the branch tip which is in a conflicted state
  GetConflicts :: BranchName -> Command i v (Maybe Branch0)

  -- Tell the UI to display a set of conflicts
  DisplayConflicts :: Branch0 -> Command i v ()

  -- RemainingWork :: Branch -> Command i v [RemainingWork]

  -- idea here is to find "close matches" of stuff in the input file, so
  -- can suggest use of preexisting definitions
  -- Search :: UF.TypecheckedUnisonFile' v Ann -> Command v (UF.TypecheckedUnisonFile' v Ann?)


commandLine :: Codebase IO v Ann -> Free (Command Input v) a -> IO a
commandLine _codebase command = do
  -- set up file watching...
  Free.fold go command
  where
    go :: Command Input v a -> IO a
    go = undefined
