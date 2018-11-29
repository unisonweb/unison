{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}



module Unison.Codebase.Editor where

import Control.Monad.Extra (ifM)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Foldable (toList)
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Parser (Ann)
import Unison.Result (Result, Note)
import Unison.Names (Name, NameTarget)
import Unison.Util.Free (Free)
import qualified Unison.Codebase.Branch as Branch
-- import qualified Unison.Codebase as Codebase
import qualified Unison.Names as Names
import qualified Unison.Parser as Parser
import qualified Unison.Result as Result
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Util.Free as Free
import qualified Unison.UnisonFile as UF
import qualified Unison.PrettyPrintEnv as PPE

type BranchName = Name
type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type TypecheckingResult v = Result (Seq (Note v Ann))  (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile' v Ann))

data AddOutput v
  = NothingToAdd
  | Added { -- Previously existed only in the file; now added to the codebase.
            successful :: UF.TypecheckedUnisonFile' v Ann
          -- Exists in the branch and the file, with the same name and contents.
          , duplicates :: UF.TypecheckedUnisonFile' v Ann
          -- Has a colliding name but a different definition than the codebase.
          , collisions :: UF.TypecheckedUnisonFile' v Ann }

data Input
  = AliasI NameTarget Name Name
  | RenameI NameTarget Name Name
  | UnnameI NameTarget Name
  | AddI -- [Name]
  | ListBranchesI
  | SwitchBranchI BranchName
  | ForkBranchI BranchName
  | MergeBranchI BranchName
  | QuitI

data Output v
  = Success Input Bool
  | NoUnisonFile
  | UnknownBranch BranchName
  | UnknownName BranchName NameTarget Name
  | NameAlreadyExists BranchName NameTarget Name
  -- `name` refers to more than one `nameTarget`
  | ConflictedName BranchName NameTarget Name
  | BranchAlreadyExists BranchName
  | ListOfBranches [BranchName]
  | AddOutput (AddOutput v)
  | ParseErrors [Parser.Err v]
  | TypeErrors PPE.PrettyPrintEnv [Context.ErrorNote v Ann]

data Command v a where
  Input :: Command v (Either (TypecheckingResult v) Input)

  -- Presents some output to the user
  Notify :: Output v -> Command v ()

  Add :: BranchName -> UF.TypecheckedUnisonFile' v Ann -> Command v (AddOutput v)

  Typecheck :: SourceName -> Source -> Command v (TypecheckingResult v)

  ListBranches :: Command v [BranchName]

  -- Loads a branch by name from the codebase, returning `Nothing` if not found.
  LoadBranch :: BranchName -> Command v (Maybe Branch)

  -- Returns `Nothing` if a branch by that name already exists.
  NewBranch :: BranchName -> Command v Bool

  -- Create a new branch which is a copy of the given branch, and assign the
  -- forked branch the given name. Returns `False` if the forked branch name
  -- already exists.
  ForkBranch :: Branch -> BranchName -> Command v Bool

  -- Merges the branch with the existing branch with the given name. Returns
  -- `Nothing` if no branch with that name exists.
  MergeBranch :: BranchName -> Branch -> Command v Bool

  -- Return the subset of the branch tip which is in a conflicted state
  GetConflicts :: BranchName -> Command v (Maybe Branch0)

  -- Tell the UI to display a set of conflicts
  DisplayConflicts :: Branch0 -> Command v ()

  -- RemainingWork :: Branch -> Command v [RemainingWork]

  -- idea here is to find "close matches" of stuff in the input file, so
  -- can suggest use of preexisting definitions
  -- Search :: UF.TypecheckedUnisonFile' v Ann -> Command v (UF.TypecheckedUnisonFile' v Ann?)


commandLine :: Codebase IO v Ann -> Free (Command v) a -> IO a
commandLine _codebase command = do
  -- set up file watching...
  Free.fold go command
  where
    go :: Command v a -> IO a
    go = undefined


data LoopState v
  = LoopState BranchName (Maybe (UF.TypecheckedUnisonFile' v Ann))

loop :: LoopState v -> Free (Command v) ()
loop s = Free.unfold' go s where
  go :: forall v. LoopState v -> Free (Command v) (Either () (LoopState v))
  go s@(LoopState currentBranchName uf) = do
    e <- Free.eval Input
    case e of
      Left (Result.Result notes r) -> case r of
        Nothing -> -- parsing failed
          repeatWithOutput $
            ParseErrors [ err | Result.Parsing err <- toList notes]
        Just (errorEnv, r) -> case r of
          Nothing -> -- typechecking failed
            repeatWithOutput $
              TypeErrors errorEnv [ err | Result.TypeError err <- toList notes]
          Just unisonFile -> updateUnisonFile unisonFile
      Right input -> case input of
        AliasI nameTarget existingName newName -> do
          branch <- Free.eval (LoadBranch currentBranchName)
          case branch of
            Nothing -> repeatWithOutput $ UnknownBranch currentBranchName
            Just branch ->
              let wrangle :: Foldable f
                          => (Name -> Branch -> f a)
                          -> (a -> Name -> Branch -> Branch)
                          -> Free (Command v) (Either () (LoopState v))
                  wrangle named add =
                    if null (named newName branch)
                    then repeatWithOutput $
                      NameAlreadyExists currentBranchName nameTarget newName
                    else case toList (named existingName branch) of
                      [t] -> do
                        success <- Free.eval . MergeBranch currentBranchName $
                          add t newName branch
                        if success then repeatWithOutput (Success input True)
                        else repeatWithOutput $ UnknownBranch currentBranchName
                      [] -> repeatWithOutput $
                        UnknownName currentBranchName nameTarget existingName
                      _ -> repeatWithOutput $
                        ConflictedName currentBranchName nameTarget existingName
              in case nameTarget of
                Names.TermName ->
                  wrangle Branch.termsNamed Branch.addTermName
                Names.PatternName ->
                  wrangle Branch.patternsNamed (uncurry Branch.addPatternName)
                Names.TypeName ->
                  wrangle Branch.typesNamed Branch.addTypeName

        RenameI _ _ _ -> error "todo"
        -- RenameI nameTarget oldName newName -> do
        --   branch <- Free.eval (LoadBranch currentBranchName)
        --   case branch of
        --     Nothing -> repeatWithOutput $ UnknownBranch currentBranchName
        --     Just branch ->
        --       let errorUnknownName = repeatWithOutput $
        --             UnknownName currentBranchName nameTarget oldName
        --           errorNameExists = repeatWithOutput $
        --             NameAlreadyExists currentBranchName nameTarget newName
        --           errorConflictedName = repeatWithOutput $
        --             ConflictedName currentBranchName nameTarget existingName
        --       in case nameTarget of
        --         Names.TermName ->
        --           if branch.hasTermNamed newName branch
        --           then errorNameExists
        --           else case toList (Branch.termsNamed oldName branch) of
        --             [t] -> Free.eval (MergeBranch currentBranchName (Branch.renameTerm oldName newName branch)) >> repeatWithOutput (Success input True)
        --             [] -> errorUnknownName
        --             _ -> errorConflictedName

        UnnameI _nameTarget _name -> error "todo"
        --   (Free.eval $ Unname currentBranchName nameTarget name) >>=
        --     (repeatWithOutput . Success input)
        AddI -> case uf of
          Nothing -> repeatWithOutput NoUnisonFile
          Just uf -> do
            (Free.eval $ Add currentBranchName uf) >>=
              (repeatWithOutput . AddOutput)
        ListBranchesI -> do
          Free.eval ListBranches >>= repeatWithOutput . ListOfBranches
        SwitchBranchI branchName -> switchBranch branchName
        ForkBranchI targetBranchName ->
          loadBranchOrComplain currentBranchName $ \branch -> do
            ifM (Free.eval $ ForkBranch branch targetBranchName)
                (do
                  Free.eval . Notify $ Success input True
                  switchBranch targetBranchName)
                (repeatWithOutput $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranch -> do
          branch <- Free.eval $ LoadBranch inputBranch
          case branch of
            Nothing -> repeatWithOutput $ UnknownBranch inputBranch
            Just branch ->
              (Free.eval $ MergeBranch currentBranchName branch) >>=
                (repeatWithOutput . Success input)
        QuitI -> quit
    where
      repeat = pure $ Right s
      repeatWithOutput output = (Free.eval . Notify) output >> repeat
      switchBranch branchName = pure . Right $ LoopState branchName uf
      updateUnisonFile :: forall f v. Applicative f => UF.TypecheckedUnisonFile' v Ann -> f (Either () (LoopState v))
      updateUnisonFile = pure . Right . LoopState currentBranchName . Just
      quit = pure $ Left ()
      loadBranchOrComplain ::
        BranchName -> (Branch -> Free (Command v) (Either () (LoopState v))) -> Free (Command v) (Either () (LoopState v))
      loadBranchOrComplain branchName f = do
        branch <- Free.eval $ LoadBranch branchName
        case branch of
          Nothing -> do
            Free.eval . Notify $ UnknownBranch branchName
            repeat
          Just branch -> f branch

-- commandLine :: Codebase -> IO (Command v) a -> IO a
--
-- interact :: IO Line -> IO (SourceName, Source) -> FreeT (Command v) IO ()
--
-- -- data Free f a = Pure a | forall x . Bind (f x) (x -> Free f a)
--
-- do
--   line <- lift $ getLine
