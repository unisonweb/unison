{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Unison.Codebase.Editor where

import           Control.Monad.Extra        (ifM)
import           Data.Foldable              (toList)
import           Data.Sequence              (Seq)
import           Data.Text                  (Text)
import           Unison.Codebase            (Codebase)
import           Unison.Codebase.Branch     (Branch, Branch0)
import qualified Unison.Codebase.Branch     as Branch
import           Unison.Names               (Name, NameTarget, Referent)
import           Unison.Parser              (Ann)
import           Unison.Result              (Note, Result)
import           Unison.Util.Free           (Free)
-- import qualified Unison.Codebase as Codebase
import qualified Unison.Names               as Names
import qualified Unison.Parser              as Parser
import qualified Unison.PrettyPrintEnv      as PPE
import           Unison.Reference           (Reference)
import qualified Unison.Result              as Result
import qualified Unison.Term                as Term
import qualified Unison.Type                as Type
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile          as UF
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

data Input
  = AliasUnconflictedI NameTarget Name Name
  | RenameUnconflictedI NameTarget Name Name
  | UnnameAllI NameTarget Name
  | AddI -- [Name]
  | ListBranchesI
  | FuzzySearchByNameI String
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
  | SearchResult BranchName String [(Name, Referent, Type v Ann)] [(Name, Reference {-, Kind -})] [(Name, Reference, Int, Type v Ann)]
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
        FuzzySearchByNameI _ -> error "todo"
        AliasUnconflictedI nameTarget existingName newName -> do
          branch <- Free.eval (LoadBranch currentBranchName)
          case branch of
            Nothing -> repeatWithOutput $ UnknownBranch currentBranchName
            Just branch ->
              let alias :: Foldable f
                        => (Name -> Branch -> f a)
                        -> (a -> Name -> Branch -> Branch)
                        -> Free (Command v) (Either () (LoopState v))
                  alias named add =
                    if (not . null) (named newName branch)
                    then repeatWithOutput
                      (NameAlreadyExists currentBranchName nameTarget newName)
                    else case toList (named existingName branch) of
                      [t] ->
                        ifM (Free.eval . MergeBranch currentBranchName $
                          add t newName branch)
                            (repeatWithOutput (Success input))
                            (repeatWithOutput (UnknownBranch currentBranchName))
                      [] -> repeatWithOutput $
                        UnknownName currentBranchName nameTarget existingName
                      _ -> repeatWithOutput $
                        ConflictedName currentBranchName nameTarget existingName
              in case nameTarget of
                Names.TermName -> alias Branch.termsNamed Branch.addTermName
                Names.TypeName -> alias Branch.typesNamed Branch.addTypeName
                Names.PatternName -> alias Branch.patternsNamed (uncurry Branch.addPatternName)

        RenameUnconflictedI nameTarget oldName newName -> do
          branch <- Free.eval (LoadBranch currentBranchName)
          case branch of
            Nothing -> repeatWithOutput $ UnknownBranch currentBranchName
            Just branch ->
              let rename :: Foldable f
                        => (Name -> Branch -> f a)
                        -> (Name -> Name -> Branch -> Branch)
                        -> Free (Command v) (Either () (LoopState v))
                  rename named rename' =
                    if (not . null) (named newName branch)
                    then repeatWithOutput (NameAlreadyExists currentBranchName nameTarget newName)
                    else case toList (named oldName branch) of
                      [_] -> -- unique, unconflicted name
                        ifM (Free.eval . MergeBranch currentBranchName $ rename' oldName newName branch)
                          (repeatWithOutput (Success input))
                          (repeatWithOutput (UnknownBranch currentBranchName))
                      [] -> repeatWithOutput
                        (UnknownName currentBranchName nameTarget oldName)
                      _ -> repeatWithOutput
                        (ConflictedName currentBranchName nameTarget oldName)
              in case nameTarget of
                Names.TermName -> rename Branch.termsNamed Branch.renameTerm
                Names.TypeName -> rename Branch.typesNamed Branch.renameType
                Names.PatternName -> rename Branch.patternsNamed Branch.renamePattern

        UnnameAllI nameTarget name -> do
          branch <- Free.eval (LoadBranch currentBranchName)
          case branch of
            Nothing -> repeatWithOutput (UnknownBranch currentBranchName)
            Just branch ->
              let unname :: (Name -> Branch -> Branch)
                         -> Free (Command v) (Either () (LoopState v))
                  unname unname' =
                    ifM (Free.eval . MergeBranch currentBranchName $ unname' name branch)
                        (repeatWithOutput (Success input))
                        (repeatWithOutput (UnknownBranch currentBranchName))
              in case nameTarget of
                Names.TermName    -> unname Branch.deleteTermsNamed
                Names.TypeName    -> unname Branch.deleteTypesNamed
                Names.PatternName -> unname Branch.deletePatternsNamed

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
                  Free.eval . Notify $ Success input
                  switchBranch targetBranchName)
                (repeatWithOutput $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranch -> do
          branch <- Free.eval $ LoadBranch inputBranch
          case branch of
            Nothing -> repeatWithOutput $ UnknownBranch inputBranch
            Just branch ->
              ifM (Free.eval $ MergeBranch currentBranchName branch)
                  (repeatWithOutput $ Success input)
                  (repeatWithOutput $ UnknownBranch inputBranch)
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
