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
  | SearchResult BranchName String [(Name, Referent, Type v Ann)] [(Name, Reference {-, Kind -})] [(Name, Reference, Int, Type v Ann)]
  | AddOutput (AddOutput v)
  | ParseErrors [Parser.Err v]
  | TypeErrors PPE.PrettyPrintEnv [Context.ErrorNote v Ann]

data Command i v a where
  Input :: Command i v (Either (TypecheckingResult v) i)

  -- Presents some output to the user
  Notify :: Output v -> Command i v ()

  Add :: BranchName -> UF.TypecheckedUnisonFile' v Ann -> Command i v (AddOutput v)

  Typecheck :: SourceName -> Source -> Command i v (TypecheckingResult v)

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


data LoopState v
  = LoopState BranchName (Maybe (UF.TypecheckedUnisonFile' v Ann))

loop :: LoopState v -> Free (Command Input v) ()
loop s = Free.unfold' go s where
  go :: forall v. LoopState v -> Free (Command Input v) (Either () (LoopState v))
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
        SearchByNameI _ _ -> error "todo"
        AddTermNameI _ _ -> error "todo"
        AddTypeNameI _ _ -> error "todo"
        AddPatternNameI _ _ _ -> error "todo"
        RemoveTermNameI _ _ -> error "todo"
        RemoveTypeNameI _ _ -> error "todo"
        RemovePatternNameI _ _ _ -> error "todo"
        ChooseTermForNameI _ _ -> error "todo"
        ChooseTypeForNameI _ _ -> error "todo"
        ChoosePatternForNameI _ _ _ -> error "todo"
        AliasUnconflictedI nameTarget existingName newName ->
          aliasUnconflicted currentBranchName repeatWithOutput (Success input) nameTarget existingName newName
        RenameUnconflictedI nameTarget oldName newName ->
          renameUnconflicted currentBranchName repeatWithOutput (Success input) nameTarget oldName newName
        UnnameAllI nameTarget name ->
          unnameAll currentBranchName repeatWithOutput (Success input) nameTarget name

        AddI -> case uf of
          Nothing -> repeatWithOutput NoUnisonFile
          Just uf ->
            (Free.eval $ Add currentBranchName uf) >>=
              (repeatWithOutput . AddOutput)
        ListBranchesI ->
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
      repeat :: forall i. Free (Command i v) (Either () (LoopState v))
      repeat = pure $ Right s
      repeatWithOutput output = (Free.eval . Notify) output >> repeat
      switchBranch branchName = pure . Right $ LoopState branchName uf
      updateUnisonFile :: forall f v. Applicative f => UF.TypecheckedUnisonFile' v Ann -> f (Either () (LoopState v))
      updateUnisonFile = pure . Right . LoopState currentBranchName . Just
      quit = pure $ Left ()
      loadBranchOrComplain :: forall i.
        BranchName -> (Branch -> Free (Command i v) (Either () (LoopState v))) -> Free (Command i v) (Either () (LoopState v))
      loadBranchOrComplain branchName f = do
        branch <- Free.eval $ LoadBranch branchName
        case branch of
          Nothing -> do
            Free.eval . Notify $ UnknownBranch branchName
            repeat
          Just branch -> f branch

aliasUnconflicted :: forall i v. BranchName -> (Output v -> Free (Command i v) (Either () (LoopState v))) -> Output v -> NameTarget -> Name -> Name -> Free (Command i v) (Either () (LoopState v))
aliasUnconflicted currentBranchName repeatWithOutput success nameTarget existingName newName = do
  branch <- Free.eval (LoadBranch currentBranchName)
  case branch of
    Nothing -> repeatWithOutput $ UnknownBranch currentBranchName
    Just branch ->
      let alias :: Foldable f
                => (Name -> Branch -> f a)
                -> (a -> Name -> Branch -> Branch)
                -> Free (Command i v) (Either () (LoopState v))
          alias named add =
            if (not . null) (named newName branch)
            then repeatWithOutput
              (NameAlreadyExists currentBranchName nameTarget newName)
            else case toList (named existingName branch) of
              [t] ->
                ifM (Free.eval . MergeBranch currentBranchName $
                  add t newName branch)
                    (repeatWithOutput success)
                    (repeatWithOutput (UnknownBranch currentBranchName))
              [] -> repeatWithOutput $
                UnknownName currentBranchName nameTarget existingName
              _ -> repeatWithOutput $
                ConflictedName currentBranchName nameTarget existingName
      in case nameTarget of
        Names.TermName -> alias Branch.termsNamed Branch.addTermName
        Names.TypeName -> alias Branch.typesNamed Branch.addTypeName
        Names.PatternName -> alias Branch.patternsNamed (uncurry Branch.addPatternName)

renameUnconflicted :: forall i v. BranchName -> (Output v -> Free (Command i v) (Either () (LoopState v))) -> Output v -> NameTarget -> Name -> Name -> Free (Command i v) (Either () (LoopState v))
renameUnconflicted currentBranchName repeatWithOutput success nameTarget oldName newName = do
  branch <- Free.eval (LoadBranch currentBranchName)
  case branch of
    Nothing -> repeatWithOutput $ UnknownBranch currentBranchName
    Just branch ->
      let rename :: Foldable f
                => (Name -> Branch -> f a)
                -> (Name -> Name -> Branch -> Branch)
                -> Free (Command i v) (Either () (LoopState v))
          rename named rename' =
            if (not . null) (named newName branch)
            then repeatWithOutput (NameAlreadyExists currentBranchName nameTarget newName)
            else case toList (named oldName branch) of
              [_] -> -- unique, unconflicted name
                ifM (Free.eval . MergeBranch currentBranchName $ rename' oldName newName branch)
                  (repeatWithOutput success)
                  (repeatWithOutput (UnknownBranch currentBranchName))
              [] -> repeatWithOutput
                (UnknownName currentBranchName nameTarget oldName)
              _ -> repeatWithOutput
                (ConflictedName currentBranchName nameTarget oldName)
      in case nameTarget of
        Names.TermName -> rename Branch.termsNamed Branch.renameTerm
        Names.TypeName -> rename Branch.typesNamed Branch.renameType
        Names.PatternName -> rename Branch.patternsNamed Branch.renamePattern

unnameAll :: forall i v. BranchName -> (Output v -> Free (Command i v) (Either () (LoopState v))) -> Output v -> NameTarget -> Name -> Free (Command i v) (Either () (LoopState v))
unnameAll currentBranchName repeatWithOutput success nameTarget name = do
  branch <- Free.eval (LoadBranch currentBranchName)
  case branch of
    Nothing -> repeatWithOutput (UnknownBranch currentBranchName)
    Just branch ->
      let unname :: (Name -> Branch -> Branch)
                 -> Free (Command i v) (Either () (LoopState v))
          unname unname' =
            ifM (Free.eval . MergeBranch currentBranchName $ unname' name branch)
                (repeatWithOutput success)
                (repeatWithOutput (UnknownBranch currentBranchName))
      in case nameTarget of
        Names.TermName    -> unname Branch.deleteTermsNamed
        Names.TypeName    -> unname Branch.deleteTypesNamed
        Names.PatternName -> unname Branch.deletePatternsNamed
-- commandLine :: Codebase -> IO (Command v) a -> IO a
--
-- interact :: IO Line -> IO (SourceName, Source) -> FreeT (Command v) IO ()
--
-- -- data Free f a = Pure a | forall x . Bind (f x) (x -> Free f a)
--
-- do
--   line <- lift $ getLine
