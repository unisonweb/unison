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
type Action i v = Free (Command i v) (Either () (LoopState v))


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
  go :: forall v. LoopState v -> Action Input v
  go s@(LoopState currentBranchName uf) = do
    e <- Free.eval Input
    case e of
      Left (Result.Result notes r) -> case r of
        Nothing -> -- parsing failed
          repeat $
            ParseErrors [ err | Result.Parsing err <- toList notes]
        Just (errorEnv, r) -> case r of
          Nothing -> -- typechecking failed
            repeat $
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
        ChooseTermForNameI r name ->
          unnameAll currentBranchName repeat Names.TermName name $
            addTermName currentBranchName repeat success r name
        ChooseTypeForNameI r name ->
          unnameAll currentBranchName repeat Names.TypeName name $
            addTypeName currentBranchName repeat success r name
        ChoosePatternForNameI r i name ->
          unnameAll currentBranchName repeat Names.PatternName name $
            addPatternName currentBranchName repeat success r i name
        AliasUnconflictedI nameTarget existingName newName ->
          aliasUnconflicted currentBranchName repeat nameTarget existingName newName success
        RenameUnconflictedI nameTarget oldName newName ->
          renameUnconflicted currentBranchName repeat nameTarget oldName newName success
        UnnameAllI nameTarget name ->
          unnameAll currentBranchName repeat nameTarget name success
        AddI -> case uf of
          Nothing -> repeat NoUnisonFile
          Just uf ->
            (Free.eval $ Add currentBranchName uf) >>= (repeat . AddOutput)
        ListBranchesI ->
          Free.eval ListBranches >>= repeat . ListOfBranches
        SwitchBranchI branchName -> switchBranch branchName
        ForkBranchI targetBranchName ->
          withBranch currentBranchName repeat $ \branch -> do
            ifM (Free.eval $ ForkBranch branch targetBranchName)
                (outputSuccess >> switchBranch targetBranchName)
                (repeat $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranchName ->
          mergeBranch inputBranchName currentBranchName repeat success
        QuitI -> quit
        where success = repeat $ Success input
              outputSuccess = (Free.eval . Notify) (Success input)
    where
      repeat :: Output v -> Action i v
      repeat output = (Free.eval . Notify) output >> pure (Right s)
      switchBranch branchName = pure . Right $ LoopState branchName uf
      updateUnisonFile :: forall f v. Applicative f => UF.TypecheckedUnisonFile' v Ann -> f (Either () (LoopState v))
      updateUnisonFile = pure . Right . LoopState currentBranchName . Just
      quit = pure $ Left ()

withBranch :: BranchName -> (Output v -> Action i v) -> (Branch -> Action i v) -> Action i v
withBranch branchName respond f = do
  branch <- Free.eval $ LoadBranch branchName
  case branch of
    Nothing -> respond $ UnknownBranch branchName
    Just branch -> f branch

aliasUnconflicted :: forall i v. BranchName -> (Output v -> Action i v) -> NameTarget -> Name -> Name -> Action i v -> Action i v
aliasUnconflicted currentBranchName repeat nameTarget existingName newName success = do
  branch <- Free.eval (LoadBranch currentBranchName)
  case branch of
    Nothing -> repeat $ UnknownBranch currentBranchName
    Just branch ->
      let alias :: Foldable f
                => (Name -> Branch -> f a)
                -> (a -> Name -> Branch -> Branch)
                -> Action i v
          alias named add =
            if (not . null) (named newName branch)
            then repeat (NameAlreadyExists currentBranchName nameTarget newName)
            else case toList (named existingName branch) of
              [t] ->
                ifM (Free.eval . MergeBranch currentBranchName $
                  add t newName branch)
                    success
                    (repeat $ UnknownBranch currentBranchName)
              [] -> repeat $
                UnknownName currentBranchName nameTarget existingName
              _ -> repeat $
                ConflictedName currentBranchName nameTarget existingName
      in case nameTarget of
        Names.TermName -> alias Branch.termsNamed Branch.addTermName
        Names.TypeName -> alias Branch.typesNamed Branch.addTypeName
        Names.PatternName -> alias Branch.patternsNamed (uncurry Branch.addPatternName)

renameUnconflicted :: forall i v. BranchName -> (Output v -> Action i v) -> NameTarget -> Name -> Name -> Action i v -> Action i v
renameUnconflicted currentBranchName repeat nameTarget oldName newName success = do
  branch <- Free.eval (LoadBranch currentBranchName)
  case branch of
    Nothing -> repeat $ UnknownBranch currentBranchName
    Just branch ->
      let rename :: Foldable f
                => (Name -> Branch -> f a)
                -> (Name -> Name -> Branch -> Branch)
                -> Action i v
          rename named rename' =
            if (not . null) (named newName branch)
            then repeat (NameAlreadyExists currentBranchName nameTarget newName)
            else case toList (named oldName branch) of
              [_] -> -- unique, unconflicted name
                ifM (Free.eval . MergeBranch currentBranchName $ rename' oldName newName branch)
                  success
                  (repeat (UnknownBranch currentBranchName))
              [] -> repeat
                (UnknownName currentBranchName nameTarget oldName)
              _ -> repeat
                (ConflictedName currentBranchName nameTarget oldName)
      in case nameTarget of
        Names.TermName -> rename Branch.termsNamed Branch.renameTerm
        Names.TypeName -> rename Branch.typesNamed Branch.renameType
        Names.PatternName -> rename Branch.patternsNamed Branch.renamePattern

unnameAll :: forall i v
          . BranchName
          -> (Output v -> Action i v)
          -> NameTarget
          -> Name
          -> Action i v
          -> Action i v
unnameAll currentBranchName repeat nameTarget name success = do
  branch <- Free.eval (LoadBranch currentBranchName)
  case branch of
    Nothing -> repeat (UnknownBranch currentBranchName)
    Just branch ->
      let unname :: (Name -> Branch -> Branch)
                 -> Action i v
          unname unname' =
            ifM (Free.eval . MergeBranch currentBranchName $ unname' name branch)
                success
                (repeat (UnknownBranch currentBranchName))
      in case nameTarget of
        Names.TermName    -> unname Branch.deleteTermsNamed
        Names.TypeName    -> unname Branch.deleteTypesNamed
        Names.PatternName -> unname Branch.deletePatternsNamed

addTermName :: BranchName -> (Output v -> Action i v) -> Action i v -> Referent -> Name -> Action i v
addTermName currentBranchName respond success r name =
  updateBranch currentBranchName (Branch.addTermName r name) respond success

addTypeName :: BranchName -> (Output v -> Action i v) -> Action i v -> Reference -> Name -> Action i v
addTypeName = undefined

addPatternName :: BranchName -> (Output v -> Action i v) -> Action i v -> Reference -> Int -> Name -> Action i v
addPatternName = undefined

mergeBranch :: BranchName -> BranchName -> (Output v -> Action i v) -> Action i v -> Action i v
mergeBranch sourceBranchName targetBranchName respond success =
  withBranch sourceBranchName respond $ \b ->
    mergeBranch' targetBranchName b respond success

mergeBranch' :: BranchName -> Branch -> (Output v -> Action i v) -> Action i v -> Action i v
mergeBranch' targetBranchName s respond success =
  ifM (Free.eval $ MergeBranch targetBranchName s)
      success
      (respond $ UnknownBranch targetBranchName)

updateBranch :: BranchName -> (Branch -> Branch) -> (Output v -> Action i v) -> Action i v -> Action i v
updateBranch branchName f respond success =
  withBranch branchName respond $
    \b -> mergeBranch' branchName (f b) respond success


-- commandLine :: Codebase -> IO (Command v) a -> IO a
--
-- interact :: IO Line -> IO (SourceName, Source) -> FreeT (Command v) IO ()
--
-- -- data Free f a = Pure a | forall x . Bind (f x) (x -> Free f a)
--
-- do
--   line <- lift $ getLine
