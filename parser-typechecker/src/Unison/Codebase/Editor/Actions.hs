{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.Editor.Actions where

import           Control.Monad.Extra    (ifM)
import           Data.Foldable          (toList)
import           Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import           Unison.Codebase.Editor
import           Unison.Names           (Name, NameTarget, Referent)
import qualified Unison.Names           as Names
import           Unison.Parser          (Ann)
import           Unison.Reference       (Reference)
import qualified Unison.Result          as Result
import qualified Unison.UnisonFile      as UF
import           Unison.Util.Free       (Free)
import qualified Unison.Util.Free       as Free

type Action i v = Free (Command i v) (Either () (LoopState v))

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
          respond $
            ParseErrors [ err | Result.Parsing err <- toList notes]
        Just (errorEnv, r) -> case r of
          Nothing -> -- typechecking failed
            respond $
              TypeErrors errorEnv [ err | Result.TypeError err <- toList notes]
          Just unisonFile -> updateUnisonFile unisonFile
      Right input -> case input of
        SearchByNameI _ _ -> error "todo"
        UpdateTermI _old _new -> error "todo"
        UpdateTypeI _old _new -> error "todo"
        RemoveAllTermUpdatesI _t -> error "todo"
        RemoveAllTypeUpdatesI _t -> error "todo"
        ChooseUpdateForTermI _old _new -> error "todo"
        ChooseUpdateForTypeI _old _new -> error "todo"
        ListAllUpdatesI -> error "todo"
        AddTermNameI r name ->
          addTermName currentBranchName respond success r name
        AddTypeNameI r name ->
          addTypeName currentBranchName respond success r name
        AddPatternNameI r i name ->
          addPatternName currentBranchName respond success r i name
        RemoveTermNameI r name ->
          updateBranch respond success currentBranchName $
            Branch.deleteTermName r name
        RemoveTypeNameI r name ->
          updateBranch respond success currentBranchName $
            Branch.deleteTypeName r name
        RemovePatternNameI r i name ->
          updateBranch respond success currentBranchName $
            Branch.deletePatternName r i name
        ChooseTermForNameI r name ->
          unnameAll currentBranchName respond Names.TermName name $
            addTermName currentBranchName respond success r name
        ChooseTypeForNameI r name ->
          unnameAll currentBranchName respond Names.TypeName name $
            addTypeName currentBranchName respond success r name
        ChoosePatternForNameI r i name ->
          unnameAll currentBranchName respond Names.PatternName name $
            addPatternName currentBranchName respond success r i name
        AliasUnconflictedI nameTarget existingName newName ->
          aliasUnconflicted currentBranchName respond nameTarget existingName newName success
        RenameUnconflictedI nameTarget oldName newName ->
          renameUnconflicted currentBranchName respond nameTarget oldName newName success
        UnnameAllI nameTarget name ->
          unnameAll currentBranchName respond nameTarget name success
        AddI -> case uf of
          Nothing -> respond NoUnisonFile
          Just uf -> do
            branch <- Free.eval $ LoadBranch currentBranchName
            case branch of
              Nothing -> respond $ UnknownBranch currentBranchName
              Just branch -> Free.eval (Add branch uf) >>= (respond . AddOutput)
        ListBranchesI ->
          Free.eval ListBranches >>= respond . ListOfBranches
        SwitchBranchI branchName -> switchBranch branchName
        ForkBranchI targetBranchName ->
          withBranch currentBranchName respond $ \branch -> do
            ifM (Free.eval $ ForkBranch branch targetBranchName)
                (outputSuccess >> switchBranch targetBranchName)
                (respond $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranchName ->
          mergeBranch inputBranchName currentBranchName respond success
        QuitI -> quit
        where success = respond $ Success input
              outputSuccess = (Free.eval . Notify) (Success input)
    where
      respond :: Output v -> Action i v
      respond output = (Free.eval . Notify) output >> pure (Right s)
      switchBranch branchName = pure . Right $ LoopState branchName uf
      updateUnisonFile :: forall f v. Applicative f => UF.TypecheckedUnisonFile' v Ann -> f (Either () (LoopState v))
      updateUnisonFile = pure . Right . LoopState currentBranchName . Just
      quit = pure $ Left ()

withBranch :: BranchName -> (Output v -> Action i v) -> (Branch -> Action i v) -> Action i v
withBranch branchName respond f = do
  branch <- Free.eval $ LoadBranch branchName
  case branch of
    Nothing     -> respond $ UnknownBranch branchName
    Just branch -> f branch

aliasUnconflicted :: forall i v. BranchName -> (Output v -> Action i v) -> NameTarget -> Name -> Name -> Action i v -> Action i v
aliasUnconflicted branchName respond nameTarget existingName newName success =
  withBranch branchName respond $ case nameTarget of
    Names.TermName -> alias Branch.termsNamed Branch.addTermName
    Names.TypeName -> alias Branch.typesNamed Branch.addTypeName
    Names.PatternName -> alias Branch.patternsNamed (uncurry Branch.addPatternName)
  where
    alias :: Foldable f
          => (Name -> Branch -> f a)
          -> (a -> Name -> Branch -> Branch)
          -> Branch
          -> Action i v
    alias named alias' branch =
      if (not . null) (named newName branch)
      then respond (NameAlreadyExists branchName nameTarget newName)
      else case toList (named existingName branch) of
        [t] -> mergeBranch' branchName respond success $ alias' t newName branch
        [] -> respond (UnknownName branchName nameTarget existingName)
        _ -> respond (ConflictedName branchName nameTarget existingName)

renameUnconflicted :: forall i v. BranchName -> (Output v -> Action i v) -> NameTarget -> Name -> Name -> Action i v -> Action i v
renameUnconflicted branchName respond nameTarget oldName newName success =
  withBranch branchName respond $ case nameTarget of
    Names.TermName    -> rename Branch.termsNamed Branch.renameTerm
    Names.TypeName    -> rename Branch.typesNamed Branch.renameType
    Names.PatternName -> rename Branch.patternsNamed Branch.renamePattern
  where
    rename :: Foldable f
           => (Name -> Branch -> f a)
           -> (Name -> Name -> Branch -> Branch)
           -> Branch
           -> Action i v
    rename named rename' branch =
      if (not . null) (named newName branch)
      then respond (NameAlreadyExists branchName nameTarget newName)
      else case toList (named oldName branch) of
        [_] -> mergeBranch' branchName respond success $
                  rename' oldName newName branch
        [] -> respond (UnknownName branchName nameTarget oldName)
        _ -> respond (ConflictedName branchName nameTarget oldName)

unnameAll :: forall i v
          . BranchName
          -> (Output v -> Action i v)
          -> NameTarget
          -> Name
          -> Action i v
          -> Action i v
unnameAll branchName respond nameTarget name success =
  updateBranch respond success branchName $ case nameTarget of
    Names.TermName -> Branch.deleteTermsNamed name
    Names.TypeName -> Branch.deleteTypesNamed name
    Names.PatternName -> Branch.deletePatternsNamed name

addTermName :: BranchName -> (Output v -> Action i v) -> Action i v -> Referent -> Name -> Action i v
addTermName branchName respond success r name =
  updateBranch respond success branchName (Branch.addTermName r name)

addTypeName :: BranchName -> (Output v -> Action i v) -> Action i v -> Reference -> Name -> Action i v
addTypeName branchName respond success r name =
  updateBranch respond success branchName (Branch.addTypeName r name)

addPatternName :: BranchName -> (Output v -> Action i v) -> Action i v -> Reference -> Int -> Name -> Action i v
addPatternName branchName respond success r i name =
  updateBranch respond success branchName (Branch.addPatternName r i name)

mergeBranch :: BranchName -> BranchName -> (Output v -> Action i v) -> Action i v -> Action i v
mergeBranch sourceBranchName targetBranchName respond success =
  withBranch sourceBranchName respond $
    mergeBranch' targetBranchName respond success

mergeBranch' :: BranchName -> (Output v -> Action i v) -> Action i v -> Branch -> Action i v
mergeBranch' targetBranchName respond success b =
  ifM (Free.eval $ MergeBranch targetBranchName b)
      success
      (respond $ UnknownBranch targetBranchName)

updateBranch :: (Output v -> Action i v) -> Action i v -> BranchName -> (Branch -> Branch) -> Action i v
updateBranch respond success branchName f =
  withBranch branchName respond $ mergeBranch' branchName respond success . f
