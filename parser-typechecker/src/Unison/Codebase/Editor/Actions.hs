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
    Nothing     -> respond $ UnknownBranch branchName
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
        Names.TermName    -> rename Branch.termsNamed Branch.renameTerm
        Names.TypeName    -> rename Branch.typesNamed Branch.renameType
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
