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
aliasUnconflicted branchName respond nameTarget existingName newName success = do
  -- updateBranch respond success branchName $ case nameTarget of
  --   Names.TermName -> alias Branch.termsNamed Branch.addTermName
  --   Names.TypeName -> alias Branch.typesNamed Branch.addTypeName
  --   Names.PatternName -> alias Branch.patternsNamed (uncurry Branch.addPatternName)
  -- where
  --   alias named alias' branch =
  --     if (not . null) (named newName branch)
  --     then respond (NameAlreadyExists branchName nameTarget newName)
  --     else case toList (named existingName branch) of
  --       [t] -> alias' t newName branch
  --       [] -> respond (UnknownName branchName nameTarget existingName)
  --       _ -> respond (ConflictedName branchName nameTarget existingName)
  --
  --
  --
  --
  -- do
  branch <- Free.eval (LoadBranch branchName)
  case branch of
    Nothing -> respond $ UnknownBranch branchName
    Just branch ->
      let alias :: Foldable f
                => (Name -> Branch -> f a)
                -> (a -> Name -> Branch -> Branch)
                -> Action i v
          alias named add =
            if (not . null) (named newName branch)
            then respond (NameAlreadyExists branchName nameTarget newName)
            else case toList (named existingName branch) of
              [t] ->
                ifM (Free.eval . MergeBranch branchName $
                  add t newName branch)
                    success
                    (respond $ UnknownBranch branchName)
              [] -> respond $
                UnknownName branchName nameTarget existingName
              _ -> respond $
                ConflictedName branchName nameTarget existingName
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

-- renameUnconflicted :: forall i v. BranchName -> (Output v -> Action i v) -> NameTarget -> Name -> Name -> Action i v -> Action i v
-- renameUnconflicted branchName respond nameTarget oldName newName success =
--   case nameTarget of
--     Names.TermName -> rename Branch.termsNamed Branch.renameTerm
--     Names.TypeName -> rename Branch.typesNamed Branch.renameType
--     Names.PatternName -> rename Branch.patternsNamed Branch.renamePattern
--   where
--     rename named rename' branch =
--       if (not . null) (named newName branch)
--       then respond (NameAlreadyExists branchName nameTarget newName)
--       else case toList (named oldName branch) of
--         [_] -> updateBranch' respond success branchName (rename' oldName newName)
--         [] -> respond (UnknownName branchName nameTarget oldName)
--         _ -> respond (ConflictedName branchName nameTarget oldName)

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
addTermName currentBranchName respond success r name =
  updateBranch respond success currentBranchName (Branch.addTermName r name)

addTypeName :: BranchName -> (Output v -> Action i v) -> Action i v -> Reference -> Name -> Action i v
addTypeName = undefined

addPatternName :: BranchName -> (Output v -> Action i v) -> Action i v -> Reference -> Int -> Name -> Action i v
addPatternName = undefined

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

-- updateBranch' :: (Output v -> Action i v) -> Action i v -> BranchName -> ((Branch -> Branch) -> Action i v) -> Action i v
-- updateBranch' respond success branchName f = undefined
