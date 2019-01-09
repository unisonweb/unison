{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}

module Unison.Codebase.Editor.Actions where

import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( ifM )
import           Data.Foldable                  ( foldl', toList )
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import           Data.Tuple                     ( swap )
import qualified Data.Set as Set
import           Data.Set (Set)
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Editor         ( Command(..)
                                                , BranchName
                                                , Input(..)
                                                , Output(..)
                                                , Event(..)
                                                , AddOutput(..)
                                                , DisplayThing(..)
                                                , NameChangeResult(NameChangeResult,changedSuccessfully)
                                                , collateReferences
                                                )
import           Unison.Names                   ( Name
                                                , NameTarget
                                                )
import qualified Unison.Names                  as Names
import           Unison.Parser                  ( Ann )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                (Referent)
import           Unison.Result                  (pattern Result)
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Var                     ( Var )
import qualified Unison.Codebase as Codebase

type Action i v = Free (Command i v) (Either () (LoopState v))

data LoopState v
  = LoopState Branch BranchName (Maybe (UF.TypecheckedUnisonFile' v Ann))

loopState0 :: Branch -> BranchName -> LoopState v
loopState0 b bn = LoopState b bn Nothing

startLoop
  :: Var v => Branch -> BranchName -> Free (Command (Either Event Input) v) ()
startLoop = (loop .) . loopState0

loop
  :: forall v . Var v => LoopState v -> Free (Command (Either Event Input) v) ()
loop s = Free.unfold' go s
 where
  go :: forall v . Var v => LoopState v -> Action (Either Event Input) v
  go s@(LoopState currentBranch currentBranchName uf) = do
    e <- Free.eval Input
    case e of
      Left (UnisonBranchChanged names) -> if Set.member currentBranchName names
        then switchBranch currentBranchName
        else pure (Right s)
      Left (UnisonFileChanged sourceName text) -> do
        Free.eval (Notify $ FileChangeEvent sourceName text)
        Result notes r <- Free.eval (Typecheck currentBranch sourceName text)
        case r of
          -- Parsing failed
          Nothing -> respond
            $ ParseErrors text [ err | Result.Parsing err <- toList notes ]
          Just (errorEnv, r) -> case r of
            -- Typing failed
            Nothing -> respond $ TypeErrors
              text
              errorEnv
              [ err | Result.TypeError err <- toList notes ]
            -- A unison file has changed
            Just unisonFile -> do
              Free.eval (Notify $ Typechecked sourceName errorEnv unisonFile)
              e <- Free.eval
                (Evaluate currentBranch $ UF.discardTypes' unisonFile)
              Free.eval . Notify $ Evaluated (Branch.toNames currentBranch) e
              updateUnisonFile unisonFile
      Right input -> case input of
        SearchByNameI qs -> do
          terms  <- Free.eval $ SearchTerms currentBranch qs
          types  <- Free.eval $ SearchTypes currentBranch qs
          types' <-
            let go (name, ref) = case ref of
                  Reference.DerivedId id ->
                    (name, ref, )
                      .   maybe (MissingThing id) RegularThing
                      <$> Free.eval (LoadType id)
                  _ -> pure (name, ref, BuiltinThing)
            in  traverse go types
          respond $ ListOfDefinitions currentBranch terms types'
        ShowDefinitionI qs -> do
          terms <- Free.eval $ SearchTerms currentBranch qs
          types <- Free.eval $ SearchTypes currentBranch qs
          let terms' = [ (n, r) | (n, r, _) <- terms ]
              (collatedTerms, collatedTypes) =
                collateReferences (snd <$> terms') (snd <$> types)
          loadedTerms <- for collatedTerms $ \r -> case r of
            Reference.DerivedId i ->
              (r, ) . maybe (MissingThing i) RegularThing <$> Free.eval
                (LoadTerm i)
            _ -> pure (r, BuiltinThing)
          loadedTypes <- for collatedTypes $ \r -> case r of
            Reference.DerivedId i ->
              (r, ) . maybe (MissingThing i) RegularThing <$> Free.eval
                (LoadType i)
            _ -> pure (r, BuiltinThing)
          let ppe =
                PPE.fromTermNames [ (r, n) | (n, r, _) <- terms ]
                  `PPE.unionLeft` PPE.fromTypeNames (swap <$> types)
                  `PPE.unionLeft` Branch.prettyPrintEnv [currentBranch]
          respond $ DisplayDefinitions ppe loadedTerms loadedTypes
        RemoveAllTermUpdatesI _t       -> error "todo"
        RemoveAllTypeUpdatesI _t       -> error "todo"
        ChooseUpdateForTermI _old _new -> error "todo"
        ChooseUpdateForTypeI _old _new -> error "todo"
        ListAllUpdatesI                -> error "todo"
        AddTermNameI r name ->
          addTermName currentBranchName respond success r name
        AddTypeNameI r name ->
          addTypeName currentBranchName respond success r name
        RemoveTermNameI r name ->
          updateBranch respond success currentBranchName
            $ Branch.deleteTermName r name
        RemoveTypeNameI r name ->
          updateBranch respond success currentBranchName
            $ Branch.deleteTypeName r name
        ChooseTermForNameI r name ->
          unnameAll currentBranchName respond Names.TermName name
            $ addTermName currentBranchName respond success r name
        ChooseTypeForNameI r name ->
          unnameAll currentBranchName respond Names.TypeName name
            $ addTypeName currentBranchName respond success r name
        AliasUnconflictedI targets existingName newName -> aliasUnconflicted
          currentBranchName
          respond
          respondNewBranch
          targets
          existingName
          newName
        RenameUnconflictedI targets oldName newName -> renameUnconflicted
          currentBranchName
          respond
          respondNewBranch
          targets
          oldName
          newName
        UnnameAllI nameTarget name ->
          unnameAll currentBranchName respond nameTarget name success
        AddI -> case uf of
          Nothing -> respond NoUnisonFile
          Just (UF.TypecheckedUnisonFile' datas effects tlcs _ _) -> do
            let uf' = UF.typecheckedUnisonFile datas effects tlcs
            addo <- Free.eval $ Add currentBranch uf'
            doMerge currentBranchName (updatedBranch addo)
            Free.eval . Notify $ AddOutput addo
            pure . Right $ LoopState (updatedBranch addo) currentBranchName uf
        UpdateI -> case uf of
          Nothing -> respond NoUnisonFile
          Just (UF.TypecheckedUnisonFile' datas effects tlcs _ _) -> do
            let uf' = UF.typecheckedUnisonFile datas effects tlcs
            addo          <- Free.eval $ Add currentBranch uf'
            updatedBranch <- Free.eval
              $ Update currentBranchName (updatedBranch addo) (collisions addo)
            doMerge currentBranchName updatedBranch
            Free.eval $ Notify somethingsomething
            pure . Right $ LoopState updatedBranch currentBranchName uf
        ListBranchesI ->
          Free.eval ListBranches >>= respond . ListOfBranches currentBranchName
        SwitchBranchI branchName       -> switchBranch branchName
        ForkBranchI   targetBranchName -> ifM
          (Free.eval $ ForkBranch currentBranch targetBranchName)
          (outputSuccess *> switchBranch targetBranchName)
          (respond $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranchName -> withBranch inputBranchName respond
          $ \branch -> mergeBranch currentBranchName respond success branch
        QuitI -> quit
       where
        success       = respond $ Success input
        outputSuccess = Free.eval . Notify $ Success input
   where
    doMerge branchName b = do
      updated <- doMerge0 branchName b
      when (not updated) $ do
        Free.eval $ NewBranch branchName
        updated <- doMerge0 branchName b
        when (not updated) (disappearingBranchBomb branchName)
    doMerge0 = (Free.eval .) . MergeBranch
    disappearingBranchBomb branchName =
      error
        $  "The branch named "
        <> Text.unpack branchName
        <> " disappeared from storage. "
        <> "I tried to put it back, but couldn't. Everybody panic!"
    respond :: Output v -> Action i v
    respond output = Free.eval (Notify output) >> pure (Right s)
    respondNewBranch :: Output v -> Branch -> Action i v
    respondNewBranch output newBranch =
      respond output >> pure (Right (LoopState newBranch currentBranchName uf))

    switchBranch branchName = do
      branch <- Free.eval $ LoadBranch branchName
      case branch of
        Nothing -> do
          let newBranch = Codebase.builtinBranch
          Free.eval $ SwitchBranch newBranch branchName
          _ <- Free.eval $ NewBranch branchName
          pure . Right $ LoopState newBranch branchName uf
        Just branch -> do
          Free.eval $ SwitchBranch branch branchName
          pure . Right $ LoopState branch branchName uf
    updateUnisonFile
      :: forall f v
       . Applicative f
      => UF.TypecheckedUnisonFile' v Ann
      -> f (Either () (LoopState v))
    updateUnisonFile =
      pure . Right . LoopState currentBranch currentBranchName . Just
    quit = pure $ Left ()

withBranch :: BranchName
           -> (Output v -> Action i v)
           -> (Branch -> Action i v)
           -> Action i v
withBranch branchName respond f = do
  branch <- Free.eval $ LoadBranch branchName
  case branch of
    Nothing     -> respond $ UnknownBranch branchName
    Just branch -> f branch

aliasUnconflicted
  :: forall i v
   . BranchName
  -> (Output v -> Action i v)
  -> (Output v -> Branch -> Action i v)
  -> Set NameTarget
  -> Name
  -> Name
  -> Action i v
aliasUnconflicted branchName respond respondNewBranch
                   nameTargets oldName newName =
  withBranch branchName respond $ \branch -> let
    (branch', result) = foldl' go (branch, mempty) nameTargets where
      go (branch, result) nameTarget = (result <>) <$> case nameTarget of
        Names.TermName ->
          alias nameTarget Branch.termsNamed Branch.addTermName branch
        Names.TypeName ->
          alias nameTarget Branch.typesNamed Branch.addTypeName branch
    -- the RenameOutput action and setting the loop state
    in
      if (not . null . changedSuccessfully) result then
        let success = respondNewBranch (AliasOutput oldName newName result) branch'
        in mergeBranch branchName respond success branch'
      else respond $ AliasOutput oldName newName result

 where
  alias
    :: Foldable f
    => NameTarget
    -> (Name -> Branch -> f a)
    -> (a -> Name -> Branch -> Branch)
    -> Branch
    -> (Branch, NameChangeResult)
  alias nameTarget named alias' branch =
    let
      oldMatches = toList . named oldName $ branch
      oldNameMatchCount = length oldMatches
      newNameExists = (not . null) (named newName branch)
    in case oldMatches of
         [oldMatch] | not newNameExists ->
            (alias' oldMatch newName branch,
          NameChangeResult mempty mempty (Set.singleton nameTarget))
         _ -> (branch, NameChangeResult a b mempty) where
           a = if oldNameMatchCount > 1 then Set.singleton nameTarget
               else mempty
           b = if newNameExists then Set.singleton nameTarget
               else mempty

renameUnconflicted
  :: forall i v
   . BranchName
  -> (Output v -> Action i v)
  -> (Output v -> Branch -> Action i v)
  -> Set NameTarget
  -> Name
  -> Name
  -> Action i v
renameUnconflicted branchName respond respondNewBranch
                   nameTargets oldName newName =
  withBranch branchName respond $ \branch -> let
    (branch', result) = foldl' go (branch, mempty) nameTargets where
      go (branch, result) nameTarget = (result <>) <$> case nameTarget of
        Names.TermName ->
          rename nameTarget Branch.termsNamed Branch.renameTerm branch
        Names.TypeName ->
          rename nameTarget Branch.typesNamed Branch.renameType branch
    -- the RenameOutput action and setting the loop state
    in
      if (not . null . changedSuccessfully) result then
        let success = respondNewBranch (RenameOutput oldName newName result) branch'
        in mergeBranch branchName respond success branch'
      else respond $ RenameOutput oldName newName result

 where
  rename
    :: Foldable f
    => NameTarget
    -> (Name -> Branch -> f a)
    -> (Name -> Name -> Branch -> Branch)
    -> Branch
    -> (Branch, NameChangeResult)
  rename nameTarget named rename' branch =
    let
      oldNameMatchCount = length . named oldName $ branch
      newNameExists = (not . null) (named newName branch)
    in if (oldNameMatchCount == 1 && not newNameExists)
       then
        (rename' oldName newName branch,
          NameChangeResult mempty mempty (Set.singleton nameTarget))
       else
        (branch, NameChangeResult
          (if oldNameMatchCount > 1 then Set.singleton nameTarget else mempty)
          (if newNameExists then Set.singleton nameTarget else mempty)
          mempty)

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
    -- Names.PatternName -> Branch.deletePatternsNamed name

addTermName
  :: BranchName
  -> (Output v -> Action i v)
  -> Action i v
  -> Referent
  -> Name
  -> Action i v
addTermName branchName respond success r name =
  updateBranch respond success branchName (Branch.addTermName r name)

addTypeName
  :: BranchName
  -> (Output v -> Action i v)
  -> Action i v
  -> Reference
  -> Name
  -> Action i v
addTypeName branchName respond success r name =
  updateBranch respond success branchName (Branch.addTypeName r name)

addPatternName
  :: BranchName
  -> (Output v -> Action i v)
  -> Action i v
  -> Reference
  -> Int
  -> Name
  -> Action i v
addPatternName branchName respond success r i name =
  updateBranch respond success branchName (Branch.addPatternName r i name)

mergeBranch
  :: BranchName
  -> (Output v -> Action i v)
  -> Action i v
  -> Branch
  -> Action i v
mergeBranch targetBranchName respond success b = ifM
  (Free.eval $ MergeBranch targetBranchName b)
  success
  (respond $ UnknownBranch targetBranchName)

updateBranch
  :: (Output v -> Action i v)
  -> Action i v
  -> BranchName
  -> (Branch -> Branch)
  -> Action i v
updateBranch respond success branchName f =
  withBranch branchName respond $ mergeBranch branchName respond success . f
