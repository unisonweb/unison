{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}

module Unison.Codebase.Editor.Actions where

import Control.Applicative
import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( ifM )
import           Data.Foldable                  ( foldl', toList )
import Data.Maybe (fromMaybe)
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import           Data.Tuple                     ( swap )
import qualified Data.Set as Set
import           Data.Set (Set)
import           Unison.Codebase.Branch         ( Branch, Branch0 )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Editor         ( Command(..)
                                                , BranchName
                                                , Input(..)
                                                , Output(..)
                                                , Event(..)
                                                , SlurpResult(..)
                                                , DisplayThing(..)
                                                , NameChangeResult(NameChangeResult,changedSuccessfully)
                                                , collateReferences
                                                )
import qualified Unison.Codebase.Editor         as Editor
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
  = LoopState
      { currentBranch :: Branch
      , currentBranchName :: BranchName
      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , latestFile :: Maybe (FilePath, SkipNextUpdate)
      , latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile' v Ann) }

type SkipNextUpdate = Bool

loopState0 :: Branch -> BranchName -> LoopState v
loopState0 b bn = LoopState b bn Nothing Nothing

startLoop
  :: Var v => Branch -> BranchName -> Free (Command (Either Event Input) v) ()
startLoop = (loop .) . loopState0

loop
  :: forall v . Var v => LoopState v -> Free (Command (Either Event Input) v) ()
loop s = Free.unfold' go s
 where
  go :: forall v . Var v => LoopState v -> Action (Either Event Input) v
  go s = do
    let uf = latestTypecheckedFile s
    e <- Free.eval Input
    case e of
      Left (UnisonBranchChanged names) -> if Set.member (currentBranchName s) names
        then switchBranch (currentBranchName s)
        else pure (Right s)
      Left (UnisonFileChanged sourceName text) ->
        if fromMaybe False . fmap snd $ latestFile s
        -- We skip this update if it was programmatically generated
        then pure . Right $ s { latestFile = fmap (const False) <$> latestFile s }
        else do
          Free.eval (Notify $ FileChangeEvent sourceName text)
          Result notes r <- Free.eval (Typecheck (currentBranch s) sourceName text)
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
                  (Evaluate (currentBranch s) $ UF.discardTypes' unisonFile)
                Free.eval . Notify $ Evaluated (Branch.toNames $ currentBranch s) e
                pure . Right $ s { latestFile = Just (Text.unpack sourceName, False)
                                 , latestTypecheckedFile = Just unisonFile }
      Right input -> case input of
        SearchByNameI qs -> do
          terms  <- Free.eval $ SearchTerms (currentBranch s) qs
          types  <- Free.eval $ SearchTypes (currentBranch s) qs
          types' <-
            let go (name, ref) = case ref of
                  Reference.DerivedId id ->
                    (name, ref, )
                      .   maybe (MissingThing id) RegularThing
                      <$> Free.eval (LoadType id)
                  _ -> pure (name, ref, BuiltinThing)
            in  traverse go types
          respond $ ListOfDefinitions (currentBranch s) terms types'
        ShowDefinitionI outputLoc qs -> do
          terms <- Free.eval $ SearchTerms (currentBranch s) qs
          types <- Free.eval $ SearchTypes (currentBranch s) qs
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
          -- makes sure that the user search terms get used as the names
          -- in the pretty-printer
          let ppe =
                PPE.fromTermNames [ (r, n) | (n, r, _) <- terms ]
                  `PPE.unionLeft` PPE.fromTypeNames (swap <$> types)
                  `PPE.unionLeft` Branch.prettyPrintEnv [Branch.head $ currentBranch s]
              loc = case outputLoc of
                Editor.ConsoleLocation -> Nothing
                Editor.FileLocation path -> Just path
                Editor.LatestFileLocation ->
                  fmap fst (latestFile s) <|> Just (Text.unpack (currentBranchName s) <> ".u")
          do
            Free.eval . Notify $ DisplayDefinitions loc ppe loadedTerms loadedTypes
            -- We set latestFile to be programmatically generated, if we
            -- are viewing these definitions to a file - this will skip the
            -- next update for that file (which will happen immediately)
            pure $ Right (s { latestFile = (,True) <$> loc })
        RemoveAllTermUpdatesI _t       -> error "todo"
        RemoveAllTypeUpdatesI _t       -> error "todo"
        ChooseUpdateForTermI _old _new -> error "todo"
        ChooseUpdateForTypeI _old _new -> error "todo"
        ListAllUpdatesI                -> error "todo"
        AddTermNameI r name ->
          addTermName (currentBranchName s) respond success r name
        AddTypeNameI r name ->
          addTypeName (currentBranchName s) respond success r name
        RemoveTermNameI r name ->
          updateBranch respond success (currentBranchName s)
            $ Branch.modify (Branch.deleteTermName r name)
        RemoveTypeNameI r name ->
          updateBranch respond success (currentBranchName s)
            $ Branch.modify (Branch.deleteTypeName r name)
        ChooseTermForNameI r name ->
          unnameAll (currentBranchName s) respond Names.TermName name
            $ addTermName (currentBranchName s) respond success r name
        ChooseTypeForNameI r name ->
          unnameAll (currentBranchName s) respond Names.TypeName name
            $ addTypeName (currentBranchName s) respond success r name
        AliasUnconflictedI targets existingName newName -> aliasUnconflicted
          (currentBranchName s)
          respond
          respondNewBranch
          targets
          existingName
          newName
        RenameUnconflictedI targets oldName newName -> renameUnconflicted
          (currentBranchName s)
          respond
          respondNewBranch
          targets
          oldName
          newName
        UnnameAllI nameTarget name ->
          unnameAll (currentBranchName s) respond nameTarget name success
        SlurpFileI allowUpdates -> case uf of
          Nothing -> respond NoUnisonFile
          Just (UF.TypecheckedUnisonFile' datas effects tlcs _ _) -> do
            let uf' = UF.typecheckedUnisonFile datas effects tlcs
                collisionHandler =
                  if allowUpdates then Editor.updateCollisionHandler
                  else Editor.addCollisionHandler
            updateo <- Free.eval $ SlurpFile collisionHandler (currentBranch s) uf'
            let branch' = updatedBranch updateo
            -- Don't bother doing anything if the branch is unchanged by the slurping
            when (branch' /= currentBranch s) $ do
              -- This order is important - we tell the app state about the
              -- branch change before doing the merge, so it knows to ignore
              -- the file system event that is triggered by `doMerge`
              Free.eval $ SwitchBranch branch' (currentBranchName s)
              doMerge (currentBranchName s) branch'
            Free.eval . Notify $ SlurpOutput updateo
            pure . Right $ s { currentBranch = branch' }
        ListBranchesI ->
          Free.eval ListBranches >>= respond . ListOfBranches (currentBranchName s)
        SwitchBranchI branchName       -> switchBranch branchName
        ForkBranchI   targetBranchName -> ifM
          (Free.eval $ ForkBranch (currentBranch s) targetBranchName)
          (outputSuccess *> switchBranch targetBranchName)
          (respond $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranchName -> withBranch inputBranchName respond
          $ \branch -> mergeBranch (currentBranchName s) respond success branch
        TodoI ->
          Free.eval (Todo (currentBranch s)) >>= respond . TodoOutput (currentBranch s)
        QuitI -> quit
       where
        success       = respond $ Success input
        outputSuccess = Free.eval . Notify $ Success input
   where
    doMerge branchName b = do
      updated <- doMerge0 branchName b
      when (not updated) $ do
        _ <- Free.eval $ NewBranch branchName
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
      respond output >> pure (Right $ s { currentBranch = newBranch })

    switchBranch branchName = do
      branch <- Free.eval $ LoadBranch branchName
      case branch of
        Nothing -> do
          let newBranch = Codebase.builtinBranch
          Free.eval $ SwitchBranch newBranch branchName
          _ <- Free.eval $ NewBranch branchName
          pure . Right $ s { currentBranch = newBranch, currentBranchName = branchName }
        Just branch -> do
          Free.eval $ SwitchBranch branch branchName
          pure . Right $ s { currentBranch = branch, currentBranchName = branchName }
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
    -> (Name -> Branch0 -> f a)
    -> (a -> Name -> Branch0 -> Branch0)
    -> Branch
    -> (Branch, NameChangeResult)
  alias nameTarget named alias' branch =
    let
      oldMatches = toList . named oldName $ Branch.head branch
      oldNameMatchCount = length oldMatches
      newNameExists = (not . null) (named newName $ Branch.head branch)
    in case oldMatches of
         [oldMatch] | not newNameExists ->
            (Branch.modify (alias' oldMatch newName) branch,
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
    -> (Name -> Branch0 -> f a)
    -> (Name -> Name -> Branch0 -> Branch0)
    -> Branch
    -> (Branch, NameChangeResult)
  rename nameTarget named rename' branch =
    let
      oldNameMatchCount = length . named oldName $ Branch.head branch
      newNameExists = (not . null) (named newName $ Branch.head branch)
    in if (oldNameMatchCount == 1 && not newNameExists)
       then
        (Branch.modify (rename' oldName newName) branch,
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
    Names.TermName -> Branch.modify (Branch.deleteTermsNamed name)
    Names.TypeName -> Branch.modify (Branch.deleteTypesNamed name)

addTermName
  :: BranchName
  -> (Output v -> Action i v)
  -> Action i v
  -> Referent
  -> Name
  -> Action i v
addTermName branchName respond success r name =
  updateBranch respond success branchName (Branch.modify (Branch.addTermName r name))

addTypeName
  :: BranchName
  -> (Output v -> Action i v)
  -> Action i v
  -> Reference
  -> Name
  -> Action i v
addTypeName branchName respond success r name =
  updateBranch respond success branchName (Branch.modify (Branch.addTypeName r name))

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
