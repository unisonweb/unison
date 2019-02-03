{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Unison.Codebase.Editor.Actions where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad                  ( when, unless )
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.State            ( StateT
                                                , evalStateT
                                                , get
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import           Data.Foldable                  ( foldl'
                                                , toList
                                                )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map as Map
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import           Data.Tuple                     ( swap )
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Unison.ABT as ABT
import           Unison.Codebase.Branch         ( Branch
                                                , Branch0
                                                )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Editor         ( Command(..)
                                                , BranchName
                                                , Input(..)
                                                , Output(..)
                                                , Event(..)
                                                , SlurpResult(..)
                                                , DisplayThing(..)
                                                , NameChangeResult
                                                  ( NameChangeResult
                                                  , changedSuccessfully
                                                  )
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
import qualified Unison.Referent               as Referent
import           Unison.Result                  (pattern Result)
import qualified Unison.Term as Term
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Var                     ( Var )
import qualified Unison.Codebase as Codebase

type Action i v = MaybeT (StateT (LoopState v) (Free (Command i v)))

data LoopState v
  = LoopState
      { _currentBranch :: Branch
      , _currentBranchName :: BranchName
      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , _latestFile :: Maybe (FilePath, SkipNextUpdate)
      , _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile' v Ann) }

type SkipNextUpdate = Bool

makeLenses ''LoopState

loopState0 :: Branch -> BranchName -> LoopState v
loopState0 b bn = LoopState b bn Nothing Nothing

startLoop
  :: Var v => Branch -> BranchName -> Free (Command (Either Event Input) v) ()
startLoop = (loop .) . loopState0

loop
  :: forall v . Var v => LoopState v -> Free (Command (Either Event Input) v) ()
loop s = Free.unfold' (evalStateT (maybe (Left ()) Right <$> runMaybeT (go *> get))) s
 where
  go :: forall v . Var v => Action (Either Event Input) v ()
  go = do
    uf                 <- use latestTypecheckedFile
    currentBranchName' <- use currentBranchName
    latestFile'        <- use latestFile
    currentBranch'     <- use currentBranch
    e                  <- eval Input
    case e of
      Left (UnisonBranchChanged names) ->
        when (Set.member currentBranchName' names)
          $ switchBranch currentBranchName'
      Left (UnisonFileChanged sourceName text) ->
        -- We skip this update if it was programmatically generated
        if fromMaybe False . fmap snd $ latestFile'
          then modifying latestFile $ (fmap (const False) <$>)
          else do
            eval (Notify $ FileChangeEvent sourceName text)
            Result notes r <- eval
              (Typecheck (view currentBranch s) sourceName text)
            case r of
              -- Parsing failed
              Nothing ->
                respond $ ParseErrors
                  text
                  [ err | Result.Parsing err <- toList notes ]
              Just (errorEnv, r) -> case r of
                -- Typing failed
                Nothing ->
                  respond $ TypeErrors
                    text
                    errorEnv
                    [ err | Result.TypeError err <- toList notes ]
                -- A unison file has changed
                Just unisonFile -> do
                  eval (Notify $ Typechecked sourceName errorEnv unisonFile)
                  e <-
                    eval
                      ( Evaluate (view currentBranch s)
                      $ UF.discardTypes' unisonFile
                      )
                  eval . Notify $ Evaluated (Branch.toNames currentBranch') e
                  latestFile .= Just (Text.unpack sourceName, False)
                  latestTypecheckedFile .= Just unisonFile
      Right input -> case input of
        SearchByNameI qs -> do
          terms  <- eval $ SearchTerms currentBranch' qs
          types  <- eval $ SearchTypes currentBranch' qs
          types' <-
            let
              go (name, ref) = case ref of
                Reference.DerivedId id ->
                  (name, ref, ) . maybe (MissingThing id) RegularThing <$> eval
                    (LoadType id)
                _ -> pure (name, ref, BuiltinThing)
            in  traverse go types
          respond $ ListOfDefinitions currentBranch' terms types'
        ShowDefinitionI outputLoc qs -> do
          terms <- eval $ SearchTerms currentBranch' qs
          types <- eval $ SearchTypes currentBranch' qs
          let terms' = [ (n, r) | (n, r, _) <- terms ]
              termTypes =
                Map.fromList [ (r, t) | (_, Referent.Ref r, Just t) <- terms ]
              (collatedTerms, collatedTypes) =
                collateReferences (snd <$> terms') (snd <$> types)
          loadedTerms <- for collatedTerms $ \r -> case r of
            Reference.DerivedId i -> do
              tm <- eval (LoadTerm i)
              -- We add a type annotation to the term using if it doesn't
              -- already have one that the user provided
              pure . (r, ) $ case liftA2 (,) tm (Map.lookup r termTypes) of
                Nothing        -> MissingThing i
                Just (tm, typ) -> case tm of
                  Term.Ann' _ _ -> RegularThing tm
                  _ -> RegularThing (Term.ann (ABT.annotation tm) tm typ)
            _ -> pure (r, BuiltinThing)
          loadedTypes <- for collatedTypes $ \r -> case r of
            Reference.DerivedId i ->
              (r, ) . maybe (MissingThing i) RegularThing <$> eval (LoadType i)
            _ -> pure (r, BuiltinThing)
          -- makes sure that the user search terms get used as the names
          -- in the pretty-printer
          let
            ppe =
              PPE.fromTermNames [ (r, n) | (n, r, _) <- terms ]
                `PPE.unionLeft` PPE.fromTypeNames (swap <$> types)
                `PPE.unionLeft` Branch.prettyPrintEnv
                                  [Branch.head $ currentBranch']
            loc = case outputLoc of
              Editor.ConsoleLocation    -> Nothing
              Editor.FileLocation path  -> Just path
              Editor.LatestFileLocation -> fmap fst latestFile'
                <|> Just (Text.unpack currentBranchName' <> ".u")
          do
            eval . Notify $ DisplayDefinitions loc ppe loadedTerms loadedTypes
            -- We set latestFile to be programmatically generated, if we
            -- are viewing these definitions to a file - this will skip the
            -- next update for that file (which will happen immediately)
            latestFile .= ((, True) <$> loc)
        RemoveAllTermUpdatesI _t       -> error "todo"
        RemoveAllTypeUpdatesI _t       -> error "todo"
        ChooseUpdateForTermI _old _new -> error "todo"
        ChooseUpdateForTypeI _old _new -> error "todo"
        ListAllUpdatesI                -> error "todo"
        AddTermNameI r name -> addTermName currentBranchName' success r name
        AddTypeNameI r name -> addTypeName currentBranchName' success r name
        RemoveTermNameI r name ->
          updateBranch success currentBranchName'
            . Branch.modify
            $ Branch.deleteTermName r name
        RemoveTypeNameI r name ->
          updateBranch success currentBranchName'
            . Branch.modify
            $ Branch.deleteTypeName r name
        ChooseTermForNameI r name ->
          unnameAll currentBranchName' Names.TermName name
            $ addTermName currentBranchName' success r name
        ChooseTypeForNameI r name ->
          unnameAll currentBranchName' Names.TypeName name
            $ addTypeName currentBranchName' success r name
        AliasUnconflictedI targets existingName newName ->
          aliasUnconflicted currentBranchName' targets existingName newName
        RenameUnconflictedI targets oldName newName ->
          renameUnconflicted currentBranchName' targets oldName newName
        UnnameAllI nameTarget name ->
          unnameAll currentBranchName' nameTarget name success
        SlurpFileI allowUpdates -> case uf of
          Nothing -> respond NoUnisonFile
          Just (UF.TypecheckedUnisonFile' datas effects tlcs _ _) -> do
            let uf'              = UF.typecheckedUnisonFile datas effects tlcs
                collisionHandler = if allowUpdates
                  then Editor.updateCollisionHandler
                  else Editor.addCollisionHandler
            updateo <- eval $ SlurpFile collisionHandler currentBranch' uf'
            let branch' = updatedBranch updateo
            -- Don't bother doing anything if the branch is unchanged by the slurping
            when (branch' /= currentBranch') $ do
              -- This order is important - we tell the app state about the
              -- branch change before doing the merge, so it knows to ignore
              -- the file system event that is triggered by `doMerge`
              eval $ SwitchBranch branch' currentBranchName'
              doMerge currentBranchName' branch'
            eval . Notify $ SlurpOutput updateo
            currentBranch .= branch'
        ListBranchesI ->
          eval ListBranches >>= respond . ListOfBranches currentBranchName'
        SwitchBranchI branchName       -> switchBranch branchName
        ForkBranchI   targetBranchName -> ifM
          (eval $ ForkBranch currentBranch' targetBranchName)
          (outputSuccess *> switchBranch targetBranchName)
          (respond $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranchName ->
          withBranch inputBranchName $ \branch -> do
            let merged0 = branch <> currentBranch'
            merged <- eval $ Propagate merged0
            ok     <- eval $ MergeBranch currentBranchName' merged
            if ok
              then do
                todo <- eval $ Todo merged
                _    <- success
                _    <- respond $ TodoOutput merged todo
                currentBranch .= merged
              else respond (UnknownBranch inputBranchName)
        TodoI ->
          eval (Todo currentBranch') >>= respond . TodoOutput currentBranch'
        PropagateI -> do
          b <- eval . Propagate $ currentBranch'
          _ <- eval $ MergeBranch currentBranchName' b
          _ <- success
          currentBranch .= b
        QuitI -> quit
       where
        success       = respond $ Success input
        outputSuccess = eval . Notify $ Success input
   where
    doMerge branchName b = do
      updated <- doMerge0 branchName b
      when (not updated) $ do
        _       <- eval $ NewBranch branchName
        updated <- doMerge0 branchName b
        when (not updated) (disappearingBranchBomb branchName)
    doMerge0 = (eval .) . MergeBranch
    disappearingBranchBomb branchName =
      error
        $  "The branch named "
        <> Text.unpack branchName
        <> " disappeared from storage. "
        <> "I tried to put it back, but couldn't. Everybody panic!"
    switchBranch branchName = do
      branch <- eval $ LoadBranch branchName
      case branch of
        Nothing -> do
          let newBranch = Codebase.builtinBranch
          eval $ SwitchBranch newBranch branchName
          _ <- eval $ NewBranch branchName
          currentBranch .= newBranch
          currentBranchName .= branchName
        Just branch -> do
          eval $ SwitchBranch branch branchName
          currentBranch .= branch
          currentBranchName .= branchName
    quit = MaybeT $ pure Nothing

eval :: Command i v a -> Action i v a
eval = lift . lift . Free.eval

loadBranch :: BranchName -> Action i v (Maybe Branch)
loadBranch = eval . LoadBranch

withBranch :: BranchName -> (Branch -> Action i v ()) -> Action i v ()
withBranch b f = loadBranch b >>= maybe (respond $ UnknownBranch b) f

respond :: Output v -> Action i v ()
respond output = eval $ Notify output

aliasUnconflicted
  :: forall i v . BranchName -> Set NameTarget -> Name -> Name -> Action i v ()
aliasUnconflicted branchName nameTargets oldName newName = do
  withBranch branchName $ \branch ->
    let (branch', result) = foldl' go (branch, mempty) nameTargets
         where
        go (branch, result) nameTarget = (result <>) <$> case nameTarget of
          Names.TermName ->
            alias nameTarget Branch.termsNamed Branch.addTermName branch
          Names.TypeName ->
            alias nameTarget Branch.typesNamed Branch.addTypeName branch
                            -- the RenameOutput action and setting the loop state
    in  do
          respond $ AliasOutput oldName newName result
          unless (null $ changedSuccessfully result) $ currentBranch .= branch'
 where
  alias
    :: Foldable f
    => NameTarget
    -> (Name -> Branch0 -> f a)
    -> (a -> Name -> Branch0 -> Branch0)
    -> Branch
    -> (Branch, NameChangeResult)
  alias nameTarget named alias' branch =
    let oldMatches        = toList . named oldName $ Branch.head branch
        oldNameMatchCount = length oldMatches
        newNameExists     = (not . null) (named newName $ Branch.head branch)
    in  case oldMatches of
          [oldMatch] | not newNameExists ->
            ( Branch.modify (alias' oldMatch newName) branch
            , NameChangeResult mempty mempty (Set.singleton nameTarget)
            )
          _ -> (branch, NameChangeResult a b mempty)
           where
            a =
              if oldNameMatchCount > 1 then Set.singleton nameTarget else mempty
            b = if newNameExists then Set.singleton nameTarget else mempty

renameUnconflicted
  :: forall i v . BranchName -> Set NameTarget -> Name -> Name -> Action i v ()
renameUnconflicted branchName nameTargets oldName newName = do
  withBranch branchName $ \branch ->
    let (branch', result) = foldl' go (branch, mempty) nameTargets
         where
          go (branch, result) nameTarget = (result <>) <$> case nameTarget of
            Names.TermName ->
              rename nameTarget Branch.termsNamed Branch.renameTerm branch
            Names.TypeName ->
              rename nameTarget Branch.typesNamed Branch.renameType branch
    in  do
          respond $ RenameOutput oldName newName result
          unless (null $ changedSuccessfully result) $ currentBranch .= branch'
 where
  rename
    :: Foldable f
    => NameTarget
    -> (Name -> Branch0 -> f a)
    -> (Name -> Name -> Branch0 -> Branch0)
    -> Branch
    -> (Branch, NameChangeResult)
  rename nameTarget named rename' branch =
    let oldNameMatchCount = length . named oldName $ Branch.head branch
        newNameExists     = (not . null) (named newName $ Branch.head branch)
    in  if (oldNameMatchCount == 1 && not newNameExists)
          then
            ( Branch.modify (rename' oldName newName) branch
            , NameChangeResult mempty mempty (Set.singleton nameTarget)
            )
          else
            ( branch
            , NameChangeResult
              (if oldNameMatchCount > 1
                then Set.singleton nameTarget
                else mempty
              )
              (if newNameExists then Set.singleton nameTarget else mempty)
              mempty
            )

unnameAll :: BranchName -> NameTarget -> Name -> Action i v () -> Action i v ()
unnameAll branchName nameTarget name success =
  updateBranch success branchName $ case nameTarget of
    Names.TermName -> Branch.modify (Branch.deleteTermsNamed name)
    Names.TypeName -> Branch.modify (Branch.deleteTypesNamed name)

addTermName :: BranchName -> Action i v () -> Referent -> Name -> Action i v ()
addTermName branchName success r name =
  updateBranch success branchName . Branch.modify $ Branch.addTermName r name

addTypeName :: BranchName -> Action i v () -> Reference -> Name -> Action i v ()
addTypeName branchName success r name =
  updateBranch success branchName . Branch.modify $ Branch.addTypeName r name

merging :: BranchName -> Branch -> Action i v () -> Action i v ()
merging targetBranchName b success =
  ifM (eval $ MergeBranch targetBranchName b) success . respond $ UnknownBranch
    targetBranchName

updateBranch
  :: Action i v () -> BranchName -> (Branch -> Branch) -> Action i v ()
updateBranch success branchName f =
  withBranch branchName $ \b -> merging branchName (f b) success

