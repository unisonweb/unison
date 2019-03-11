{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
module Unison.Codebase.Editor.Actions where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.State            ( StateT
                                                , get
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Maybe      ( MaybeT(..))
import           Data.Foldable                  ( foldl'
                                                , toList
                                                )
import           Data.Maybe                     ( catMaybes, fromMaybe )
import qualified Data.Map as Map
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
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
                                                  ( NameChangeResult)
                                                , collateReferences
                                                )
import qualified Unison.Codebase.Editor        as Editor
import qualified Unison.DataDeclaration        as DD
import qualified Unison.HashQualified          as HQ
import           Unison.Name                    ( Name )
import           Unison.Names                   ( NameTarget )
import qualified Unison.Names                  as Names
import           Unison.Parser                  ( Ann(..) )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import           Unison.Result                  (pattern Result)
import qualified Unison.Term as Term
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Unison.Codebase as Codebase

type Action i v a = MaybeT (StateT (LoopState v) (Free (Command i v))) a

data LoopState v
  = LoopState
      { _currentBranch :: Branch
      , _currentBranchName :: BranchName
      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , _latestFile :: Maybe (FilePath, SkipNextUpdate)
      , _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile v Ann) }

type SkipNextUpdate = Bool

makeLenses ''LoopState

loopState0 :: Branch -> BranchName -> LoopState v
loopState0 b bn = LoopState b bn Nothing Nothing

loop :: forall v . Var v => Action (Either Event Input) v ()
loop = do
    s <- get
    uf                 <- use latestTypecheckedFile
    currentBranchName' <- use currentBranchName
    latestFile'        <- use latestFile
    currentBranch'     <- use currentBranch
    e                  <- eval Input
    let withFile sourceName text k = do
          Result notes r <- eval
            (Typecheck (view currentBranch s) sourceName text)
          case r of
            -- Parsing failed
            Nothing ->
              respond $ ParseErrors
                text
                [ err | Result.Parsing err <- toList notes ]
            Just (errorEnv, r) ->
              let h = respond $ TypeErrors
                        text
                        errorEnv
                        [ err | Result.TypeError err <- toList notes ]
               in maybe h (k errorEnv) r
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
            withFile sourceName text $ \errorEnv unisonFile -> do
              eval (Notify $ Typechecked sourceName errorEnv unisonFile)
              (bindings, e) <-
                eval . Evaluate (view currentBranch s) $ UF.discardTypes unisonFile
              let e' = Map.map go e
                  go (ann, _hash, _uneval, eval, isHit) = (ann, eval, isHit)
              -- todo: this would be a good spot to update the cache
              -- with all the (hash, eval) pairs, even if it's just an
              -- in-memory cache
              eval . Notify $ Evaluated text
                (errorEnv <> Branch.prettyPrintEnv (Branch.head currentBranch'))
                bindings
                e'
              latestFile .= Just (Text.unpack sourceName, False)
              latestTypecheckedFile .= Just unisonFile
      Right input -> case input of
        -- ls with no arguments
        SearchByNameI [] ->
          (eval $ ListBranch currentBranch') >>=
            respond . ListOfDefinitions currentBranch'
        -- ls with arguments
        SearchByNameI (fmap HQ.fromString -> qs) ->
          (eval $ SearchBranch currentBranch' qs)
            >>= respond . ListOfDefinitions currentBranch'
        ShowDefinitionI outputLoc (fmap HQ.fromString -> qs) -> do
          results <- eval $ SearchBranch currentBranch' qs
          let termTypes :: Map.Map Reference (Editor.Type v Ann)
              termTypes = Map.fromList
                [ (r, t)
                | Editor.Tm _ (Just t) (Referent.Ref r) _ <- results ]
              termReferent (Editor.Tm _ _ r _) = Just r
              termReferent _ = Nothing
              typeReference (Editor.Tp _ _ r _) = Just r
              typeReference _ = Nothing
              (collatedTerms, collatedTypes) =
                collateReferences (catMaybes . map termReferent $ results)
                                  (catMaybes . map typeReference $ results)
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
            Reference.Builtin _ -> pure (r, BuiltinThing)
          loadedTypes <- for collatedTypes $ \r -> case r of
            Reference.DerivedId i ->
              (r, ) . maybe (MissingThing i) RegularThing <$> eval (LoadType i)
            Reference.Builtin _ -> pure (r, BuiltinThing)
          -- makes sure that the user search terms get used as the names
          -- in the pretty-printer
          let
            ppe =
              PPE.fromTermNames [ (r, n) | Editor.Tm n _ r _ <- results ] <>
              PPE.fromTypeNames [ (r, n) | Editor.Tp n _ r _ <- results ] <>
              Branch.prettyPrintEnv (Branch.head currentBranch')
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
        AddTermNameI r name -> modifyCurrentBranch0 $
          Branch.addTermName r name
        AddTypeNameI r name -> modifyCurrentBranch0 $
          Branch.addTypeName r name
        RemoveTermNameI r name -> modifyCurrentBranch0 $
          Branch.deleteTermName r name
        RemoveTypeNameI r name -> modifyCurrentBranch0 $
          Branch.deleteTypeName r name
        ChooseTermForNameI r name -> modifyCurrentBranch0 $
          Branch.addTermName r name . Branch.unnameAll Names.TermName name
        ChooseTypeForNameI r name -> modifyCurrentBranch0 $
          Branch.addTypeName r name . Branch.unnameAll Names.TypeName name
        AliasUnconflictedI targets existingName newName ->
          aliasUnconflicted targets existingName newName
        RenameUnconflictedI targets oldName newName ->
          renameUnconflicted targets oldName newName
        UnnameAllI nameTarget name -> modifyCurrentBranch0 $
          Branch.unnameAll nameTarget name
        SlurpFileI allowUpdates -> case uf of
          Nothing -> respond NoUnisonFile
          Just uf' -> do
            let collisionHandler = if allowUpdates
                  then Editor.updateCollisionHandler
                  else Editor.addCollisionHandler
            updateo <- eval $ SlurpFile collisionHandler currentBranch' uf'
            let branch' = updatedBranch updateo
            -- Don't bother doing anything if the branch is unchanged by the slurping
            when (branch' /= currentBranch') $ doMerge currentBranchName' branch'
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
            ok     <- eval $ SyncBranch currentBranchName' merged
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
          _ <- eval $ SyncBranch currentBranchName' b
          _ <- success
          currentBranch .= b
        ExecuteI input ->
          let augment uf = uf { UF.watches = [(Var.nameds "__main__", term)] }
              term =
                Term.apps' (Term.var External (Var.named "_main_"))
                           [DD.unitTerm External]
          in withFile "execute command"
                   ("_main_ : '{IO} ()\n_main_ = () -> " <> Text.pack input) $
                   \_ unisonFile ->
                      eval . Execute (view currentBranch s) . augment $
                        UF.discardTypes unisonFile
        QuitI -> quit
       where
        success       = respond $ Success input
        outputSuccess = eval . Notify $ Success input
   where
    doMerge branchName b = do
      updated <- eval $ SyncBranch branchName b
      when (not updated) $ do
        _       <- eval $ NewBranch branchName
        updated <- eval $ SyncBranch branchName b
        when (not updated) (disappearingBranchBomb branchName)
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
          _ <- eval $ NewBranch branchName
          currentBranch .= newBranch
          currentBranchName .= branchName
        Just branch -> do
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
  :: forall i v . Set NameTarget -> Name -> Name -> Action i v ()
aliasUnconflicted nameTargets oldName newName =
  modifyCurrentBranchM $ \branch ->
    let (branch', result) = foldl' go (branch, mempty) nameTargets
    in do
      respond $ AliasOutput oldName newName result
      pure $ branch'
  where
  go (branch, result) nameTarget = (result <>) <$> case nameTarget of
     Names.TermName ->
       alias nameTarget Branch.termsNamed Branch.addTermName branch
     Names.TypeName ->
       alias nameTarget Branch.typesNamed Branch.addTypeName branch
                       -- the RenameOutput action and setting the loop state
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
  :: forall i v . Set NameTarget -> Name -> Name -> Action i v ()
renameUnconflicted nameTargets oldName newName =
  modifyCurrentBranchM $ \branch ->
    let (branch', result) = foldl' go (branch, mempty) nameTargets
    in do
      respond $ RenameOutput oldName newName result
      pure branch'
  where
  go (branch, result) nameTarget = (result <>) <$> case nameTarget of
    Names.TermName ->
      rename nameTarget Branch.termsNamed Branch.renameTerm branch
    Names.TypeName ->
      rename nameTarget Branch.typesNamed Branch.renameType branch
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

merging :: BranchName -> Branch -> Action i v () -> Action i v ()
merging targetBranchName b success =
  ifM (eval $ SyncBranch targetBranchName b) success . respond $ UnknownBranch
    targetBranchName

modifyCurrentBranch0 :: (Branch0 -> Branch0) -> Action i v ()
modifyCurrentBranch0 f = modifyCurrentBranchM (\b -> pure $ Branch.modify f b)

modifyCurrentBranchM :: (Branch -> Action i v Branch) -> Action i v ()
modifyCurrentBranchM f = do
  b <- use currentBranch
  b' <- f b
  when (b /= b') $ do
    branchName <- use currentBranchName
    worked <- eval $ SyncBranch branchName b'
    when worked (currentBranch .= b')
