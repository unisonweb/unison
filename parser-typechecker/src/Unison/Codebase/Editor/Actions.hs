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
import           Control.Monad                  ( unless, when )
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.State            ( StateT
                                                , get
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Maybe      ( MaybeT(..))
import           Data.Foldable                  ( foldl'
                                                , toList
                                                , traverse_
                                                )
import Data.List (sortOn)
import           Data.Maybe                     ( catMaybes, fromMaybe, fromJust )
import qualified Data.Map as Map
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import           Data.Tuple.Extra               ((&&&))
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
                                                , SearchMode(..)
                                                , SlurpResult(..)
                                                , DisplayThing(..)
                                                , NameChangeResult
                                                  ( NameChangeResult)
                                                , collateReferences
                                                )
import qualified Unison.Codebase.Editor        as Editor
import           Unison.Codebase.SearchResult  (SearchResult)
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.Codebase.TermEdit      as TermEdit
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import           Unison.HashQualified           ( HashQualified )
import qualified Unison.HashQualified          as HQ
import qualified Unison.ShortHash              as SH
import qualified Unison.Name                   as Name
import           Unison.Name                    ( Name )
import           Unison.Names                   ( NameTarget )
import qualified Unison.Names                  as Names
import           Unison.Parser                  ( Ann(..) )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import           Unison.Result                  (pattern Result)
import qualified Unison.Runtime.IOSource      as IOSource
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Find              as Find
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import qualified Unison.Util.Relation          as Relation
import Unison.Util.TransitiveClosure (transitiveClosure')
import           Unison.Var                     ( Var )
import Debug.Trace
type Action i v a = MaybeT (StateT (LoopState v) (Free (Command i v))) a

data LoopState v
  = LoopState
      { _currentBranch :: Branch
      , _currentBranchName :: BranchName
      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , _latestFile :: Maybe (FilePath, SkipNextUpdate)
      , _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile v Ann)
      , _lastInput :: Maybe Input
      , _numberedArgs :: [String]
      }

type SkipNextUpdate = Bool

makeLenses ''LoopState

loopState0 :: Branch -> BranchName -> LoopState v
loopState0 b bn = LoopState b bn Nothing Nothing Nothing []

loop :: forall v . Var v => Action (Either Event Input) v ()
loop = do
    s <- get
    uf                 <- use latestTypecheckedFile
    currentBranchName' <- use currentBranchName
    latestFile'        <- use latestFile
    currentBranch'     <- use currentBranch
    e                  <- eval Input
    let withFile ambient sourceName text k = do
          Result notes r <- eval $
            Typecheck ambient (view currentBranch s) sourceName text
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
            withFile [] sourceName text $ \errorEnv unisonFile -> do
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
        SearchByNameI [] -> do
          let results = listBranch currentBranch'
          numberedArgs .= fmap searchResultToHQString results
          eval (LoadSearchResults results)
            >>= respond . ListOfDefinitions currentBranch' False
        SearchByNameI ["-l"] -> do
          let results = listBranch currentBranch'
          numberedArgs .= fmap searchResultToHQString results
          eval (LoadSearchResults results)
            >>= respond . ListOfDefinitions currentBranch' True
        -- ls with arguments
        SearchByNameI ("-l" : (fmap HQ.fromString -> qs)) -> do
          let results = searchBranch currentBranch' qs Editor.FuzzySearch
          numberedArgs .= fmap searchResultToHQString results
          eval (LoadSearchResults results)
            >>= respond . ListOfDefinitions currentBranch' True
        SearchByNameI (map HQ.fromString -> qs) -> do
          let results = searchBranch currentBranch' qs Editor.FuzzySearch
          numberedArgs .= fmap searchResultToHQString results
          eval (LoadSearchResults results)
            >>= respond . ListOfDefinitions currentBranch' False
        ShowDefinitionI outputLoc (fmap HQ.fromString -> qs) -> do
          results <- eval . LoadSearchResults $
                              searchBranch currentBranch' qs Editor.ExactSearch
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
        UnnameAllI hqs -> do
          modifyCurrentBranch0 $ \b ->
            let wrangle b hq = doTerms (doTypes b)
                  where
                  doTerms b = foldl' doTerm b (Branch.resolveHQNameTerm b hq)
                  doTypes b = foldl' doType b (Branch.resolveHQNameType b hq)
                  doTerm b (n, r) = Branch.deleteTermName r n b
                  doType b (n, r) = Branch.deleteTypeName r n b
            in foldl' wrangle b hqs
          respond $ Success input
        SlurpFileI allowUpdates query -> case uf of
          Nothing -> respond NoUnisonFile
          Just uf -> let
            -- 1. convert the query HQs to a list of refs.
            -- 2. compute the closure of the refs
            -- 3. if the refs = the closure of the refs, then add the refs.
            -- 4. otherwise prompt the user to reissue to add the closure.
            branch0 = Branch.head currentBranch'
            refs :: Set Reference
            refs = Set.fromList (toList termRefMap <> toList typeRefMap)
            termRefMap = fmap (\(r,_,_) -> r) $ UF.hashTerms uf
            typeRefMap = fmap fst (UF.dataDeclarations' uf)
                      <> fmap fst (UF.effectDeclarations' uf)
            closedDeps = transitiveClosure'
                          (flip Relation.lookupDom (UF.dependencies' uf))
                          queryRefs
            fileClosedDeps = Set.intersection closedDeps refs
            queryRefs = Set.unions $ fmap wrangle query
              where
              multiMatchCrash sh = error $ "todo (a non-crashing error): " <> SH.toString sh <> " matches multiple definitions in the file: " <> show termRefMap
              wrangle = \case
                HQ.NameOnly n -> refsNamed n
                HQ.HashOnly sh -> match1 sh refs
                HQ.HashQualified n sh -> match1 sh (refsNamed n)
              match1 :: SH.ShortHash -> Set Reference -> Set Reference
              match1 sh refs =
                case [r | r <- toList refs
                        , sh `SH.isPrefixOf` Reference.toShortHash r] of
                  [] -> mempty
                  [r] -> Set.singleton r
                  _ -> multiMatchCrash sh
              refsNamed ( Name.toVar -> v) =
                Set.fromList
                  $ toList (Map.lookup v termRefMap)
                  <> toList (Map.lookup v typeRefMap)
            uf' = if null query then uf else UF.filterTLCs keep uf
            keep r = Set.member r closedDeps && not (Branch.contains branch0 r)
            ppe = PPE.fromNames (UF.typecheckedToNames uf')
                <> Branch.prettyPrintEnv branch0
            reprompt = respond (AddTransitivelyConfirmation ppe uf')
            proceed = do
              let
                collisionHandler = if allowUpdates
                  then Editor.updateCollisionHandler
                  else Editor.addCollisionHandler
              updateo <- eval $ SlurpFile collisionHandler currentBranch' uf'
              let branch' = updatedBranch updateo
              -- Don't bother doing anything if the branch is unchanged by the slurping
              when (branch' /= currentBranch') $ doMerge currentBranchName' branch'
              eval . Notify $ SlurpOutput updateo
              currentBranch .= branch'
            in
            if fileClosedDeps == queryRefs then proceed
            else do
              traceShowM queryRefs
              traceShowM fileClosedDeps
              traceShowM closedDeps
              ifM (confirmedCommand input) proceed reprompt
        ListBranchesI ->
          eval ListBranches >>= respond . ListOfBranches currentBranchName'
        SwitchBranchI branchName       -> switchBranch branchName
        ForkBranchI   targetBranchName -> ifM
          (eval $ NewBranch currentBranch' targetBranchName)
          (outputSuccess *> switchBranch targetBranchName)
          (respond $ BranchAlreadyExists targetBranchName)
        MergeBranchI inputBranchName ->
          withBranch inputBranchName $ \branch -> do
            let merged0 = branch <> currentBranch'
            merged <- eval $ Propagate merged0
            ok     <- eval $ SyncBranch currentBranchName' merged
            if ok
              then do
                currentBranch .= merged
                respond $ Success input -- a merge-specific message
                checkTodo
              else respond (UnknownBranch inputBranchName)
        DeleteBranchI branchNames ->
          withBranches branchNames $ \bnbs -> do
          uniqueToDelete <- prettyUniqueDefinitions bnbs
          let deleteBranches b =
                traverse (eval . DeleteBranch) b >> respond (Success input)
          if (currentBranchName' `elem` branchNames) then
                  respond DeletingCurrentBranch
          else if null uniqueToDelete then deleteBranches branchNames
          else ifM (confirmedCommand input)
                   (deleteBranches branchNames)
                   (respond . DeleteBranchConfirmation $ uniqueToDelete)

        TodoI -> checkTodo
        PropagateI -> do
          b <- eval . Propagate $ currentBranch'
          _ <- eval $ SyncBranch currentBranchName' b
          _ <- success
          currentBranch .= b
        ExecuteI input ->
          withFile [Type.ref External $ IOSource.ioReference]
                   "execute command"
                   ("main_ = " <> Text.pack input) $
                     \_ unisonFile ->
                        eval . Execute (view currentBranch s) $
                          UF.discardTypes unisonFile
        UpdateBuiltinsI -> do
          modifyCurrentBranch0 updateBuiltins
          checkTodo
        ListEditsI -> do
          (Branch.head -> b) <- use currentBranch
          respond $ ListEdits b
        QuitI -> quit
       where
        success       = respond $ Success input
        outputSuccess = eval . Notify $ Success input
    case e of
      Right input -> lastInput .= Just input
      _ -> pure ()
   where
    doMerge branchName b = do
      updated <- eval $ SyncBranch branchName b
      -- updated is False if `branchName` doesn't exist.
      -- Not sure why you were updating a nonexistent branch, but under the
      -- assumption that it just got deleted somehow, I guess, we'll write
      -- it to disk now.
      unless updated $ do
        written <- eval $ NewBranch b branchName
        unless written (disappearingBranchBomb branchName)
    disappearingBranchBomb branchName =
      error
        $  "The branch named "
        <> Text.unpack branchName
        <> " disappeared from storage. "
        <> "I tried to put it back, but couldn't. Everybody panic!"
    -- todo: when `branch` becomes purely switchBranch and not newBranch, fix this up.
    switchBranch branchName = do
      branch <- eval $ LoadBranch branchName
      case branch of
        Nothing -> do
          let newBranch = Editor.builtinBranch
          _ <- eval $ NewBranch newBranch branchName
          currentBranch .= newBranch
          currentBranchName .= branchName
          respond $ CreatedBranch $ branchName
        Just branch -> do
          currentBranch .= branch
          currentBranchName .= branchName
          respond $ SwitchedBranch $ branchName
      checkForBuiltinsMismatch
    quit = MaybeT $ pure Nothing

eval :: Command i v a -> Action i v a
eval = lift . lift . Free.eval

confirmedCommand :: Input -> Action i v Bool
confirmedCommand i = do
  i0 <- use lastInput
  pure $ Just i == i0

loadBranch :: BranchName -> Action i v (Maybe Branch)
loadBranch = eval . LoadBranch

listBranch :: Branch -> [SearchResult]
listBranch (Branch.head -> b) =
  sortOn (\s -> (SR.name s, s)) (Branch.asSearchResults b)

searchResultToHQString :: SearchResult -> String
searchResultToHQString = \case
  SR.Tm' n r _ -> HQ.toString $ HQ.requalify n r
  SR.Tp' n r _ -> HQ.toString $ HQ.requalify n (Referent.Ref r)
  _ -> error "unpossible match failure"

-- Return a list of definitions whose names fuzzy match the given queries.
searchBranch :: Branch -> [HashQualified] -> SearchMode -> [SearchResult]
searchBranch (Branch.head -> b) queries = \case
  ExactSearch -> Branch.searchBranch b exactNameDistance queries
  FuzzySearch -> Branch.searchBranch b fuzzyNameDistance queries
  where
  exactNameDistance n1 n2 = if n1 == n2 then Just () else Nothing
  fuzzyNameDistance (Name.toString -> q) (Name.toString -> n) =
    case Find.fuzzyFindMatchArray q [n] id of
      [] -> Nothing
      (m, _) : _ -> Just m


withBranch :: BranchName -> (Branch -> Action i v ()) -> Action i v ()
withBranch b f = loadBranch b >>= maybe (respond $ UnknownBranch b) f

withBranches :: [BranchName] -> ([(BranchName, Branch)] -> Action i v ()) -> Action i v ()
withBranches branchNames f = do
  branches :: [Maybe Branch] <- traverse (eval . LoadBranch) branchNames
  if any null branches
  then traverse_ (respond . UnknownBranch)
          [name | (name, Nothing) <- branchNames `zip` branches]
  else f (branchNames `zip` fmap fromJust branches)

respond :: Output v -> Action i v ()
respond output = eval $ Notify output

-- Collects the definitions that are not named in any branch outside the inputs.
-- It collects all the references within the branches that *aren't* specified,
-- and then reports if the branches that *are* specified contain any references
-- that don't exist anywhere else.
prettyUniqueDefinitions :: forall i v.
  [(BranchName, Branch)] -> Action i v [(BranchName, (PPE.PrettyPrintEnv, [Editor.SearchResult' v Ann]))]
prettyUniqueDefinitions queryBNBs = do
    let (branchNames, _) = unzip queryBNBs
    otherBranchNames <- filter (`notElem` branchNames) <$> eval ListBranches
    otherKnownReferences :: Set Reference <-
      mconcat
        . fmap (Branch.allNamedReferences . Branch.head)
        . ((Editor.builtinBranch):) -- we dont care about saving these
        . catMaybes
        <$> traverse loadBranch otherBranchNames
    raw <- (traverse . traverse) -- traverse over `[]` and `(BranchName,)`
      (sequence -- traverse over (PPE,)
        . go otherKnownReferences
        . Branch.head)
      queryBNBs
    -- remove empty entries like this one: ("test4",(PrettyPrintEnv,[]))
    pure . filter (not . null . snd . snd) $ raw
  where
  go :: Set Reference
     -> Branch0
     -> (PPE.PrettyPrintEnv, Action i v [Editor.SearchResult' v Ann])
  go known =
    Branch.prettyPrintEnv &&& eval . LoadSearchResults . pickResults known
  pickResults :: Set Reference -> Branch0 -> [SearchResult]
  pickResults known = filter (keep known) . Branch.asSearchResults
  keep :: Set Reference -> SearchResult -> Bool
  keep known = \case
    SR.Tp' _ r@(Reference.DerivedId _) _ -> Set.notMember r known
    SR.Tm' _ (Referent.Ref r@(Reference.DerivedId _)) _ -> Set.notMember r known
    _ -> False

updateBuiltins :: Branch0 -> Branch0
updateBuiltins b
  -- This branch should include:
  --   names for all refs missing from existing branch
  --   ~~deprecations for the missing references~~
  --   no, can't just be deprecations for the missing references,
  --      they would never be able to come back. :-\
  --   ok, we'll fix the story for neverending edits later;
  --   todo: reevaluate this after that.
  -- todo: remove deprecations for newly added terms?
            -- what if user intentionally removed
  = over (Branch.namespaceL . Branch.terms) (Relation.||> oldRefts)
  . over (Branch.namespaceL . Branch.types) (Relation.||> oldRefs)
  . over (Branch.namespaceL . Branch.terms) (<> newTerms)
  . over (Branch.namespaceL . Branch.types) (<> newTypes)
  . over (Branch.editedTermsL) (<> deprecatedTerms)
  . over (Branch.editedTypesL) (<> deprecatedTypes)
  $ b
  where
  newTerms =
    (Branch.termNamespace . Branch.head) Editor.builtinBranch
      Relation.|> (Set.map Referent.Ref newRefs)
  newTypes =
    (Branch.typeNamespace . Branch.head) Editor.builtinBranch
      Relation.|> newRefs
  deprecatedTerms =
    Relation.fromList [ (r, TermEdit.Deprecate) | r <- toList oldRefs ]
  deprecatedTypes =
    Relation.fromList [ (r, TypeEdit.Deprecate) | r <- toList oldRefs ]

  -- Builtin references in the "empty" branch, but not in the current branch
  newRefs = refs Editor.builtinBranch0 `Set.difference` refs b
  -- Builtin references in the current branch, but not in the empty branch
  -- Todo: filter away the structural types from this list; they don't need
  -- to be deleted.  For nominal / unique types, let's think about it?
  oldRefts = Set.map (Referent.Ref) oldRefs
  oldRefs = Set.fromList [r | r@(Reference.Builtin _) <- toList $ oldRefs']
  oldRefs' = refs b `Set.difference` refs Editor.builtinBranch0
  refs = Branch.allNamedReferences


checkForBuiltinsMismatch :: Action i v ()
checkForBuiltinsMismatch = do
  b <- use currentBranch
  when (not $ all null [new b, old b]) $
    respond $ BustedBuiltins (new b) (old b)
  where
  -- Builtin references in the "empty" branch, but not in the current branch
  new b = refs Editor.builtinBranch `Set.difference` refs b
  -- Builtin references in the current branch, but not in the empty branch
  -- Todo: filter away the structural types from this list; they don't need
  -- to be deleted.  For nominal / unique types, let's think about it.
  old b = Set.fromList [r | r@(Reference.Builtin _) <- toList $ old' b]
  old' b = refs b `Set.difference` refs Editor.builtinBranch
  refs = Branch.allNamedReferences . Branch.head

checkTodo :: Action i v ()
checkTodo = do
  b <- use currentBranch
  eval (Todo b) >>= respond . TodoOutput b


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

-- todo: should this go away?
merging :: BranchName -> Branch -> Action i v () -> Action i v ()
merging targetBranchName b success =
  ifM (eval $ SyncBranch targetBranchName b) success . respond $ UnknownBranch
    targetBranchName

modifyCurrentBranch0 :: (Branch0 -> Branch0) -> Action i v ()
modifyCurrentBranch0 f = modifyCurrentBranchM (\b -> pure $ Branch.modify f b)

modifyCurrentBranch0M :: (Branch0 -> Action i v Branch0) -> Action i v ()
modifyCurrentBranch0M f = modifyCurrentBranchM $ \b -> do
  b0' <- f $ Branch.head b
  pure $ Branch.append b0' b

modifyCurrentBranchM :: (Branch -> Action i v Branch) -> Action i v ()
modifyCurrentBranchM f = do
  b <- use currentBranch
  b' <- f b
  when (b /= b') $ do
    branchName <- use currentBranchName
    worked <- eval $ SyncBranch branchName b'
    when worked (currentBranch .= b')
