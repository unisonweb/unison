{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Codebase.Editor.HandleInput (loop, loopState0) where

import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.RemoteRepo

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad                  ( foldM, liftM2, when )
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.State            ( StateT
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Foldable                  ( find
                                                , toList
                                                )
import qualified Data.List                      as List
import           Data.List.Extra                (nubOrd)
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , mapMaybe
                                                )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import           Data.Tuple.Extra               ((&&&))
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Unison.ABT                    as ABT
import           Unison.Codebase.Branch2        ( Branch
                                                , Branch0
                                                )
import qualified Unison.Codebase.Branch2       as Branch
import qualified Unison.Codebase.BranchUtil    as BranchUtil
import           Unison.Codebase.Path           ( Path
                                                , Path'
                                                , HQSegment
                                                , NameSegment
                                                )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.SearchResult   ( SearchResult )
import qualified Unison.Codebase.SearchResult  as SR
import           Unison.HashQualified           ( HashQualified )
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name
import           Unison.Name                    ( Name )
import           Unison.Names2                  ( Names'(..), Names, Names0, NamesSeg )
import qualified Unison.Names2                  as Names
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.Result                  ( pattern Result )
import qualified Unison.ShortHash as SH
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Find              as Find
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Util.List               ( uniqueBy )
import qualified Unison.Util.Relation          as R
import           Unison.Var                     ( Var )

type F m i v = Free (Command m i v)
type Type v a = Type.AnnotatedType v a

-- type (Action m i v) a
type Action m i v = MaybeT (StateT (LoopState m v) (F m i v))

liftToAction :: m a -> Action m i v a
liftToAction = lift . lift . Free.eval . Eval

data LoopState m v
  = LoopState
      { _root :: Branch m
      -- the current position in the namespace
      , _currentPath :: Path.Absolute

      -- TBD
      -- , _activeEdits :: Set Branch.EditGuid

      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , _latestFile :: Maybe (FilePath, SkipNextUpdate)
      , _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile v Ann)

      -- The previous user input. Used to request confirmation of
      -- questionable user commands.
      , _lastInput :: Maybe Input

      -- A 1-indexed list of strings that can be referenced by index at the
      -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
      -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
      , _numberedArgs :: [String]
      }

type SkipNextUpdate = Bool

makeLenses ''LoopState

loopState0 :: Branch m -> Path.Absolute -> LoopState m v
loopState0 b p = LoopState b p Nothing Nothing Nothing []

type Action' m v = Action m (Either Event Input) v

loop :: forall m v . (Monad m, Var v) => Action m (Either Event Input) v ()
loop = do
  uf          <- use latestTypecheckedFile
  root'        <- use root
  currentPath' <- use currentPath
  latestFile'  <- use latestFile
  currentBranch' <- getAt currentPath'
  let
      root0 = Branch.head root'
      resolvePath' :: (Path', a) -> (Path, a)
      resolvePath' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      resolveAsAbsolute = Path.fromAbsoluteSplit . Path.toAbsoluteSplit Path.absoluteEmpty
      getAtSplit :: Path.Split' -> Maybe (Branch m)
      getAtSplit p = BranchUtil.getBranch (resolvePath' p) root0
      getHQTypes :: Path.HQSplit' -> Set Reference
      getHQTypes p = BranchUtil.getType (resolvePath' p) root0
      getHQTerms :: Path.HQSplit' -> Set Referent
      getHQTerms p = BranchUtil.getTerm (resolvePath' p) root0
      getHQ'Terms = getHQTerms . fmap HQ'.toHQ
      -- These don't quite make sense, because a HQ'Split' includes a name.
      -- A regular HQSplit' may be missing a name, and then it .. well
      -- even then, a NameSegment probably isn't going to cut it.
      -- getNamedHQTypes :: Path.HQ'Split' -> Set (NameSegment, Reference)
      -- getNamedHQTypes p = BranchUtil.getNamedType (resolvePath' p) root0
      -- getNamedHQTerms :: Path.HQ'Split' -> Set (NameSegment, Referent)
      -- getNamedHQTerms p = BranchUtil.getNamedTerm (resolvePath' p) root0
      getTypes :: Path.Split' -> Set Reference
      getTypes = getHQTypes . fmap HQ.NameOnly
      getTerms :: Path.Split' -> Set Referent
      getTerms = getHQTerms . fmap HQ.NameOnly

      -- unsnocPath' :: Path' -> (Absolute, NameSegment)
      -- unsnocPath' = fromJust
      --           . fmap (first Absolute)
      --           . (\(Absolute p) -> Path.unsnoc p)
      --           . Path.toAbsolutePath currentPath'

      -- todo: don't need to use this version, because the NamesSeg and deepReferentes are built into the Branch0 now.
      -- loadHqSrc ::
      --   Path.HQSplit' -> _ (Branch m, NamesSeg, Names0, Absolute, HQSegment)
      -- loadHqSrc hq = do
      --   let (p, seg) = toAbsoluteSplit hq
      --   b <- getAt p
      --   pure ( b
      --        , Branch.toNamesSeg (Branch.head b)
      --        , Branch.toNames0 (Branch.head b)
      --        , p, seg)

  let names' = Branch.toNames (Branch.head currentBranch')
  e           <- eval Input
  let withFile ambient sourceName text k = do
        Result notes r <- eval
             $ Typecheck ambient (error "todo") sourceName text
          -- $ Typecheck ambient (view Branch.history $ get nav') sourceName text
        case r of
          -- Parsing failed
          Nothing -> error "todo"
          -- respond
          --   $ ParseErrors text [ err | Result.Parsing err <- toList notes ]
          Just (names, r) ->
            let h = respond $ TypeErrors
                  text
                  names
                  [ err | Result.TypeError err <- toList notes ]
            in  maybe h (k names) r
  case e of
    Left (IncomingRootBranch _names) ->
      error $ "todo: notify user about externally deposited head, and offer\n"
           ++ "a command to undo the merge that is about to happen.  In the\n"
           ++ "mean time until this is implemented, you can fix the issue by\n"
           ++ "deleting one of the heads from `.unison/branches/head/`."

    Left (UnisonFileChanged sourceName text) ->
      -- We skip this update if it was programmatically generated
      if fromMaybe False . fmap snd $ latestFile'
        then modifying latestFile (fmap (const False) <$>)
        else do
          eval (Notify $ FileChangeEvent sourceName text)
          withFile [] sourceName text $ \errorEnv unisonFile -> do
            eval (Notify $ Typechecked sourceName errorEnv unisonFile)
            (bindings, e) <- error "todo"
--               eval . Evaluate (view currentBranch s) $
--                    UF.discardTypes unisonFile
            let e' = Map.map go e
                go (ann, _hash, _uneval, eval, isHit) = (ann, eval, isHit)
            -- todo: this would be a good spot to update the cache
            -- with all the (hash, eval) pairs, even if it's just an
            -- in-memory cache
            eval . Notify $ Evaluated
              text
              (error$"todo: produce Names2 for displaying evaluated Result.\n"
                  ++ "It should include names from the file and the Branch,\n"
                  ++ "and distinguish somehow between existing and new defns\n"
                  ++ "having the same name.")
              -- (errorEnv <> Branch.prettyPrintEnv (Branch.head currentBranch'))
              bindings
              e'
            latestFile .= Just (Text.unpack sourceName, False)
            latestTypecheckedFile .= Just unisonFile
    Right input ->
      let
        ifConfirmed = ifM (confirmedCommand input)
        ifNotConfirmed = flip ifConfirmed
        branchNotFound = respond . BranchNotFound input
        typeNotFound = respond . TypeNotFound input
        termNotFound = respond . TermNotFound input
        typeConflicted src = respond . TypeAmbiguous input src
        termConflicted src = respond . TermAmbiguous input src
        branchExists dest _ = respond $ BranchAlreadyExists input dest
        typeExists dest = respond . TypeAlreadyExists input dest
        termExists dest = respond . TermAlreadyExists input dest
      in case input of
      ForkLocalBranchI src dest ->
        maybe (branchNotFound src) srcOk (getAtSplit src)
        where
        srcOk b = maybe (destOk b) (branchExists dest) (getAtSplit dest)
        destOk b = do
          stepAt . BranchUtil.makeSetBranch (resolvePath' dest) $ b
          success -- could give rando stats about new defns

      MergeLocalBranchI src dest ->
        maybe (branchNotFound src) srcOk (getAtSplit src)
        where
        srcOk b = maybe (destEmpty b) (destExists b) (getAtSplit dest)
        destEmpty b = ifNotConfirmed (branchNotFound dest)
          (stepAt $ BranchUtil.makeSetBranch (resolvePath' dest) b)
        destExists srcb destb = do
          merged <- eval . Eval $ Branch.merge srcb destb
          stepAt $ BranchUtil.makeSetBranch (resolvePath' dest) merged
          success

      SwitchBranchI path' -> do
        path <- use $ currentPath . to (`Path.toAbsolutePath` path')
        currentPath .= path
        branch' <- getAt path
        when (Branch.isEmpty . Branch.head $ branch')
          (respond $ CreatedNewBranch path)

      AliasTermI src dest ->
        zeroOneOrMore (getHQTerms src) (termNotFound src) srcOk (termConflicted src)
        where
        srcOk src = zeroOrMore (getTerms dest) (destOk src) (termExists dest)
        destOk = stepAt . BranchUtil.makeAddTermName (resolvePath' dest)

      AliasTypeI src dest ->
        zeroOneOrMore (getHQTypes src) (typeNotFound src) srcOk (typeConflicted src)
        where
        srcOk r = zeroOrMore (getTypes dest) (destOk r) (typeExists dest)
        destOk = stepAt . BranchUtil.makeAddTypeName (resolvePath' dest)

      MoveTermI src'@(fmap HQ'.toHQ -> src) dest ->
        zeroOneOrMore (getHQTerms src) (termNotFound src) srcOk (termConflicted src)
        where
        srcOk r = zeroOrMore (getTerms dest) (destOk r) (termExists dest)
        destOk r = stepManyAt
          [ BranchUtil.makeDeleteTermName (resolvePath' (HQ'.toName <$> src')) r
          , BranchUtil.makeAddTermName (resolvePath' dest) r ]

      MoveTypeI src'@(fmap HQ'.toHQ -> src) dest ->
        zeroOneOrMore (getHQTypes src) (typeNotFound src) srcOk (typeConflicted src)
        where
        srcOk r = zeroOrMore (getTypes dest) (destOk r) (typeExists dest)
        destOk r = stepManyAt
          [ BranchUtil.makeDeleteTypeName (resolvePath' (HQ'.toName <$> src')) r
          , BranchUtil.makeAddTypeName (resolvePath' dest) r ]

      MoveBranchI src dest ->
        maybe (branchNotFound src) srcOk (getAtSplit src)
        where
        srcOk b = maybe (destOk b) (branchExists dest) (getAtSplit dest)
        destOk b = do
          stepManyAt
            [ BranchUtil.makeSetBranch (resolvePath' src) Branch.empty
            , BranchUtil.makeSetBranch (resolvePath' dest) b ]
          success -- could give rando stats about new defns

      DeleteTypeI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTypes hq) (typeNotFound hq) (goMany . Set.singleton)
                      (liftM2 ifConfirmed goMany (typeConflicted hq))
        where
        resolvedPath = resolvePath' (HQ'.toName <$> hq')
        makeDelete = BranchUtil.makeDeleteTypeName resolvedPath
        goMany rs = do
          let rootNames = Branch.toNames0 root0
              name = Path.toName . Path.unsplit $ resolvedPath
              toDelete = Names.fromTypes ((name,) <$> toList rs)
          (failed, failedDependents) <- getEndangeredDependents (eval . GetDependents) rootNames toDelete
          if failed == mempty then stepManyAt . fmap makeDelete . toList $ rs
          else do
            failed <- eval . LoadSearchResults $ Names.asSearchResults failed
            failedDependents <- eval . LoadSearchResults $ Names.asSearchResults failedDependents
            respond $ CantDelete input failed failedDependents

      -- like the previous
      DeleteTermI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTerms hq) (termNotFound hq) (goMany . Set.singleton)
                      (liftM2 ifConfirmed goMany (termConflicted hq))
        where
        resolvedPath = resolvePath' (HQ'.toName <$> hq')
        makeDelete = BranchUtil.makeDeleteTermName resolvedPath
        goMany rs = do
          let rootNames = Branch.toNames0 root0
              name = Path.toName . Path.unsplit $ resolvedPath
              toDelete = Names.fromTerms ((name,) <$> toList rs)
          (failed, failedDependents) <- getEndangeredDependents (eval . GetDependents) rootNames toDelete
          if failed == mempty then stepManyAt . fmap makeDelete . toList $ rs
          else do
            failed <- eval . LoadSearchResults $ Names.asSearchResults failed
            failedDependents <- eval . LoadSearchResults $ Names.asSearchResults failedDependents
            respond $ CantDelete input failed failedDependents

      DeleteBranchI p -> maybe (branchNotFound p) go $ getAtSplit p where
        go (Branch.head -> b) = do
          let rootNames = Branch.toNames0 root0
              p' = resolvePath' p
              toDelete = Names.prefix0 (Path.toName . Path.unsplit $ p') (Branch.toNames0 b)
          (failed, failedDependents) <- getEndangeredDependents (eval . GetDependents) rootNames toDelete
          if failed == mempty then
            stepAt $ BranchUtil.makeSetBranch (resolvePath' p) Branch.empty
          else do
            failed <- eval . LoadSearchResults $ Names.asSearchResults failed
            failedDependents <- eval . LoadSearchResults $ Names.asSearchResults failedDependents
            respond $ CantDelete input failed failedDependents

      -- todo: this should probably be able to show definitions by Path.HQSplit'
      ShowDefinitionI outputLoc (fmap HQ.fromString -> hqs) -> do
        results <- eval . LoadSearchResults $ searchBranchExact currentBranch' hqs
        let termTypes :: Map.Map Reference (Type v Ann)
            termTypes =
              Map.fromList
                [ (r, t) | Output.Tm _ (Just t) (Referent.Ref r) _ <- results ]
            (collatedTypes, collatedTerms) = collateReferences
              (mapMaybe Output.tpReference results)
              (mapMaybe Output.tmReferent results)
        loadedTerms <- fmap Map.fromList . for (toList collatedTerms) $ \case
          r@(Reference.DerivedId i) -> do
            tm <- eval (LoadTerm i)
            -- We add a type annotation to the term using if it doesn't
            -- already have one that the user provided
            pure . (r, ) $ case liftA2 (,) tm (Map.lookup r termTypes) of
              Nothing        -> MissingThing i
              Just (tm, typ) -> case tm of
                Term.Ann' _ _ -> RegularThing tm
                _ -> RegularThing (Term.ann (ABT.annotation tm) tm typ)
          r@(Reference.Builtin _) -> pure (r, BuiltinThing)
        loadedTypes <- fmap Map.fromList . for (toList collatedTypes) $ \case
          r@(Reference.DerivedId i) ->
            (r, ) . maybe (MissingThing i) RegularThing <$> eval (LoadType i)
          r@(Reference.Builtin _) -> pure (r, BuiltinThing)

        -- makes sure that the user search terms get used as the names
        -- in the pretty-printer
        let
          names :: Names
          names = error $ "todo: come up with a Names here that's sufficient\n"
                      ++  "to pretty-print these loadedTerms, loadedTypes"
          -- ppe = -- now Names:
          --   PPE.fromTermNames [ (r, n) | Editor.Tm n _ r _ <- results ]
          --     <> PPE.fromTypeNames [ (r, n) | Editor.Tp n _ r _ <- results ]
          --     <> Branch.prettyPrintEnv (Branch.head currentBranch')
          loc = case outputLoc of
            ConsoleLocation    -> Nothing
            FileLocation path  -> Just path
            LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
        do
          eval . Notify $ DisplayDefinitions loc names loadedTypes loadedTerms
          -- We set latestFile to be programmatically generated, if we
          -- are viewing these definitions to a file - this will skip the
          -- next update for that file (which will happen immediately)
          latestFile .= ((, True) <$> loc)
      -- ls with no arguments
      SearchByNameI [] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' False
      SearchByNameI ["-l"] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' True
      -- ls with arguments
      SearchByNameI ("-l" : (fmap HQ.fromString -> qs)) -> do
        let results = searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' True
      SearchByNameI (map HQ.fromString -> qs) -> do
        let results = searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' False

      ResolveTypeNameI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTypes hq) (typeNotFound hq) go (typeConflicted hq)
        where
        conflicted = getHQTypes (fmap HQ'.toNameOnlyHQ hq')
        makeDelete r =
          BranchUtil.makeDeleteTypeName (resolvePath' (HQ'.toName <$> hq')) r
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      ResolveTermNameI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTerms hq) (termNotFound hq) go (termConflicted hq)
        where
        conflicted = getHQTerms (fmap HQ'.toNameOnlyHQ hq')
        makeDelete r =
          BranchUtil.makeDeleteTermName (resolvePath' (HQ'.toName <$> hq')) r
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      AddI hqs -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf ->
          let result = toSlurpResult hqs uf
                     . Branch.toNames0
                     . Branch.head
                     $ currentBranch' in
          if Output.isNonemptySlurp result then do
            stepAt (Path.unabsolute currentPath', applySlurpResult uf result)
            eval $ AddDefsToCodebase (finalFile result)
          -- todo: notify the user if we grew their selection automatically to
          --       include transitive dependencies, and tell them how to undo.
          else respond $ SlurpOutput input result
          -- finalUF is the transitive closure of the intersection of HQs and uf

      UpdateI _edits _hqs -> error "todo"

      -- SlurpFileI allowUpdates -> case uf of
      --   Nothing  -> respond NoUnisonFile
      --   Just uf' -> do
      --     let collisionHandler = if allowUpdates
      --           then Editor.updateCollisionHandler
      --           else Editor.addCollisionHandler
      --     updateo <- eval $ SlurpFile collisionHandler currentBranch' uf'
      --     let branch' = updatedBranch updateo
      --     -- Don't bother doing anything if the branch is unchanged by the slurping
      --     when (branch' /= currentBranch') $ doMerge currentBranchName' branch'
      --     eval . Notify $ SlurpOutput updateo
      --     currentBranch .= branch'


      -- ListBranchesI ->
      --   eval ListBranches >>= respond . ListOfBranches currentBranchName'
      -- DeleteBranchI branchNames -> withBranches branchNames $ \bnbs -> do
      --   uniqueToDelete <- prettyUniqueDefinitions bnbs
      --   let deleteBranches b =
      --         traverse (eval . DeleteBranch) b >> respond (Success input)
      --   if (currentBranchName' `elem` branchNames)
      --     then respond DeletingCurrentBranch
      --     else if null uniqueToDelete
      --       then deleteBranches branchNames
      --       else ifM (confirmedCommand input)
      --                (deleteBranches branchNames)
      --                (respond . DeleteBranchConfirmation $ uniqueToDelete)
      -- TodoI -> checkTodo
      -- PropagateI -> do
      --   b <- eval . Propagate $ currentBranch'
      --   _ <- eval $ SyncBranch currentBranchName' b
      --   _ <- success
      --   currentBranch .= b
      -- ExecuteI input ->
      --   withFile [Type.ref External $ IOSource.ioReference]
      --            "execute command"
      --            ("main_ = " <> Text.pack input) $
      --              \_ unisonFile ->
      --                 eval . Execute (view currentBranch s) $
      --                   UF.discardTypes unisonFile
      -- UpdateBuiltinsI -> do
      --   stepAt updateBuiltins
      --   checkTodo
      -- ListEditsI -> do
      --   (Branch.head -> b) <- use currentBranch
      --   respond $ ListEdits b
      -- QuitI -> quit
      _ -> error $ "todo: " <> show input
     where
      success       = respond $ Success input
  case e of
    Right input -> lastInput .= Just input
    _ -> pure ()
 -- where
  {-
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
  -}

eval :: Command m i v a -> Action m i v a
eval = lift . lift . Free.eval

confirmedCommand :: Input -> Action m i v Bool
confirmedCommand i = do
  i0 <- use lastInput
  pure $ Just i == i0

-- loadBranch :: BranchName -> Action m i v (Maybe Branch)
-- loadBranch = eval . LoadBranch


listBranch :: Branch0 m -> [SearchResult]
listBranch (Branch.toNames0 -> b) =
  List.sortOn (\s -> (SR.name s, s)) (Names.asSearchResults b)

-- | restores the full hash to these search results, for _numberedArgs purposes
searchResultToHQString :: SearchResult -> String
searchResultToHQString = \case
  SR.Tm' n r _ -> HQ.toString $ HQ.requalify n r
  SR.Tp' n r _ -> HQ.toString $ HQ.requalify n (Referent.Ref r)
  _ -> error "unpossible match failure"

-- Return a list of definitions whose names fuzzy match the given queries.
fuzzyNameDistance :: Name -> Name -> Maybe _ -- MatchArray
fuzzyNameDistance (Name.toString -> q) (Name.toString -> n) =
  case Find.fuzzyFindMatchArray q [n] id of
    [] -> Nothing
    (m, _) : _ -> Just m

-- return `name` and `name.<everything>...`
searchBranchPrefix :: Branch m -> Name -> [SearchResult]
searchBranchPrefix b n = case Path.unsnoc (Path.fromName n) of
  Nothing -> []
  Just (init, last) -> case Branch.getAt init b of
    Nothing -> []
    Just b -> Names.asSearchResults . Names.prefix0 n $ names0
      where
      lastName = Path.toName (Path.singleton last)
      subnames = Branch.toNames0 . Branch.head $ (Branch.getAt' (Path.singleton last) b)
      rootnames =
        Names.filter (== lastName) .
        Branch.toNames0 . set Branch.children mempty $ Branch.head b
      names0 = rootnames <> Names.prefix0 lastName subnames

searchBranchScored :: forall m score. (Ord score)
              => Branch m
              -> (Name -> Name -> Maybe score)
              -> [HashQualified]
              -> [SearchResult]
searchBranchScored b score queries =
  nubOrd . fmap snd . toList $ searchTermNamespace <> searchTypeNamespace
  where
  names0 = Branch.toNames0 . Branch.head $ b
  searchTermNamespace = foldMap do1query queries
    where
    do1query :: HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.terms $ names0)
    score1hq :: HashQualified -> (Name, Referent) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` (Referent.toShortHash ref) ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` (Referent.toShortHash ref) ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = Names.termSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty
  searchTypeNamespace = foldMap do1query queries
    where
    do1query :: HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.types $ names0)
    score1hq :: HashQualified -> (Name, Reference) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` (Reference.toShortHash ref) ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` (Reference.toShortHash ref) ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = Names.typeSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty

-- Separates type references from term references and returns types and terms,
-- respectively. For terms that are constructors, turns them into their data
-- types.
collateReferences
  :: Foldable f
  => Foldable g
  => f Reference -- types requested
  -> g Referent -- terms requested, including ctors
  -> (Set Reference, Set Reference)
collateReferences (toList -> types) (toList -> terms) =
  let terms' = [ r | Referent.Ref r <- terms ]
      types' = [ r | Referent.Con r _ <- terms ]
  in  (Set.fromList types' <> Set.fromList types, Set.fromList terms')

-- Foo#123
-- Foo#890
-- bar#567
-- blah#abc
-- cat#abc
-- and search for

-- Foo, want Foo#123 and Foo#890
-- Foo#1, want Foo#123
-- #567, want bar -- what goes in the SR.name?
-- blah, cat, want blah (with comment about cat)?

-- #567 :: Int
-- #567 = +3

searchBranchExact :: Branch m -> [HashQualified] -> [SearchResult]
searchBranchExact b queries = let
  names0 = Branch.toNames0 . Branch.head $ b
  matchesHashPrefix :: (r -> SH.ShortHash) -> (Name, r) -> HashQualified -> Bool
  matchesHashPrefix toShortHash (name, r) = \case
    HQ.NameOnly n -> n == name
    HQ.HashOnly q -> q `SH.isPrefixOf` toShortHash r
    HQ.HashQualified n q ->
      n == name && q `SH.isPrefixOf` toShortHash r
  filteredTypes, filteredTerms, deduped :: [SearchResult]
  filteredTypes =
    [ SR.typeResult query r (Names.hqTypeAliases names0 name r)
    | (name, r) <- R.toList $ Names.types names0
    , Just query <- [find (matchesHashPrefix Reference.toShortHash (name, r)) queries ]
    ]
  filteredTerms =
    [ SR.termResult query r (Names.hqTermAliases names0 name r)
    | (name, r) <- R.toList $ Names.terms names0
    , Just query <- [find (matchesHashPrefix Referent.toShortHash (name, r)) queries ]
    ]
  deduped = uniqueBy SR.toReferent (filteredTypes <> filteredTerms)
  in List.sort deduped

-- withBranch :: BranchName -> (Branch -> Action m i v ()) -> Action m i v ()
-- withBranch b f = loadBranch b >>= maybe (respond $ UnknownBranch b) f
--
-- withBranches :: [BranchName] -> ([(BranchName, Branch)] -> Action m i v ()) -> Action m i v ()
-- withBranches branchNames f = do
--   branches :: [Maybe Branch] <- traverse (eval . LoadBranch) branchNames
--   if any null branches
--   then traverse_ (respond . UnknownBranch)
--           [name | (name, Nothing) <- branchNames `zip` branches]
--   else f (branchNames `zip` fmap fromJust branches)

respond :: Output v -> Action m i v ()
respond output = eval $ Notify output

-- -- Collects the definitions that are not named in any branch outside the inputs.
-- -- It collects all the references within the branches that *aren't* specified,
-- -- and then reports if the branches that *are* specified contain any references
-- -- that don't exist anywhere else.
-- prettyUniqueDefinitions :: forall i v.
--   [(BranchName, Branch)] -> Action m i v [(BranchName, (PPE.PrettyPrintEnv, [Editor.SearchResult' v Ann]))]
-- prettyUniqueDefinitions queryBNBs = do
--     let (branchNames, _) = unzip queryBNBs
--     otherBranchNames <- filter (`notElem` branchNames) <$> eval ListBranches
--     otherKnownReferences :: Set Reference <-
--       mconcat
--         . fmap (Branch.allNamedReferences . Branch.head)
--         . ((Editor.builtinBranch):) -- we dont care about saving these
--         . catMaybes
--         <$> traverse loadBranch otherBranchNames
--     raw <- (traverse . traverse) -- traverse over `[]` and `(BranchName,)`
--       (sequence -- traverse over (PPE,)
--         . go otherKnownReferences
--         . Branch.head)
--       queryBNBs
--     -- remove empty entries like this one: ("test4",(PrettyPrintEnv,[]))
--     pure . filter (not . null . snd . snd) $ raw
--   where
--   go :: Set Reference
--      -> Branch0
--      -> (PPE.PrettyPrintEnv, Action m i v [Editor.SearchResult' v Ann])
--   go known =
--     Branch.prettyPrintEnv &&& eval . LoadSearchResults . pickResults known
--   pickResults :: Set Reference -> Branch0 -> [SearchResult]
--   pickResults known = filter (keep known) . Branch.asSearchResults
--   keep :: Set Reference -> SearchResult -> Bool
--   keep known = \case
--     SR.Tp' _ r@(Reference.DerivedId _) _ -> Set.notMember r known
--     SR.Tm' _ (Referent.Ref r@(Reference.DerivedId _)) _ -> Set.notMember r known
--     _ -> False
--
-- updateBuiltins :: Branch0 -> Branch0
-- updateBuiltins b
--   -- This branch should include:
--   --   names for all refs missing from existing branch
--   --   ~~deprecations for the missing references~~
--   --   no, can't just be deprecations for the missing references,
--   --      they would never be able to come back. :-\
--   --   ok, we'll fix the story for neverending edits later;
--   --   todo: reevaluate this after that.
--   -- todo: remove deprecations for newly added terms?
--             -- what if user intentionally removed
--   = over (Branch.namespaceL . Branch.terms) (Relation.||> oldRefts)
--   . over (Branch.namespaceL . Branch.types) (Relation.||> oldRefs)
--   . over (Branch.namespaceL . Branch.terms) (<> newTerms)
--   . over (Branch.namespaceL . Branch.types) (<> newTypes)
--   . over (Branch.editedTermsL) (<> deprecatedTerms)
--   . over (Branch.editedTypesL) (<> deprecatedTypes)
--   $ b
--   where
--   newTerms =
--     (Branch.termNamespace . Branch.head) Editor.builtinBranch
--       Relation.|> (Set.map Referent.Ref newRefs)
--   newTypes =
--     (Branch.typeNamespace . Branch.head) Editor.builtinBranch
--       Relation.|> newRefs
--   deprecatedTerms =
--     Relation.fromList [ (r, TermEdit.Deprecate) | r <- toList oldRefs ]
--   deprecatedTypes =
--     Relation.fromList [ (r, TypeEdit.Deprecate) | r <- toList oldRefs ]
--
--   -- Builtin references in the "empty" branch, but not in the current branch
--   newRefs = refs Editor.builtinBranch0 `Set.difference` refs b
--   -- Builtin references in the current branch, but not in the empty branch
--   -- Todo: filter away the structural types from this list; they don't need
--   -- to be deleted.  For nominal / unique types, let's think about it?
--   oldRefts = Set.map (Referent.Ref) oldRefs
--   oldRefs = Set.fromList [r | r@(Reference.Builtin _) <- toList $ oldRefs']
--   oldRefs' = refs b `Set.difference` refs Editor.builtinBranch0
--   refs = Branch.allNamedReferences
--
--
-- checkForBuiltinsMismatch :: Action m i v ()
-- checkForBuiltinsMismatch = do
--   b <- use currentBranch
--   when (not $ all null [new b, old b]) $
--     respond $ BustedBuiltins (new b) (old b)
--   where
--   -- Builtin references in the "empty" branch, but not in the current branch
--   new b = refs Editor.builtinBranch `Set.difference` refs b
--   -- Builtin references in the current branch, but not in the empty branch
--   -- Todo: filter away the structural types from this list; they don't need
--   -- to be deleted.  For nominal / unique types, let's think about it.
--   old b = Set.fromList [r | r@(Reference.Builtin _) <- toList $ old' b]
--   old' b = refs b `Set.difference` refs Editor.builtinBranch
--   refs = Branch.allNamedReferences . Branch.head
--
-- checkTodo :: Action m i v ()
-- checkTodo = do
--   b <- use currentBranch
--   eval (Todo b) >>= respond . TodoOutput b

loadRemoteBranchAt :: RemoteRepo -> Path.Absolute -> Action m i v (Branch m)
loadRemoteBranchAt repo (Path.Absolute p) = do
  root <- eval $ LoadRemoteRootBranch repo
  let b = Branch.getAt' p root
  let (types, terms) = collateReferences types0 terms0
        where
        types0 = Branch.deepTypeReferences (Branch.head root)
        terms0 = Branch.deepReferents (Branch.head root)
  eval $ RetrieveHashes repo types terms
  pure b

getAt :: Functor m => Path.Absolute -> Action m i v (Branch m)
getAt (Path.Absolute p) =
  use root <&> fromMaybe Branch.empty . Branch.getAt p

updateAtM :: Applicative m
          => Path.Absolute
          -> (Branch m -> Action m i v (Branch m))
          -> Action m i v ()
updateAtM (Path.Absolute p) f = do
  b <- use root
  b' <- Branch.modifyAtM p f b
  root .= b'
  when (b /= b') $ eval $ SyncLocalRootBranch b'

stepAt :: forall m i v. Applicative m
       => (Path, Branch0 m -> Branch0 m)
       -> Action m i v ()
stepAt = stepManyAt @m @[] . pure

stepManyAt :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m)
           -> Action m i v ()
stepManyAt actions = do
    b <- use root
    let b' = Branch.stepManyAt actions b
    root .= b'
    when (b /= b') $ eval $ SyncLocalRootBranch b'

-- cata for 0, 1, or more elements of a Foldable
-- tries to match as lazily as possible
zeroOneOrMore :: Foldable f => f a -> b -> (a -> b) -> (f a -> b) -> b
zeroOneOrMore f zero one more = case toList f of
  _ : _ : _ -> more f
  a : _ -> one a
  _ -> zero

zeroOrMore :: Foldable f => f a -> b -> (f a -> b) -> b
zeroOrMore f zero more = case toList f of
  a : _ -> more f
  _ -> zero

emptyOrNot :: (Monoid m, Eq m) => m -> b -> (m -> b) -> b
emptyOrNot m zero more = if m == mempty then zero else more m

-- Returns
--   ( the set of names that couldn't be deleted
--   , the set of dependents of the names that couldn't be deleted)
getEndangeredDependents :: forall m. Monad m
                        => (Reference -> m (Set Reference))
                        -> Names0
                        -> Names0
                        -> m (Names0, Names0)
getEndangeredDependents getDependents toBeDeleted root =
  -- for each r <- toBeDeleted,
    -- for each d <- dependents r
      -- if d `notElem` remaining
      -- then add r to failed, add d to failedDependents
      -- otherwise continue
  do
    acc <- foldM doTerms (mempty, mempty) (R.toList $ Names.terms toBeDeleted)
    foldM doTypes acc (R.toList $ Names.types toBeDeleted)
  where
  doTerms :: (Names0, Names0) -> (Name, Referent) -> m (Names0, Names0)
  doTerms acc (name, r) =
    List.foldl' f acc <$> getDependents (Referent.toReference r)
    where
    f (failed, failedDeps) d =
      if d `Set.notMember` remainingRefs
      then (Names.addTerm name r failed, addDependent d failedDeps)
      else (failed, failedDeps)
  addDependent :: Reference -> Names0 -> Names0
  addDependent r =
    (<> Names (Names.terms root R.|> Set.singleton (Referent.Ref r))
              (Names.types root R.|> Set.singleton r))
  doTypes :: (Names0, Names0) -> (Name, Reference) -> m (Names0, Names0)
  doTypes acc (name, r) =
    List.foldl' f acc <$> getDependents r
    where
    f (failed, failedDeps) d =
      if d `Set.notMember` remainingRefs
      then (Names.addType name r failed, addDependent d failedDeps)
      else (failed, failedDeps)
  remainingRefs :: Set Reference
  remainingRefs = Set.map Referent.toReference (Names.termReferents remaining)
                <> Names.typeReferences remaining
    where remaining = root `Names.difference` toBeDeleted

toSlurpResult :: [HashQualified] -> UF.TypecheckedUnisonFile v Ann -> Names0 -> SlurpResult v
toSlurpResult = error "todo"

applySlurpResult :: UF.TypecheckedUnisonFile v Ann -> SlurpResult v -> Branch0 m -> Branch0 m
applySlurpResult = error "todo"
