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
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.HandleInput (loop, loopState0, LoopState(..)) where

import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import qualified Unison.Codebase.Editor.SlurpResult as Slurp
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent(..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import Unison.Codebase.Editor.RemoteRepo

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad                  ( filterM, foldM, forM,
                                                  join, liftM2, when, void)
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.State            ( StateT
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Bifunctor                 ( second )
import           Data.Foldable                  ( find
                                                , toList
                                                , foldl'
                                                , traverse_
                                                )
import qualified Data.Graph as Graph
import qualified Data.List                      as List
import           Data.List.Extra                (nubOrd)
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , fromJust
                                                , isJust
                                                , mapMaybe
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import           Data.Tuple.Extra               ((&&&))
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Unison.ABT                    as ABT
import           Unison.Codebase.Branch2        ( Branch
                                                , Branch0(..)
                                                )
import qualified Unison.Codebase.Branch2       as Branch
import qualified Unison.Codebase.BranchUtil    as BranchUtil
import qualified Unison.Codebase.Metadata      as Metadata
import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Path           ( Path
                                                , Path' )
import           Unison.Codebase.NameSegment    ( HQSegment
                                                , NameSegment
                                                )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.SearchResult   ( SearchResult )
import qualified Unison.Codebase.SearchResult  as SR
import qualified Unison.DataDeclaration        as DD
import           Unison.HashQualified           ( HashQualified )
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name
import           Unison.Name                    ( Name )
import           Unison.Names2                  ( Names'(..), Names, Names0, NamesSeg )
import qualified Unison.Names2                  as Names
import qualified Unison.Names                  as OldNames
import qualified Unison.Parsers                as Parsers
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference(..) )
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
import           Unison.Util.TransitiveClosure  (transitiveClosure)
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import Unison.Codebase.TypeEdit (TypeEdit)
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Codebase.TermEdit (TermEdit)
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Typechecker as Typechecker
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.Runtime.IOSource       ( isTest )
import qualified Unison.Util.Star3             as Star3

import Debug.Trace

type F m i v = Free (Command m i v)
type Term v a = Term.AnnotatedTerm v a
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
  e           <- eval Input
  let
      root0 = Branch.head root'
      currentBranch0 = Branch.head currentBranch'
      resolveSplit' :: (Path', a) -> (Path, a)
      resolveSplit' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      resolvePath' :: Path' -> Path
      resolvePath' = Path.unabsolute . resolveToAbsolute
      resolveToAbsolute :: Path' -> Path.Absolute
      resolveToAbsolute = Path.toAbsolutePath currentPath'
      path'ToSplit :: Path' -> Maybe Path.Split
      path'ToSplit = Path.unsnoc . resolvePath'
      getAtSplit :: Path.Split -> Maybe (Branch m)
      getAtSplit p = BranchUtil.getBranch p root0
      getAtSplit' :: Path.Split' -> Maybe (Branch m)
      getAtSplit' = getAtSplit . resolveSplit'
      getHQTypes :: Path.HQSplit' -> Set Reference
      getHQTypes p = BranchUtil.getType (resolveSplit' p) root0
      getHQTerms :: Path.HQSplit' -> Set Referent
      getHQTerms p = BranchUtil.getTerm (resolveSplit' p) root0
      getHQ'Terms = getHQTerms . fmap HQ'.toHQ
      getHQ'Types = getHQTypes . fmap HQ'.toHQ
      getTypes :: Path.Split' -> Set Reference
      getTypes = getHQTypes . fmap HQ.NameOnly
      getTerms :: Path.Split' -> Set Referent
      getTerms = getHQTerms . fmap HQ.NameOnly
      getPatchAt :: Path.Split' -> Action' m v Patch
      getPatchAt patchPath' = do
        let (p, seg) = Path.toAbsoluteSplit currentPath' patchPath'
        b <- getAt p
        eval . Eval $ Branch.getPatch seg (Branch.head b)
      -- | Names used as a basis for computing slurp results.
      -- todo: include relevant external names if we support writing outside of this branch
      toSlurpResultNames _uf = prettyPrintNames0
      absoluteRootNames0 = Names.prefix0 (Name.Name "") (Branch.toNames0 root0)
      (parseNames0, prettyPrintNames0) = (parseNames00, prettyPrintNames00)
        where
        -- parsing should respond to local and absolute names
        parseNames00 = currentPathNames0 <> absoluteRootNames0
        -- pretty-printing should use local names where available
        prettyPrintNames00 = currentAndExternalNames0
        currentPathNames0 = Branch.toNames0 currentBranch0
        -- all names, but with local names in their relative form only, rather
        -- than absolute; external names appear as absolute
        currentAndExternalNames0 = currentPathNames0 <> absDot externalNames where
          absDot = Names.prefix0 (Name.Name "")
          externalNames = rootNames `Names.difference` pathPrefixed currentPathNames0
          rootNames = Branch.toNames0 root0
          pathPrefixed = case Path.unabsolute currentPath' of
            Path.Path (toList -> []) -> id
            p -> Names.prefix0 (Path.toName p)

      ppe0 = PPE.fromNames0 prettyPrintNames0
      withFile ambient sourceName text k = do
        let names = Names.names0ToNames parseNames0
        Result notes r <- eval $ Typecheck ambient names sourceName text
        case r of
          -- Parsing failed
          Nothing -> respond $
            ParseErrors text [ err | Result.Parsing err <- toList notes ]
          Just (names, r) -> case r of
            Nothing -> respond $
              TypeErrors text names [ err | Result.TypeError err <- toList notes ]
            Just r -> k names r
  case e of
    Left (IncomingRootBranch _names) ->
      error $ "todo: notify user about externally deposited head, and offer\n"
           ++ "a command to undo the merge that is about to happen.  In the\n"
           ++ "mean time until this is implemented, you can fix the issue by\n"
           ++ "deleting one of the heads from `.unison/v0/branches/head/`."

    Left (UnisonFileChanged sourceName text) ->
      -- We skip this update if it was programmatically generated
      if maybe False snd latestFile'
        then modifying latestFile (fmap (const False) <$>)
        else do
          eval (Notify $ FileChangeEvent sourceName text)
          withFile [] sourceName text $ \errorEnv unisonFile -> do
            let sr = toSlurpResult unisonFile (toSlurpResultNames unisonFile)
            eval (Notify $ Typechecked sourceName errorEnv sr unisonFile)
            (bindings, e) <- eval . Evaluate $ unisonFile
            let e' = Map.map go e
                go (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
            eval . Notify $ Evaluated text
              (PPE.unionLeft errorEnv $ PPE.fromNames0 prettyPrintNames0)
              bindings
              e'
            latestFile .= Just (Text.unpack sourceName, False)
            latestTypecheckedFile .= Just unisonFile
    Right input ->
      let
        ifConfirmed = ifM (confirmedCommand input)
        ifNotConfirmed = flip ifConfirmed
        branchNotFound = respond . BranchNotFound input
        branchNotFound' = respond . BranchNotFound input . Path.unsplit'
        typeNotFound = respond . TypeNotFound input
        termNotFound = respond . TermNotFound input
        typeConflicted src = respond . TypeAmbiguous input src
        termConflicted src = respond . TermAmbiguous input src
        branchExists dest _x = respond $ BranchAlreadyExists input dest
        branchExistsSplit = branchExists . Path.unsplit'
        typeExists dest = respond . TypeAlreadyExists input dest
        termExists dest = respond . TermAlreadyExists input dest
      in case input of
      ForkLocalBranchI src0 dest0 -> do
        let [src, dest] = Path.toAbsolutePath currentPath' <$> [src0, dest0]
        srcb <- getAt src
        if Branch.isEmpty srcb then branchNotFound src0
        else do
          ok <- updateAtM dest $ \destb ->
            pure (if Branch.isEmpty destb then srcb else destb)
          if ok then success else respond $ BadDestinationBranch input dest0

      MergeLocalBranchI src0 dest0 -> do
        let [src, dest] = Path.toAbsolutePath currentPath' <$> [src0, dest0]
        srcb <- getAt src
        if Branch.isEmpty srcb then branchNotFound src0
        else do
          _ <- updateAtM dest $ \destb -> eval . Eval $ Branch.merge srcb destb
          success

      MoveBranchI src dest ->
        maybe (branchNotFound' src) srcOk (getAtSplit' src)
        where
        srcOk b = maybe (destOk b) (branchExistsSplit dest) (getAtSplit' dest)
        destOk b = do
          stepManyAt
            [ BranchUtil.makeSetBranch (resolveSplit' src) Branch.empty
            , BranchUtil.makeSetBranch (resolveSplit' dest) b ]
          success -- could give rando stats about new defns

      DeleteBranchI p ->
        maybe (branchNotFound' p) go $ getAtSplit' p
        where
        go (Branch.head -> b) = do
          let rootNames = Branch.toNames0 root0
              toDelete = Names.prefix0 (Path.toName . Path.unsplit $ p') (Branch.toNames0 b)
                where p' = resolveSplit' p
          (failed, failedDependents) <- getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then
            stepAt $ BranchUtil.makeSetBranch (resolveSplit' p) Branch.empty
          else do
            failed <- loadSearchResults $ Names.asSearchResults failed
            failedDependents <- loadSearchResults $ Names.asSearchResults failedDependents
            respond $ CantDelete input rootNames failed failedDependents

      SwitchBranchI path' -> do
        path <- use $ currentPath . to (`Path.toAbsolutePath` path')
        currentPath .= path
        branch' <- getAt path
        when (Branch.isEmpty branch') (respond $ CreatedNewBranch path)

      UndoI -> do
        prev <- eval . Eval $ Branch.uncons root'
        case prev of
          Nothing ->
            respond . CantUndo $ if Branch.isOne root' then CantUndoPastStart
                                 else CantUndoPastMerge
          Just (_, prev) -> do
            root .= prev
            eval $ SyncLocalRootBranch prev
            success

      AliasTermI src dest -> case (toList (getHQTerms src), toList (getTerms dest)) of
        ([r],       []) -> do
          stepAt (BranchUtil.makeAddTermName (resolveSplit' dest) r (oldMD r))
          success
        ([r], rs@(_:_)) -> termExists dest (Set.fromList rs)
        ([],         _) -> termNotFound src
        (rs,         _) -> termConflicted src (Set.fromList rs)
        where
        p = resolveSplit' src
        oldMD r = BranchUtil.getTermMetadataAt p r root0

      AliasTypeI src dest -> case (toList (getHQTypes src), toList (getTypes dest)) of
        ([r],       []) -> do
          stepAt (BranchUtil.makeAddTypeName (resolveSplit' dest) r (oldMD r))
          success
        ([r], rs@(_:_)) -> typeExists dest (Set.fromList rs)
        ([],         _) -> typeNotFound src
        (rs,         _) -> typeConflicted src (Set.fromList rs)
        where
        p = resolveSplit' src
        oldMD r = BranchUtil.getTypeMetadataAt p r root0

      LinkI src mdValue -> do
        let srcle = toList (getHQ'Terms src)
            srclt = toList (getHQ'Types src)
            (parent, last) = resolveSplit' src
            mdValuel = toList (getHQTerms mdValue)
        case (srcle, srclt, mdValuel) of
          (srcle, srclt, [Referent.Ref mdValue])
            | length srcle < 2 && length srclt < 2 -> do
              mdType <- eval $ LoadTypeOfTerm mdValue
              case mdType of
                Nothing -> respond $ LinkFailure input
                Just ty -> do
                  stepAt (parent, step (Type.toReference ty))
                  success
                where
                step mdType b0 = let
                  tmUpdates terms = foldl' go terms srcle where
                    go terms src = Metadata.insert (src, mdType, mdValue) terms
                  tyUpdates types = foldl' go types srclt where
                    go types src = Metadata.insert (src, mdType, mdValue) types
                  in over Branch.terms tmUpdates . over Branch.types tyUpdates $ b0
          _ -> respond $ LinkFailure input

      UnlinkI src mdValue -> do
        let srcle = toList (getHQ'Terms src)
            srclt = toList (getHQ'Types src)
            (parent, last) = resolveSplit' src
            mdValuel = toList (getHQTerms mdValue)
        case (srcle, srclt, mdValuel) of
          (srcle, srclt, [Referent.Ref mdValue])
            | length srcle < 2 && length srclt < 2 -> do
              mdType <- eval $ LoadTypeOfTerm mdValue
              case mdType of
                Nothing -> respond $ LinkFailure input
                Just ty -> do
                  stepAt (parent, step (Type.toReference ty))
                  success
                where
                step mdType b0 = let
                  tmUpdates terms = foldl' go terms srcle where
                    go terms src = Metadata.delete (src, mdType, mdValue) terms
                  tyUpdates types = foldl' go types srclt where
                    go types src = Metadata.delete (src, mdType, mdValue) types
                  in over Branch.terms tmUpdates . over Branch.types tyUpdates $ b0
          _ -> respond $ LinkFailure input

      LinksI src mdTypeStr -> do
        let srcle = toList (getHQ'Terms src)
            srclt = toList (getHQ'Types src)
            p@(parent, last) = resolveSplit' src
            mdTerms = foldl' Metadata.merge mempty [
              BranchUtil.getTermMetadataUnder p r root0 | r <- srcle ]
            mdTypes = foldl' Metadata.merge mempty [
              BranchUtil.getTypeMetadataUnder p r root0 | r <- srclt ]
            allMd = Metadata.merge mdTerms mdTypes
            allowed = case mdTypeStr of
              Just str -> case Parsers.parseType str mempty of
                Left e -> Left e
                Right ty0 -> let
                  ty = Type.bindBuiltins' parseNames0 ty0
                  in Right $ Set.singleton (Type.toReference ty)
              Nothing -> Right (Map.keysSet allMd)
        case allowed of
          Left e -> respond $ ParseErrors (Text.pack (fromMaybe "" mdTypeStr)) [e]
          Right allowed -> do
            let allMd' = Map.restrictKeys allMd allowed
                allRefs = toList (Set.unions (Map.elems allMd'))
                ppe = PPE.fromNames0 prettyPrintNames0
            termDisplays <- Map.fromList <$> do
              terms <- filterM (eval . IsTerm) allRefs
              traverse (\r -> (r,) <$> loadTermDisplayThing r) terms
            typeDisplays <- Map.fromList <$> do
              types <- filterM (eval . IsType) allRefs
              traverse (\r -> (r,) <$> loadTypeDisplayThing r) types
            respond $ DisplayLinks ppe allMd' typeDisplays termDisplays

      MoveTermI src'@(fmap HQ'.toHQ -> src) dest ->
        case (toList (getHQTerms src), toList (getTerms dest)) of
          ([r], []) -> do
            stepManyAt
              [ BranchUtil.makeDeleteTermName p r
              , BranchUtil.makeAddTermName (resolveSplit' dest) r (mdSrc r)]
            success
          ([_], rs) -> termExists dest (Set.fromList rs)
          ([],   _) -> termNotFound src
          (rs,   _) -> termConflicted src (Set.fromList rs)
        where p = resolveSplit' (HQ'.toName <$> src')
              mdSrc r = BranchUtil.getTermMetadataAt p r root0

      MoveTypeI src'@(fmap HQ'.toHQ -> src) dest ->
        case (toList (getHQTypes src), toList (getTypes dest)) of
          ([r], []) -> do
            stepManyAt
              [ BranchUtil.makeDeleteTypeName p r
              , BranchUtil.makeAddTypeName (resolveSplit' dest) r (mdSrc r) ]
            success
          ([_], rs) -> typeExists dest (Set.fromList rs)
          ([], _)   -> typeNotFound src
          (rs, _)   -> typeConflicted src (Set.fromList rs)
        where
        p = resolveSplit' (HQ'.toName <$> src')
        mdSrc r = BranchUtil.getTypeMetadataAt p r root0

      DeleteTypeI hq'@(fmap HQ'.toHQ -> hq) -> case toList (getHQTypes hq) of
        [] -> typeNotFound hq
        [r] -> goMany (Set.singleton r)
        (Set.fromList -> rs) -> ifConfirmed (goMany rs) (typeConflicted hq rs)
        where
        resolvedPath = resolveSplit' (HQ'.toName <$> hq')
        makeDelete = BranchUtil.makeDeleteTypeName resolvedPath
        goMany rs = do
          let rootNames = Branch.toNames0 root0
              toDelete = Names.fromTypes ((name,) <$> toList rs)
                where name = Path.toName . Path.unsplit $ resolvedPath
          (failed, failedDependents) <- getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then stepManyAt . fmap makeDelete . toList $ rs
          else do
            failed <- loadSearchResults $ Names.asSearchResults failed
            failedDependents <- loadSearchResults $ Names.asSearchResults failedDependents
            respond $ CantDelete input rootNames failed failedDependents

      -- like the previous
      DeleteTermI hq'@(fmap HQ'.toHQ -> hq) -> case toList (getHQTerms hq) of
        [] -> termNotFound hq
        [r] -> goMany (Set.singleton r)
        (Set.fromList -> rs) -> ifConfirmed (goMany rs) (termConflicted hq rs)
        where
        resolvedPath = resolveSplit' (HQ'.toName <$> hq')
        makeDelete = BranchUtil.makeDeleteTermName resolvedPath
        goMany rs = do
          let rootNames, toDelete :: Names0
              rootNames = Branch.toNames0 root0
              toDelete = Names.fromTerms ((name,) <$> toList rs)
                where name = Path.toName . Path.unsplit $ resolvedPath
          (failed, failedDependents) <- getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then stepManyAt . fmap makeDelete . toList $ rs
          else do
            failed <- loadSearchResults $ Names.asSearchResults failed
            failedDependents <- loadSearchResults $ Names.asSearchResults failedDependents
            respond $ CantDelete input rootNames failed failedDependents

      -- todo: this should probably be able to show definitions by Path.HQSplit'
      ShowDefinitionI outputLoc (fmap HQ.fromString -> hqs) -> do
        let results = searchBranchExact currentBranch' hqs
            queryNames = Names terms types where
              -- todo: wouldn't need guard if SR used a Name or a HQ'
              toName = fromJust . HQ.toName
              terms = R.fromList [ (toName hq, r) | SR.Tm' hq r _as <- results, HQ.hasName hq ]
              types = R.fromList [ (toName hq, r) | SR.Tp' hq r _as <- results, HQ.hasName hq ]
        results' <- loadSearchResults results
        let termTypes :: Map.Map Reference (Type v Ann)
            termTypes =
              Map.fromList
                [ (r, t) | Output.Tm _ (Just t) (Referent.Ref r) _ <- results' ]
            (collatedTypes, collatedTerms) = collateReferences
              (mapMaybe Output.tpReference results')
              (mapMaybe Output.tmReferent results')
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

        -- We might like to make sure that the user search terms get used as
        -- the names in the pretty-printer, but the current implementation
        -- doesn't.
        let
          -- The definitions will generally reference names outside the current
          -- path.  For now we'll assume the pretty-printer will factor out
          -- excess name prefixes.
          rootNames, currentBranchNames :: Names0
          rootNames = Names.prefix0 (Name.Name "") $ Branch.toNames0 root0
          currentBranchNames = Branch.toNames0 currentBranch0
          ppe = PPE.fromNames0 $ queryNames
                                   `Names.unionLeft` currentBranchNames
                                   `Names.unionLeft` rootNames

          loc = case outputLoc of
            ConsoleLocation    -> Nothing
            FileLocation path  -> Just path
            LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
        do
          eval . Notify $ DisplayDefinitions loc ppe loadedTypes loadedTerms
          -- We set latestFile to be programmatically generated, if we
          -- are viewing these definitions to a file - this will skip the
          -- next update for that file (which will happen immediately)
          latestFile .= ((, True) <$> loc)
      FindPatchI ->
        let patches = Set.fromList
              [ Path.toName $ Path.snoc p seg
              | (p, b) <- Branch.toList0 currentBranch0
              , (seg, (_h, mp)) <- Map.toList (Branch._edits b) ]
        in respond $ ListOfPatches patches
      -- ls with no arguments
      SearchByNameI [] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results
          >>= respond . ListOfDefinitions prettyPrintNames0 False
      SearchByNameI ["-l"] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results
          >>= respond . ListOfDefinitions prettyPrintNames0 True
      -- ls with arguments
      SearchByNameI ("-l" : (fmap HQ.fromString -> qs)) -> do
        let results = uniqueBy SR.toReferent
                    $ searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results
          >>= respond . ListOfDefinitions prettyPrintNames0 True
      SearchByNameI (map HQ.fromString -> qs) -> do
        let results = searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results
          >>= respond . ListOfDefinitions prettyPrintNames0 False
      ResolveTypeNameI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTypes hq) (typeNotFound hq) go (typeConflicted hq)
        where
        conflicted = getHQTypes (fmap HQ'.toNameOnlyHQ hq')
        makeDelete =
          BranchUtil.makeDeleteTypeName (resolveSplit' (HQ'.toName <$> hq'))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      ResolveTermNameI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTerms hq) (termNotFound hq) go (termConflicted hq)
        where
        conflicted = getHQTerms (fmap HQ'.toNameOnlyHQ hq')
        makeDelete =
          BranchUtil.makeDeleteTermName (resolveSplit' (HQ'.toName <$> hq'))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      AddI hqs -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf -> do
          let result = Slurp.disallowUpdates
                     . applySelection hqs uf
                     $ toSlurpResult uf (toSlurpResultNames uf)
          when (Slurp.isNonempty result) $ do
            stepAt ( Path.unabsolute currentPath'
                   , doSlurpAdds (Slurp.adds result) uf)
            eval . AddDefsToCodebase . filterBySlurpResult result $ uf
          let fileNames0 = UF.typecheckedToNames0 uf
          let ppe = PPE.fromNames0 $ Names.unionLeft fileNames0 prettyPrintNames0
          respond $ SlurpOutput input ppe result

      UpdateI (Path.toAbsoluteSplit currentPath' -> (p,seg)) hqs -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf -> do
          let result = applySelection hqs uf
                     $ toSlurpResult uf (toSlurpResultNames uf)
          let fileNames0 = UF.typecheckedToNames0 uf
              -- todo: display some error if typeEdits or termEdits itself contains a loop
              typeEdits :: Map Name (Reference, Reference)
              typeEdits = Map.fromList $ map f (toList $ SC.types (updates result)) where
                f v = case (toList (Names.typesNamed parseNames0 n)
                           ,toList (Names.typesNamed fileNames0 n)) of
                  ([old],[new]) -> (n, (old, new))
                  _ -> error $ "Expected unique matches for "
                                  ++ Var.nameStr v ++ " but got: "
                                  ++ show otherwise
                  where n = Name.fromVar v
              hashTerms :: Map Reference (Type v Ann)
              hashTerms = Map.fromList (toList hashTerms0) where
                hashTerms0 = (\(r, _, typ) -> (r, typ)) <$> UF.hashTerms uf
              termEdits :: Map Name (Reference, Reference)
              termEdits = Map.fromList $ map g (toList $ SC.terms (updates result)) where
                g v = case ( toList (Names.refTermsNamed parseNames0 n)
                           , toList (Names.refTermsNamed fileNames0 n)) of
                  ([old], [new]) -> (n, (old, new))
                  _ -> error $ "Expected unique matches for "
                                 ++ Var.nameStr v ++ " but got: "
                                 ++ show otherwise
                  where n = Name.fromVar v
          ye'ol'Patch <- do
            b <- getAt p
            eval . Eval $ Branch.getPatch seg (Branch.head b)
          -- If `uf` updates a -> a', we want to replace all (a0 -> a) in patch
          -- with (a0 -> a') in patch'.
          -- So for all (a0 -> a) in patch, for all (a -> a') in `uf`,
          -- we must know the type of a0, a, a'.
          let
            -- we need:
            -- all of the `old` references from the `new` edits,
            -- plus all of the `old` references for edits from patch we're replacing
            collectOldForTyping :: [(Reference, Reference)] -> Patch -> Set Reference
            collectOldForTyping new old = foldl' f mempty (new ++ fromOld) where
              f acc (r, _r') = Set.insert r acc
              newLHS = Set.fromList . fmap fst $ new
              fromOld :: [(Reference, Reference)]
              fromOld = [ (r,r') | (r, TermEdit.Replace r' _) <- R.toList . Patch._termEdits $ old
                                 , Set.member r' newLHS ]
            neededTypes = collectOldForTyping (toList termEdits) ye'ol'Patch

          allTypes :: Map Reference (Type v Ann) <-
            fmap Map.fromList . for (toList neededTypes) $ \r ->
              (r,) . fromJust <$> (eval . LoadTypeOfTerm) r

          let typing r1 r2 = case (Map.lookup r1 allTypes, Map.lookup r2 hashTerms) of
                (Just t1, Just t2)
                  | Typechecker.isEqual t1 t2 -> TermEdit.Same
                  | Typechecker.isSubtype t1 t2 -> TermEdit.Subtype
                  | otherwise -> TermEdit.Different
                e -> error $ "compiler bug: typing map not constructed properly\n" <>
                  "typing " <> show r1 <> " " <> show r2 <> " : " <> show e

          let updatePatch :: Patch -> Patch
              updatePatch p = foldl' step2 (foldl' step1 p typeEdits) termEdits
                where
                step1 p (r,r') = Patch.updateType r (TypeEdit.Replace r') p
                step2 p (r,r') = Patch.updateTerm typing r (TermEdit.Replace r' (typing r r')) p
              updateEdits :: Branch0 m -> m (Branch0 m)
              updateEdits = Branch.modifyEdits seg updatePatch

          when (Slurp.isNonempty result) $ do
          -- take a look at the `updates` from the SlurpResult
          -- and make a patch diff to record a replacement from the old to new references
            stepManyAtM
              [( Path.unabsolute currentPath'
               , pure . doSlurpUpdates typeEdits termEdits)
              ,( Path.unabsolute currentPath'
               , pure . doSlurpAdds (Slurp.adds result) uf)
              ,( Path.unabsolute p, updateEdits )]
            eval . AddDefsToCodebase . filterBySlurpResult result $ uf
          let ppe = PPE.fromNames0 $ fileNames0 `Names.unionLeft` prettyPrintNames0
          respond $ SlurpOutput input ppe result

      TodoI editPath' branchPath' -> do
        patch <- getPatchAt editPath'
        branch <- getAt $ Path.toAbsolutePath currentPath' branchPath'
        -- checkTodo only needs the local names
        let names0 = (Branch.toNames0 . Branch.head) branch
        checkTodo patch names0 >>= respond . TodoOutput names0

      TestI showOk showFail -> do
        let
          testTerms = Star3.fact . Star3.select1D3 isTest
                    . Branch.deepTerms $ currentBranch0
          testRefs = Set.fromList [ r | Referent.Ref r <- toList testTerms ]
          oks results =
            [ (r, msg)
            | (r, Term.Sequence' ts) <- Map.toList results
            , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
            , cid == DD.okConstructorId && ref == DD.testResultRef ]
          fails results =
            [ (r, msg)
            | (r, Term.Sequence' ts) <- Map.toList results
            , Term.App' (Term.Constructor' ref cid) (Term.Text' msg) <- toList ts
            , cid == DD.failConstructorId && ref == DD.testResultRef ]
        cachedTests <- fmap Map.fromList . eval $ LoadWatches UF.TestWatch testRefs
        let stats = Output.CachedTests (Set.size testRefs) (Map.size cachedTests)
        respond $ TestResults stats ppe0 showOk showFail
                    (oks cachedTests) (fails cachedTests)
        let toCompute = Set.difference testRefs (Map.keysSet cachedTests)
        when (not . Set.null $ toCompute) $ do
          let total = Set.size toCompute
          computedTests <- fmap join . for (toList toCompute `zip` [1..]) $ \(r,n) -> do
            case r of
              Reference.DerivedId rid -> do
                tm <- eval $ LoadTerm rid
                case tm of
                  Nothing -> [] <$ respond (TermNotFound' input rid)
                  Just tm -> do
                    respond $ TestIncrementalOutputStart ppe0 (n,total) r tm
                    tm' <- eval $ Evaluate1 tm
                    eval $ PutWatch UF.TestWatch rid tm'
                    respond $ TestIncrementalOutputEnd ppe0 (n,total) r tm'
                    pure [(r, tm')]
              r -> error $ "unpossible, tests can't be builtins: " <> show r
          let m = Map.fromList computedTests
          respond $ TestResults Output.NewlyComputed ppe0 showOk showFail (oks m) (fails m)

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
      PatchI patchPath scopePath -> do
        patch <- getPatchAt patchPath
        changed <- updateAtM (resolveToAbsolute scopePath)
                          (propagate (PPE.fromNames0 absoluteRootNames0) patch)
        if changed then do
          branch <- getAt $ Path.toAbsolutePath currentPath' scopePath
          -- checkTodo only needs the local names
          let names0 = (Branch.toNames0 . Branch.head) branch
          checkTodo patch names0 >>= respond . TodoOutput names0
        else respond $ NothingToPatch patchPath scopePath
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
      ListEditsI (Path.toAbsoluteSplit currentPath' -> (p,seg)) -> do
        patch <- eval . Eval . Branch.getPatch seg . Branch.head =<< getAt p
        names0 <- eval . Eval $
          Branch.findRefsInHistory (Patch.allReferences patch) currentBranch'
        respond $ ListEdits patch names0
      PullRemoteBranchI repo path -> do
        loadRemoteBranchAt repo $ Path.toAbsolutePath currentPath' path
        success
      PushRemoteBranchI repo path -> do
        b <- getAt $ Path.toAbsolutePath currentPath' path
        e <- eval $ SyncRemoteRootBranch repo b
        either (eval . Notify . GitError) pure e
        success
      QuitI -> MaybeT $ pure Nothing
      _ -> error $ "todo: " <> show input
     where
      success = respond $ Success input
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
  -}

checkTodo :: Patch -> Names0 -> Action m i v (TodoOutput v Ann)
checkTodo patch names0 = do
  f <- computeFrontier (eval . GetDependents) patch names0
  let dirty = R.dom f
      frontier = R.ran f
      ppe = PPE.fromNames0 names0
  (frontierTerms, frontierTypes) <- loadDisplayInfo frontier
  (dirtyTerms, dirtyTypes) <- loadDisplayInfo dirty
  -- todo: something more intelligent here?
  let scoreFn = const 1
  remainingTransitive <- frontierTransitiveDependents (eval . GetDependents) names0 frontier
  let
    addTermNames terms = [(PPE.termName ppe (Referent.Ref r), r, t) | (r,t) <- terms ]
    addTypeNames types = [(PPE.typeName ppe r, r, d) | (r,d) <- types ]
    frontierTermsNamed = addTermNames frontierTerms
    frontierTypesNamed = addTypeNames frontierTypes
    dirtyTermsNamed = List.sortOn (\(s,_,_,_) -> s)
      [ (scoreFn r, n, r, t) | (n,r,t) <- addTermNames dirtyTerms ]
    dirtyTypesNamed = List.sortOn (\(s,_,_,_) -> s)
      [ (scoreFn r, n, r, t) | (n,r,t) <- addTypeNames dirtyTypes ]
  pure $
    TodoOutput_
      (Set.size remainingTransitive)
      (frontierTermsNamed, frontierTypesNamed)
      (dirtyTermsNamed, dirtyTypesNamed)
      (Names.conflicts names0)
      (Patch.conflicts patch)
  where
  frontierTransitiveDependents ::
    Monad m => (Reference -> m (Set Reference)) -> Names0 -> Set Reference -> m (Set Reference)
  frontierTransitiveDependents dependents names0 rs = do
    let branchDependents r = Set.filter (Names.contains names0) <$> dependents r
    tdeps <- transitiveClosure branchDependents rs
    -- we don't want the frontier in the result
    pure $ tdeps `Set.difference` rs

-- (d, f) when d is "dirty" (needs update),
--             f is in the frontier (an edited dependency of d),
--         and d depends on f
-- a ⋖ b = a depends directly on b
-- dirty(d) ∧ frontier(f) <=> not(edited(d)) ∧ edited(f) ∧ d ⋖ f
--
-- The range of this relation is the frontier, and the domain is
-- the set of dirty references.
computeFrontier :: forall m . Monad m
         => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
         -> Patch
         -> Names0
         -> m (R.Relation Reference Reference)
computeFrontier getDependents patch names = let
  edited :: Set Reference
  edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)
  addDependents :: R.Relation Reference Reference -> Reference -> m (R.Relation Reference Reference)
  addDependents dependents ref =
    (\ds -> R.insertManyDom ds ref dependents) . Set.filter (Names.contains names)
      <$> getDependents ref
  in do
    -- (r,r2) ∈ dependsOn if r depends on r2
    dependsOn <- foldM addDependents R.empty edited
    -- Dirty is everything that `dependsOn` Frontier, minus already edited defns
    pure $ R.filterDom (not . flip Set.member edited) dependsOn

eval :: Command m i v a -> Action m i v a
eval = lift . lift . Free.eval

confirmedCommand :: Input -> Action m i v Bool
confirmedCommand i = do
  i0 <- use lastInput
  pure $ Just i == i0

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
      subnames = Branch.toNames0 . Branch.head $
                   Branch.getAt' (Path.singleton last) b
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
      HQ.HashQualified qn h | h `SH.isPrefixOf` Referent.toShortHash ref ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` Referent.toShortHash ref ->
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
      HQ.HashQualified qn h | h `SH.isPrefixOf` Reference.toShortHash ref ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` Reference.toShortHash ref ->
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
    -- construct a search result with appropriately hash-qualified version of the query
    -- for each (n,r) see if it matches a query.  If so, get appropriately hash-qualified version.
    [ SR.typeResult (Names.hqTypeName names0 name r) r
                    (Names.hqTypeAliases names0 name r)
    | (name, r) <- R.toList $ Names.types names0
    , any (matchesHashPrefix Reference.toShortHash (name, r)) queries
    ]
  filteredTerms =
    [ SR.termResult (Names.hqTermName names0 name r) r
                    (Names.hqTermAliases names0 name r)
    | (name, r) <- R.toList $ Names.terms names0
    , any (matchesHashPrefix Referent.toShortHash (name, r)) queries
    ]
  deduped = uniqueBy SR.toReferent (filteredTypes <> filteredTerms)
  doTerm (n, r) =
    if Set.size (R.lookupDom n (Names.terms names0)) > 1
    then (HQ.take length $ HQ.fromNamedReferent n r, r)
    else (HQ.NameOnly n, r)
  doType (n, r) =
    if Set.size (R.lookupDom n (Names.types names0)) > 1
    then (HQ.take length $ HQ.fromNamedReference n r, r)
    else (HQ.NameOnly n, r)
  length = Names.numHashChars names0
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
--     Branch.prettyPrintEnv &&& loadSearchResults . pickResults known
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

loadRemoteBranchAt
  :: Applicative m => RemoteRepo -> Path.Absolute -> Action m i v ()
loadRemoteBranchAt repo p = do
  b <- eval (LoadRemoteRootBranch repo)
  either (eval . Notify . GitError) (void . updateAtM p . const . pure) b

getAt :: Functor m => Path.Absolute -> Action m i v (Branch m)
getAt (Path.Absolute p) =
  use root <&> fromMaybe Branch.empty . Branch.getAt p

-- Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM :: Applicative m
          => Path.Absolute
          -> (Branch m -> Action m i v (Branch m))
          -> Action m i v Bool
updateAtM (Path.Absolute p) f = do
  b <- use root
  b' <- Branch.modifyAtM p f b
  root .= b'
  when (b /= b') $ eval $ SyncLocalRootBranch b'
  pure $ b /= b'

stepAt :: forall m i v. Applicative m
       => (Path, Branch0 m -> Branch0 m)
       -> Action m i v ()
stepAt = stepManyAt @m @[] . pure

stepAtM :: forall m i v. Monad m
        => (Path, Branch0 m -> m (Branch0 m))
        -> Action m i v ()
stepAtM = stepManyAtM @m @[] . pure

stepManyAt :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m)
           -> Action m i v ()
stepManyAt actions = do
    b <- use root
    let b' = Branch.stepManyAt actions b
    root .= b'
    when (b /= b') $ eval $ SyncLocalRootBranch b'

stepManyAtM :: (Monad m, Foldable f)
           => f (Path, Branch0 m -> m (Branch0 m))
           -> Action m i v ()
stepManyAtM actions = do
    b <- use root
    b' <- eval . Eval $ Branch.stepManyAtM actions b
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
      if d `Set.member` namedRefsRemaining
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
      if d `Set.member` namedRefsRemaining
      then (Names.addType name r failed, addDependent d failedDeps)
      else (failed, failedDeps)
  namedRefsRemaining :: Set Reference
  namedRefsRemaining = Set.map Referent.toReference (Names.termReferents remaining)
                <> Names.typeReferences remaining
    where remaining = root `Names.difference` toBeDeleted

-- Applies the selection filter to the adds/updates of a slurp result,
-- meaning that adds/updates should only contain the selection or its transitive
-- dependencies, any unselected transitive dependencies of the selection will
-- be added to `extraDefinitions`.
applySelection :: forall v a. Var v =>
  [HashQualified] -> UF.TypecheckedUnisonFile v a -> SlurpResult v -> SlurpResult v
applySelection [] _ = id
applySelection hqs file = \sr@SlurpResult{..} ->
  sr { adds = adds `SC.intersection` closed
     , updates = updates `SC.intersection` closed
     , extraDefinitions = closed `SC.difference` selection
     }
  where
  selectedNames0 =
    Names.filterByHQs (Set.fromList hqs) (UF.typecheckedToNames0 file)
  selection, closed :: SlurpComponent v
  selection = SlurpComponent selectedTypes selectedTerms
  closed = SC.closeWithDependencies file selection
  selectedTypes, selectedTerms :: Set v
  selectedTypes = Set.map var $ R.dom (Names.types selectedNames0)
  selectedTerms = Set.map var $ R.dom (Names.types selectedNames0)

var :: Var v => Name -> v
var name = Var.named (Name.toText name)

toSlurpResult :: forall v. Var v => UF.TypecheckedUnisonFile v Ann -> Names0 -> SlurpResult v
toSlurpResult uf existingNames =
  Slurp.subtractComponent (conflicts <> ctorCollisions) $
  SlurpResult uf mempty adds dups mempty conflicts updates
              termCtorCollisions ctorTermCollisions termAliases typeAliases
              mempty
  where
  fileNames0 = UF.typecheckedToNames0 uf

  sc :: R.Relation Name Referent -> R.Relation Name Reference -> SlurpComponent v
  sc terms types = SlurpComponent { terms = Set.map var (R.dom terms)
                                  , types = Set.map var (R.dom types) }

  -- conflict (n,r) if n is conflicted in names0
  conflicts :: SlurpComponent v
  conflicts = sc terms types where
    terms = R.filterDom (conflicted . Names.termsNamed existingNames) (Names.terms fileNames0)
    types = R.filterDom (conflicted . Names.typesNamed existingNames) (Names.types fileNames0)
    conflicted s = Set.size s > 1

  ctorCollisions :: SlurpComponent v
  ctorCollisions =
    mempty { SC.terms = termCtorCollisions <> ctorTermCollisions }

  -- termCtorCollision (n,r) if (n, r' /= r) exists in existingNames and r is Ref and r' is Con
  termCtorCollisions :: Set v
  termCtorCollisions = Set.fromList
    [ var n | (n, r@Referent.Ref{}) <- R.toList (Names.terms fileNames0)
            , [r'@Referent.Con{}] <- [toList $ Names.termsNamed existingNames n]
            ]

  -- ctorTermCollisions (n,r) if (n, r' /= r) exists in names0 and r is Con and r' is Ref
  -- except we relaxed it to where r' can be Con or Ref
  -- what if (n,r) and (n,r' /= r) exists in names and r, r' are Con
  ctorTermCollisions :: Set v
  ctorTermCollisions = Set.fromList
    [ var n | (n, r@Referent.Con{}) <- R.toList (Names.terms fileNames0)
            , [r'] <- [toList $ Names.termsNamed existingNames n]
            , r /= r'
            ]

  -- duplicate (n,r) if (n,r) exists in names0
  dups :: SlurpComponent v
  dups = sc terms types where
    terms = R.intersection (Names.terms existingNames) (Names.terms fileNames0)
    types = R.intersection (Names.types existingNames) (Names.types fileNames0)

  -- update (n,r) if (n,r' /= r) exists in names0 and r, r' are Ref
  updates :: SlurpComponent v
  updates = SlurpComponent (Set.fromList types) (Set.fromList terms) where
    terms = [ var n | (n,r'@Referent.Ref{}) <- R.toList (Names.terms fileNames0)
                    , [r@Referent.Ref{}] <- [toList $ Names.termsNamed existingNames n]
                    , r' /= r ]
    types = [ var n | (n,r') <- R.toList (Names.types fileNames0)
                    , [r] <- [toList $ Names.typesNamed existingNames n]
                    , r' /= r ]

  -- alias (n, r) if (n' /= n, r) exists in names0
  termAliases :: Map v (Set Name)
  termAliases = Map.fromList
    [ (var n, aliases)
    | (n, r) <- R.toList $ Names.terms fileNames0
    , aliases <- [Set.delete n $ R.lookupRan r (Names.terms existingNames)]
    , not (null aliases)
    ]

  typeAliases :: Map v (Set Name)
  typeAliases = Map.fromList
    [ (var n, aliases)
    | (n, r) <- R.toList $ Names.types fileNames0
    , aliases <- [Set.delete n $ R.lookupRan r (Names.types existingNames)]
    , not (null aliases)
    ]

  -- add (n,r) if n doesn't exist and r doesn't exist in names0
  adds = sc terms types where
    terms = add (Names.terms existingNames) (Names.terms fileNames0)
    types = add (Names.types existingNames) (Names.types fileNames0)
    add :: Ord r => R.Relation Name r -> R.Relation Name r -> R.Relation Name r
    add existingNames = R.filter go where
      go (n, r) = (not . R.memberDom n) existingNames
               && (not . R.memberRan r) existingNames



filterBySlurpResult :: Ord v
           => SlurpResult v
           -> UF.TypecheckedUnisonFile v Ann
           -> UF.TypecheckedUnisonFile v Ann
filterBySlurpResult SlurpResult{..} UF.TypecheckedUnisonFile{..} =
  UF.TypecheckedUnisonFile datas effects tlcs watches
  where
  keep = updates <> adds
  keepTerms = SC.terms keep
  keepTypes = SC.types keep
  datas = Map.restrictKeys dataDeclarations' keepTypes
  effects = Map.restrictKeys effectDeclarations' keepTypes
  tlcs = filter (not.null) $ fmap (List.filter filterTLC) topLevelComponents'
  watches = filter (not.null.snd) $ fmap (second (List.filter filterTLC)) watchComponents
  filterTLC (v,_,_) = Set.member v keepTerms

-- updates the namespace for adding `slurp`
doSlurpAdds :: forall m v. (Applicative m, Var v)
            => SlurpComponent v
            -> UF.TypecheckedUnisonFile v Ann
            -> (Branch0 m -> Branch0 m)
doSlurpAdds slurp uf = Branch.stepManyAt0 (typeActions <> termActions)
  where
  typeActions = map doType . toList $ SC.types slurp
  termActions = map doTerm . toList $ SC.terms slurp
  names = UF.typecheckedToNames0 uf
  tests = Set.fromList $ fst <$> UF.watchesOfKind UF.TestWatch (UF.discardTypes uf)
  (isTestType, isTestValue) = isTest
  md v =
    if Set.member v tests then Metadata.singleton isTestType isTestValue
    else Metadata.empty
  doTerm :: v -> (Path, Branch0 m -> Branch0 m)
  doTerm v = case toList (Names.termsNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTermName split r (md v)
    wha -> error $ "Unison bug, typechecked file w/ multiple terms named "
                <> Var.nameStr v <> ": " <> show wha
  doType :: v -> (Path, Branch0 m -> Branch0 m)
  doType v = case toList (Names.typesNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTypeName split r Metadata.empty
    wha -> error $ "Unison bug, typechecked file w/ multiple types named "
                <> Var.nameStr v <> ": " <> show wha
  errorEmptyVar = error "encountered an empty var name"
  errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

doSlurpUpdates :: Applicative m
               => Map Name (Reference, Reference)
               -> Map Name (Reference, Reference)
               -> (Branch0 m -> Branch0 m)
doSlurpUpdates typeEdits termEdits b0 = Branch.stepManyAt0 (typeActions <> termActions) b0
  where
  typeActions = join . map doType . Map.toList $ typeEdits
  termActions = join . map doTerm . Map.toList $ termEdits
  -- we copy over the metadata on the old thing
  -- todo: if the thing being updated, m, is metadata for something x in b0
  -- update x's md to reference `m`
  doType, doTerm ::
    (Name, (Reference, Reference)) -> [(Path, Branch0 m -> Branch0 m)]
  doType (n, (old, new)) = case Path.splitFromName n of
    Nothing -> errorEmptyVar
    Just split -> [ BranchUtil.makeDeleteTypeName split old
                  , BranchUtil.makeAddTypeName split new oldMd ]
      where
      oldMd = BranchUtil.getTypeMetadataAt split old b0
  doTerm (n, (old, new)) = case Path.splitFromName n of
    Nothing -> errorEmptyVar
    Just split -> [ BranchUtil.makeDeleteTermName split (Referent.Ref old)
                  , BranchUtil.makeAddTermName split (Referent.Ref new) oldMd ]
      where
      oldMd = BranchUtil.getTermMetadataAt split (Referent.Ref old) b0
  errorEmptyVar = error "encountered an empty var name"

loadSearchResults :: Ord v => [SR.SearchResult] -> Action m i v [SearchResult' v Ann]
loadSearchResults = traverse loadSearchResult
  where
  loadSearchResult = \case
    SR.Tm (SR.TermResult name r aliases) -> do
      typ <- loadReferentType r
      pure $ Tm name typ r aliases
    SR.Tp (SR.TypeResult name r aliases) -> do
      dt <- loadTypeDisplayThing r
      pure $ Tp name dt r aliases

loadDisplayInfo ::
  Set Reference -> Action m i v ([(Reference, Maybe (Type v Ann))]
                                ,[(Reference, DisplayThing (DD.Decl v Ann))])
loadDisplayInfo refs = do
  termRefs <- filterM (eval . IsTerm) (toList refs)
  typeRefs <- filterM (eval . IsType) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> eval (LoadTypeOfTerm r)
  types <- forM typeRefs $ \r -> (r,) <$> loadTypeDisplayThing r
  pure (terms, types)

loadReferentType :: Referent -> _ (Maybe (Type _ _))
loadReferentType = \case
  Referent.Ref r -> eval $ LoadTypeOfTerm r
  Referent.Con r cid -> getTypeOfConstructor r cid
  where
  getTypeOfConstructor :: Reference -> Int -> Action m i v (Maybe (Type v Ann))
  getTypeOfConstructor (Reference.DerivedId r) cid = do
    maybeDecl <- eval $ LoadType r
    pure $ case maybeDecl of
      Nothing -> Nothing
      Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
  getTypeOfConstructor r cid =
    error $ "Don't know how to getTypeOfConstructor " ++ show r ++ " " ++ show cid

loadTermDisplayThing :: Ord v => Reference -> _ (DisplayThing (Term v _))
loadTermDisplayThing r = case r of
  Reference.Builtin _ -> pure BuiltinThing
  Reference.DerivedId id -> do
    tm <- eval (LoadTerm id)
    case tm of
      Nothing -> pure $ MissingThing id
      Just tm@(Term.Ann' _ _) -> pure $ RegularThing tm
      Just tm -> do
        ty <- eval $ LoadTypeOfTerm r
        case ty of
          Nothing -> pure $ MissingThing id
          Just ty -> pure $ RegularThing (Term.ann (ABT.annotation tm) tm ty)

loadTypeDisplayThing :: Reference -> _ (DisplayThing (DD.Decl _ _))
loadTypeDisplayThing = \case
  Reference.Builtin _ -> pure BuiltinThing
  Reference.DerivedId id ->
    maybe (MissingThing id) RegularThing <$> eval (LoadType id)

-- Description:
------------------
-- For any `Reference` in the frontier which has a type edit, do no propagation.
-- (for now, until we have a richer type edit algebra).
--
-- For any `Reference` in the frontier which has an unconflicted, type-preserving
-- term edit, `old -> new`, replace `old` with `new` in dependents of the
-- frontier, and call `propagate'` recursively on the new frontier.
--
-- If the term is `Typing.Same`, the dependents don't need to be typechecked.
-- If the term is `Typing.Subtype`, and the dependent only has inferred type,
-- it should be re-typechecked, and the new inferred type should be used.
--
-- This will create a whole bunch of new terms in the codebase and move the
-- names onto those new terms. Uses `Term.updateDependencies` to perform
-- the substitutions.
--
-- Algorithm:
----------------
-- compute the frontier relation (dependencies of updated terms)
-- for each dirty definition d:
--  for each member c of cycle(d):
--   construct c', an updated c incorporating all type-preserving edits
--   add an edit c -> c'
--   save c' to a `Map Reference Term`

-- propagate only deals with Terms
-- "dirty" means in need of update
-- "frontier" means updated definitions responsible for the "dirty"
-- errorPPE only needs to have external names, since we're using it to report
--   referents that are outside of the
propagate :: forall m v. (Monad m, Var v)
  => PPE.PrettyPrintEnv -> Patch -> Branch m -> Action' m v (Branch m)
propagate errorPPE patch b = validatePatch patch >>= \case
  Nothing -> do
    respond PatchNeedsToBeConflictFree
    pure b
  Just initialEdits -> do
    initialDirty <- R.dom <$> computeFrontier
                              (eval . GetDependents)
                              (typePreservingTermEdits patch)
                              names0
    missing :: Set Reference <-
      missingDependents
        initialDirty
        (Set.fromList . mapMaybe Referent.toTermReference . toList
                      . Branch.deepReferents . Branch.head $ b)
    if not $ Set.null missing then do
      respond (PatchInvolvesExternalDependents errorPPE missing)
      pure b
    else do
      order <- sortDependentsGraph initialDirty
      let getOrdered :: Set Reference -> Map Int Reference
          getOrdered rs =
            Map.fromList [ (i, r) | r <- toList rs, Just i <- [Map.lookup r order]]
          collectEdits :: (Monad m, Var v)
                       => Map Reference TermEdit
                       -> Map Reference Reference
                       -> Map Reference (Term v _, Type v _)
                       -> Set Reference
                       -> Map Int Reference
                       -> Action' m v (Map Reference TermEdit,
                                       Map Reference Reference,
                                       Map Reference (Term v _, Type v _))
          -- `replacements` contains the TermEdit.Replace elements of `edits`.
          collectEdits edits replacements newTerms seen todo = case Map.minView todo of
            Nothing -> pure (edits, replacements, newTerms)
            Just (r, todo) -> case r of
              Reference.Builtin _ -> collectEdits edits replacements newTerms seen todo
              Reference.DerivedId _ ->
                if Map.member r edits || Set.member r seen
                then collectEdits edits replacements newTerms seen todo
                else do
                  unhashComponent r >>= \case
                    Nothing -> collectEdits edits replacements newTerms (Set.insert r seen) todo
                    Just componentMap -> do
                      let componentMap' = over _2 (Term.updateDependencies replacements) <$> componentMap
                          hashedComponents' = Term.hashComponents (view _2 <$> componentMap')
                          joinedStuff :: [(Reference, Reference, Term v _, Type v _)]
                          joinedStuff = toList (Map.intersectionWith f componentMap' hashedComponents')
                          f (oldRef, _, oldType) (newRef, newTerm) = (oldRef, newRef, newTerm, newType)
                            where newType = oldType
                      -- collect the hashedComponents into edits/replacements/newterms/seen
                          edits' = edits <> (Map.fromList . fmap toEdit) joinedStuff
                          toEdit (r, r', _, _) = (r, TermEdit.Replace r' TermEdit.Same)
                          replacements' = replacements <> (Map.fromList . fmap toReplacement) joinedStuff
                          toReplacement (r, r', _, _) = (r, r')
                          newTerms' = newTerms <> (Map.fromList . fmap toNewTerm) joinedStuff
                          toNewTerm (_, r', tm, tp) = (r', (tm, tp))
                          seen' = seen <> Set.fromList (view _1 <$> joinedStuff)
                      -- plan to update the dependents of this component too
                      dependents <- fmap Set.unions . traverse (eval . GetDependents)
                                  . toList
                                  . Reference.members
                                  $ Reference.componentFor r
                      let todo' = todo <> getOrdered dependents
                      collectEdits edits' replacements' newTerms' seen' todo'
      (termEdits, _replacements, newTerms) <-
          collectEdits initialEdits
                       (Map.mapMaybe TermEdit.toReference initialEdits)
                       mempty -- newTerms
                       mempty -- skip
                       (getOrdered initialDirty)

      -- todo: can eliminate this filter if collectEdits doesn't leave temporary terms in the map!
      let termEditTargets = Set.fromList . catMaybes . fmap TermEdit.toReference
                          $ toList termEdits
      -- write the new terms to the codebase
      (writeTerms . Map.toList) (Map.restrictKeys newTerms termEditTargets)

      let deprecatedTerms, deprecatedTypes :: Set Reference
          deprecatedTerms =
            Set.fromList [r | (r, TermEdit.Deprecate) <- Map.toList termEdits]
          deprecatedTypes =
            Set.fromList [r | (r, TypeEdit.Deprecate) <-
                                              R.toList (Patch._typeEdits patch)]

      -- recursively update names and delete deprecated definitions
      pure $ Branch.step
          (Branch.stepEverywhere
            (updateNames (Map.mapMaybe TermEdit.toReference termEdits))
             . deleteDeprecatedTerms deprecatedTerms
             . deleteDeprecatedTypes deprecatedTypes) b
  where
  missingDependents :: Set Reference -> Set Reference -> _ (Set Reference)
  missingDependents dirty known = do
    closure <- transitiveClosure (eval . GetDependents) dirty
    pure $ Set.difference closure known
  sortDependentsGraph :: Set Reference -> _ (Map Reference Int)
  sortDependentsGraph rs = do
    closure <- transitiveClosure (eval . GetDependents) rs
    dependents <- traverse (\r -> (r,) <$> (eval . GetDependents) r) (toList closure)
    let graphEdges = [(r, r, toList deps) | (r, deps) <- toList dependents]
        (graph, getReference, _) = Graph.graphFromEdges graphEdges
    pure $ Map.fromList (zip (view _1 . getReference <$> Graph.topSort graph) [0..])
    -- vertex i precedes j whenever i has an edge to j and not vice versa.
    -- vertex i precedes j when j is a dependent of i.

  updateNames :: Map Reference Reference -> Branch0 m -> Branch0 m
  updateNames edits Branch0{..} = Branch.branch0 terms _types _children _edits
    where
    terms = foldl' replace _terms (Map.toList edits)
    replace s (r,r') = Star3.replaceFact (Referent.Ref r) (Referent.Ref r') s
  deleteDeprecatedTerms, deleteDeprecatedTypes ::
    Set Reference -> Branch0 m -> Branch0 m
  deleteDeprecatedTerms rs =
    over Branch.terms (Star3.deleteFact (Set.map Referent.Ref rs))
  deleteDeprecatedTypes rs = over Branch.types (Star3.deleteFact rs)
  typePreservingTermEdits :: Patch -> Patch
  typePreservingTermEdits Patch{..} = Patch termEdits mempty where
    termEdits = R.filterRan TermEdit.isTypePreserving _termEdits
  writeTerms = traverse_ (\(Reference.DerivedId id, (tm, tp)) -> eval $ PutTerm id tm tp)
  names0 = (Branch.toNames0 . Branch.head) b
  validatePatch :: Patch -> Action' m v (Maybe (Map Reference TermEdit))
  validatePatch p = pure $ R.toMap (Patch._termEdits p)

  -- Turns a cycle of references into a term with free vars that we can edit
  -- and hash again.
  -- todo: Maybe this an others can be moved to HandleCommand, in the
  --  Free (Command m i v) monad, passing in the actions that are needed.
  -- However, if we want this to be parametric in the annotation type, then
  -- Command would have to be made parametric in the annotation type too.
  unhashComponent
    :: forall m v . (Monad m, Var v)
    => Reference
    -> Action' m v (Maybe (Map v (Reference, Term v _, Type v _)))
  unhashComponent ref = do
    let component = Reference.members $ Reference.componentFor ref
    isTerm <- eval $ IsTerm ref
    isType <- eval $ IsType ref
    if isTerm then do
      let
        termInfo :: Reference -> Action' m v (v, (Reference, Term v Ann, Type v Ann))
        termInfo termRef = do
          tpm <- eval $ LoadTypeOfTerm termRef
          tp  <- maybe (fail $ "Missing type for term " <> show termRef) pure tpm
          case termRef of
            Reference.DerivedId id -> do
              mtm <- eval $ LoadTerm id
              tm <- maybe (fail $ "Missing term with id " <> show id) pure mtm
              pure (Var.typed (Var.RefNamed termRef), (termRef, tm, tp))
            _ -> fail $ "Cannot unhashComponent for a builtin: " ++ show termRef
        unhash m =
          let f (ref,_oldTm,oldTyp) (_ref,newTm) = (ref,newTm,oldTyp)
              dropType (r,tm,_tp) = (r,tm)
          in Map.intersectionWith f m (Term.unhashComponent (dropType <$> m))
      Just . unhash . Map.fromList <$> traverse termInfo (toList component)
    else if isType then pure Nothing
    else fail $ "Invalid reference: " <> show ref

--

--  typecheckTerms :: (Monad m, Var v, Ord a, Monoid a)
--                 => Codebase m v a
--                 -> [(v, Term v a)]
--                 -> m (Map v (Type v a))
--  typecheckTerms code bindings = do
--    let tm = Term.letRec' True bindings $ DD.unitTerm mempty
--    env <- typecheckingEnvironment' code tm
--    (o, notes) <- Result.runResultT $ Typechecker.synthesize env tm
--    -- todo: assert that the output map has a type for all variables in the input
--    case o of
--      Nothing -> fail $ "A typechecking error occurred - this indicates a bug in Unison"
--      Just _ -> pure $
--        Map.fromList [ (v, typ) | Context.TopLevelComponent c <- toList (Typechecker.infos notes)
--                                , (v, typ, _) <- c ]