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
                                                  liftM2, when, void)
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.State            ( StateT
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Bifunctor                 ( second )
import           Data.Foldable                  ( find
                                                , toList
                                                , foldl'
                                                )
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
                                                , Branch0
                                                )
import qualified Unison.Codebase.Branch2       as Branch
import qualified Unison.Codebase.BranchUtil    as BranchUtil
import           Unison.Codebase.Patch          ( Patch )
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
  let
      root0 = Branch.head root'
      currentBranch0 = Branch.head currentBranch'
      resolvePath' :: (Path', a) -> (Path, a)
      resolvePath' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      path'ToSplit :: Path' -> Maybe Path.Split
      path'ToSplit = Path.unsnoc . Path.unabsolute . Path.toAbsolutePath currentPath'
      resolveAsAbsolute = Path.fromAbsoluteSplit . Path.toAbsoluteSplit Path.absoluteEmpty
      getAtSplit :: Path.Split -> Maybe (Branch m)
      getAtSplit p = BranchUtil.getBranch p root0
      getAtSplit' :: Path.Split' -> Maybe (Branch m)
      getAtSplit' = getAtSplit . resolvePath'
      getHQTypes :: Path.HQSplit' -> Set Reference
      getHQTypes p = BranchUtil.getType (resolvePath' p) root0
      getHQTerms :: Path.HQSplit' -> Set Referent
      getHQTerms p = BranchUtil.getTerm (resolvePath' p) root0
      getHQ'Terms = getHQTerms . fmap HQ'.toHQ
      getTypes :: Path.Split' -> Set Reference
      getTypes = getHQTypes . fmap HQ.NameOnly
      getTerms :: Path.Split' -> Set Referent
      getTerms = getHQTerms . fmap HQ.NameOnly
  let -- names' = Branch.toNames (Branch.head currentBranch')
      names0' = Branch.toNames0 root0
  e           <- eval Input
  let withFile ambient sourceName text k = do
        let names = Names.names0ToNames $ names0'
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
            let sr = toSlurpResult unisonFile names0'
            eval (Notify $ Typechecked sourceName errorEnv sr unisonFile)
            (bindings, e) <- eval . Evaluate $ unisonFile
            let e' = Map.map go e
                go (ann, kind, _hash, _uneval, eval, isHit) = (ann, kind, eval, isHit)
            eval . Notify $ Evaluated text
              (PPE.unionLeft errorEnv $ PPE.fromNames0 names0')
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
            [ BranchUtil.makeSetBranch (resolvePath' src) Branch.empty
            , BranchUtil.makeSetBranch (resolvePath' dest) b ]
          success -- could give rando stats about new defns

      DeleteBranchI p ->
        maybe (branchNotFound' p) go $ getAtSplit' p
        where
        go (Branch.head -> b) = do
          let rootNames = Branch.toNames0 root0
              toDelete = Names.prefix0 (Path.toName . Path.unsplit $ p') (Branch.toNames0 b)
                where p' = resolvePath' p
          (failed, failedDependents) <- getEndangeredDependents (eval . GetDependents) toDelete rootNames
          if failed == mempty then
            stepAt $ BranchUtil.makeSetBranch (resolvePath' p) Branch.empty
          else do
            failed <- loadSearchResults $ Names.asSearchResults failed
            failedDependents <- loadSearchResults $ Names.asSearchResults failedDependents
            respond $ CantDelete input rootNames failed failedDependents

      SwitchBranchI path' -> do
        path <- use $ currentPath . to (`Path.toAbsolutePath` path')
        currentPath .= path
        branch' <- getAt path
        when (Branch.isEmpty $ branch') (respond $ CreatedNewBranch path)

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

      DeleteTypeI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTypes hq) (typeNotFound hq) (goMany . Set.singleton)
                      (liftM2 ifConfirmed goMany (typeConflicted hq))
        where
        resolvedPath = resolvePath' (HQ'.toName <$> hq')
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
      DeleteTermI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTerms hq) (termNotFound hq) (goMany . Set.singleton)
                      (liftM2 ifConfirmed goMany (termConflicted hq))
        where
        resolvedPath = resolvePath' (HQ'.toName <$> hq')
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
      -- ls with no arguments
      SearchByNameI [] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results >>= respond . ListOfDefinitions names0' False
      SearchByNameI ["-l"] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results >>= respond . ListOfDefinitions names0' True
      -- ls with arguments
      SearchByNameI ("-l" : (fmap HQ.fromString -> qs)) -> do
        let results = uniqueBy SR.toReferent
                    $ searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results >>= respond . ListOfDefinitions names0' True
      SearchByNameI (map HQ.fromString -> qs) -> do
        let results = searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        loadSearchResults results >>= respond . ListOfDefinitions names0' False
      ResolveTypeNameI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTypes hq) (typeNotFound hq) go (typeConflicted hq)
        where
        conflicted = getHQTypes (fmap HQ'.toNameOnlyHQ hq')
        makeDelete =
          BranchUtil.makeDeleteTypeName (resolvePath' (HQ'.toName <$> hq'))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      ResolveTermNameI hq'@(fmap HQ'.toHQ -> hq) ->
        zeroOneOrMore (getHQTerms hq) (termNotFound hq) go (termConflicted hq)
        where
        conflicted = getHQTerms (fmap HQ'.toNameOnlyHQ hq')
        makeDelete =
          BranchUtil.makeDeleteTermName (resolvePath' (HQ'.toName <$> hq'))
        go r = stepManyAt . fmap makeDelete . toList . Set.delete r $ conflicted

      AddI hqs -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf -> do
          let result = Slurp.disallowUpdates
                     . applySelection hqs uf
                     . toSlurpResult uf
                     . Branch.toNames0
                     . Branch.head
                     $ currentBranch'
          when (Slurp.isNonempty result) $ do
            stepAt ( Path.unabsolute currentPath'
                   , doSlurpAdds (Slurp.adds result) uf)
            eval . AddDefsToCodebase . filterBySlurpResult result $ uf
          let ppe1 = PPE.fromNames0 . UF.typecheckedToNames0 $ uf
              ppe2 = PPE.fromNames0 names0'
          respond $ SlurpOutput input (ppe1 `PPE.unionLeft` ppe2) result

      UpdateI (Path.toAbsoluteSplit currentPath' -> (p,seg)) hqs -> case uf of
        Nothing -> respond NoUnisonFile
        Just uf -> do
          let names0 = Branch.toNames0 . Branch.head $ currentBranch'
              result = applySelection hqs uf . toSlurpResult uf $ names0
              fileNames0 = UF.typecheckedToNames0 uf
              -- todo: display some error if typeEdits or termEdits itself contains a loop
              typeEdits :: [(Reference, TypeEdit)]
              typeEdits = map f (toList $ SC.types (updates result)) where
                f v = case (toList (Names.typesNamed names0 n)
                              ,toList (Names.typesNamed fileNames0 n)) of
                  ([old],[new]) -> (old, TypeEdit.Replace new)
                  _ -> error $ "Expected unique matches for "
                            ++ Var.nameStr v ++ " but got: "
                            ++ show otherwise
                  where n = Name.fromVar v
          termEdits <- for (toList $ SC.terms (updates result)) $ \v ->
            case ( toList (Names.refTermsNamed names0 (Name.fromVar v))
                 , toList (Names.refTermsNamed fileNames0 (Name.fromVar v))) of
              ([old],[new]) -> pure (old, new)
              _ -> error $ "Expected unique matches for "
                        ++ Var.nameStr v ++ " but got: "
                        ++ show otherwise
          ye'ol'Patch <- do
            b <- getAt p
            eval . Eval $ Branch.getPatch seg (Branch.head b)
          let neededTypes = Patch.collectForTyping termEdits ye'ol'Patch
          allTypes <- fmap Map.fromList . for (toList neededTypes) $ \r ->
            (r,) <$> (eval . LoadTypeOfTerm) r

          let typing r1 r2 = case (Map.lookup r1 allTypes, Map.lookup r2 allTypes) of
                (Just (Just t1), Just (Just t2))
                  | Typechecker.isEqual t1 t2 -> TermEdit.Same
                  | Typechecker.isSubtype t1 t2 -> TermEdit.Subtype
                  | otherwise -> TermEdit.Different
                _ -> error "compiler bug: typing map not constructed properly"
          let updatePatch :: Patch -> Patch
              updatePatch p = foldl' step2 (foldl' step1 p typeEdits) termEdits
                where
                step1 p (r,e) = Patch.updateType r e p
                step2 p (r,r') = Patch.updateTerm typing r (TermEdit.Replace r (typing r r')) p
              updateEdits :: Branch0 m -> m (Branch0 m)
              updateEdits = Branch.modifyEdits seg updatePatch

          when (Slurp.isNonempty result) $ do
          -- take a look at the `updates` from the SlurpResult
          -- and make a patch diff to record a replacement from the old to new references
            stepManyAtM
              [( Path.unabsolute currentPath'
               , pure . doSlurpAdds (Slurp.adds result) uf)
              ,( Path.unabsolute p, updateEdits )]
            eval . AddDefsToCodebase . filterBySlurpResult result $ uf
          let ppe1 = PPE.fromNames0 . UF.typecheckedToNames0 $ uf
              ppe2 = PPE.fromNames0 names0'
          respond $ SlurpOutput input (ppe1 `PPE.unionLeft` ppe2) result

      TodoI editPath' branchPath' -> do
        patch <- do
          let (p,seg) = Path.toAbsoluteSplit currentPath' editPath'
          b <- getAt p
          eval . Eval $ Branch.getPatch seg (Branch.head b)
        branch <- do
          let p = Path.toAbsolutePath currentPath' branchPath'
          getAt p
        let names0 = (Branch.toNames0 . Branch.head) branch
        checkTodo patch names0 >>= respond . TodoOutput names0

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
  f <- frontier' (eval . GetDependents) patch names0
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
  --             f is in the frontier,
  --         and d depends of f
  -- a ⋖ b = a depends on b (with no intermediate dependencies)
  -- dirty(d) ∧ frontier(f) <=> not(edited(d)) ∧ edited(f) ∧ d ⋖ f
  --
  -- The range of this relation is the frontier, and the domain is
  -- the set of dirty references.
  frontier' :: forall m . Monad m
           => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
           -> Patch
           -> Names0
           -> m (R.Relation Reference Reference)
  frontier' getDependents patch names = let
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
  doTerm :: v -> (Path, Branch0 m -> Branch0 m)
  doTerm v = case toList (Names.termsNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTermName split r
    wha -> error $ "Unison bug, typechecked file w/ multiple terms named "
                <> Var.nameStr v <> ": " <> show wha
  doType :: v -> (Path, Branch0 m -> Branch0 m)
  doType v = case toList (Names.typesNamed names (Name.fromVar v)) of
    [] -> errorMissingVar v
    [r] -> case Path.splitFromName (Name.fromVar v) of
      Nothing -> errorEmptyVar
      Just split -> BranchUtil.makeAddTypeName split r
    wha -> error $ "Unison bug, typechecked file w/ multiple types named "
                <> Var.nameStr v <> ": " <> show wha
  errorEmptyVar = error "encountered an empty var name"
  errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

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

loadTypeDisplayThing :: Reference -> _ (DisplayThing (DD.Decl _ _))
loadTypeDisplayThing = \case
  Reference.Builtin _ -> pure BuiltinThing
  Reference.DerivedId id ->
    maybe (MissingThing id) RegularThing <$> eval (LoadType id)
