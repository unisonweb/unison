{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase
  ( Codebase (..),
    CodebasePath,
    GetRootBranchError (..),
    getCodebaseDir,
    SyncToDir,
    addDefsToCodebase,
    installUcmDependencies,
    getTypeOfTerm,
    getTypeOfReferent,
    lca,
    lookupWatchCache,
    toCodeLookup,
    typeLookupForDependencies,
    importRemoteBranch,
    viewRemoteBranch,
    termsOfType,
    termsMentioningType,
    dependents,
    isTerm,
    isType,
  )
where

import Unison.Codebase.Type (Codebase (..), GetRootBranchError (..), SyncToDir, GitError (GitCodebaseError))
import Unison.CodebasePath (CodebasePath, getCodebaseDir)
import Unison.Prelude
import qualified Unison.UnisonFile as UF
import qualified Data.Map as Map
import Unison.Symbol (Symbol)
import qualified Unison.Parser.Ann as Parser
import qualified Unison.Builtin.Terms as Builtin
import qualified Unison.Builtin as Builtin
import Unison.DataDeclaration (Decl)
import qualified Unison.Reference as Reference
import Unison.Var (Var)
import Unison.Reference (Reference)
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation (builtinAnnotation))
import Unison.Type (Type)
import qualified Unison.Referent as Referent
import qualified Unison.DataDeclaration as DD
import qualified Unison.Codebase.CodeLookup as CL
import qualified Unison.WatchKind as WK
import Unison.Term (Term)
import qualified Unison.Typechecker.TypeLookup as TL
import Unison.Typechecker.TypeLookup (TypeLookup(TypeLookup))
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.Codebase.GitError as GitError
import U.Util.Timing (time)
import Unison.Codebase.Editor.Git (withStatus)
import qualified Data.Set as Set
import qualified Unison.Util.Relation as Rel
import qualified Unison.Type as Type

lca :: Monad m => Codebase m v a -> Branch m -> Branch m -> m (Maybe (Branch m))
lca code b1@(Branch.headHash -> h1) b2@(Branch.headHash -> h2) = case lcaImpl code of
  Nothing -> Branch.lca b1 b2
  Just lca -> do
    eb1 <- branchExists code h1
    eb2 <- branchExists code h2
    if eb1 && eb2 then do
      lca h1 h2 >>= \case
        Just h -> getBranchForHash code h
        Nothing -> pure Nothing -- no common ancestor
    else Branch.lca b1 b2

-- before :: Monad m => Codebase m v a -> Branch m -> Branch m -> m Bool
-- before code b1 b2 = case beforeImpl code of
--   Nothing -> Branch.before b1 b2
--   Just before -> before' (branchExists code) before b1 b2

-- before' :: Monad m => (Branch.Hash -> m Bool) -> (Branch.Hash -> Branch.Hash -> m Bool) -> Branch m -> Branch m -> m Bool
-- before' branchExists before b1@(Branch.headHash -> h1) b2@(Branch.headHash -> h2) =
--   ifM
--     (branchExists h2)
--     (ifM
--       (branchExists h2)
--       (before h1 h2)
--       (pure False))
--     (Branch.before b1 b2)


-- data GetRootBranchError
--   = NoRootBranch
--   | CouldntParseRootBranch String
--   | CouldntLoadRootBranch Branch.Hash
--   deriving Show

debug :: Bool
debug = False

-- data SyncFileCodebaseResult = SyncOk | UnknownDestinationRootBranch Branch.Hash | NotFastForward

-- getCodebaseDir :: MonadIO m => Maybe FilePath -> m FilePath
-- getCodebaseDir = maybe getHomeDirectory pure

-- | Write all of UCM's dependencies (builtins types and an empty namespace) into the codebase
installUcmDependencies :: forall m. Monad m => Codebase m Symbol Parser.Ann -> m ()
installUcmDependencies c = do
  let uf = (UF.typecheckedUnisonFile (Map.fromList Builtin.builtinDataDecls)
                                     (Map.fromList Builtin.builtinEffectDecls)
                                     [Builtin.builtinTermsSrc Parser.Intrinsic]
                                     mempty)
  addDefsToCodebase c uf

-- Feel free to refactor this to use some other type than TypecheckedUnisonFile
-- if it makes sense to later.
addDefsToCodebase :: forall m v a. (Monad m, Var v, Show a)
  => Codebase m v a -> UF.TypecheckedUnisonFile v a -> m ()
addDefsToCodebase c uf = do
  traverse_ (goType Right) (UF.dataDeclarationsId' uf)
  traverse_ (goType Left)  (UF.effectDeclarationsId' uf)
  -- put terms
  traverse_ goTerm (UF.hashTermsId uf)
  where
    goTerm t | debug && trace ("Codebase.addDefsToCodebase.goTerm " ++ show t) False = undefined
    goTerm (r, tm, tp) = putTerm c r tm tp
    goType :: Show t => (t -> Decl v a) -> (Reference.Id, t) -> m ()
    goType _f pair | debug && trace ("Codebase.addDefsToCodebase.goType " ++ show pair) False = undefined
    goType f (ref, decl) = putTypeDeclaration c ref (f decl)

getTypeOfConstructor ::
  (Monad m, Ord v) => Codebase m v a -> Reference -> Int -> m (Maybe (Type v a))
getTypeOfConstructor codebase (Reference.DerivedId r) cid = do
  maybeDecl <- getTypeDeclaration codebase r
  pure $ case maybeDecl of
    Nothing -> Nothing
    Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
getTypeOfConstructor _ r cid =
  error $ "Don't know how to getTypeOfConstructor " ++ show r ++ " " ++ show cid

lookupWatchCache :: (Monad m) => Codebase m v a -> Reference -> m (Maybe (Term v a))
lookupWatchCache codebase (Reference.DerivedId h) = do
  m1 <- getWatch codebase WK.RegularWatch h
  maybe (getWatch codebase WK.TestWatch h) (pure . Just) m1
lookupWatchCache _ Reference.Builtin{} = pure Nothing

typeLookupForDependencies
  :: (Monad m, Var v, BuiltinAnnotation a)
  => Codebase m v a -> Set Reference -> m (TL.TypeLookup v a)
typeLookupForDependencies codebase s = do
  when debug $ traceM $ "typeLookupForDependencies " ++ show s
  foldM go mempty s
 where
  go tl ref@(Reference.DerivedId id) = fmap (tl <>) $
    getTypeOfTerm codebase ref >>= \case
      Just typ -> pure $ TypeLookup (Map.singleton ref typ) mempty mempty
      Nothing  -> getTypeDeclaration codebase id >>= \case
        Just (Left ed) ->
          pure $ TypeLookup mempty mempty (Map.singleton ref ed)
        Just (Right dd) ->
          pure $ TypeLookup mempty (Map.singleton ref dd) mempty
        Nothing -> pure mempty
  go tl Reference.Builtin{} = pure tl -- codebase isn't consulted for builtins

-- -- todo: can this be implemented in terms of TransitiveClosure.transitiveClosure?
-- -- todo: add some tests on this guy?
-- transitiveDependencies
--   :: (Monad m, Var v)
--   => CL.CodeLookup v m a
--   -> Set Reference.Id
--   -> Reference.Id
--   -> m (Set Reference.Id)
-- transitiveDependencies code seen0 rid = if Set.member rid seen0
--   then pure seen0
--   else
--     let seen = Set.insert rid seen0
--         getIds = Set.mapMaybe Reference.toId
--     in CL.getTerm code rid >>= \case
--       Just t ->
--         foldM (transitiveDependencies code) seen (getIds $ Term.dependencies t)
--       Nothing ->
--         CL.getTypeDeclaration code rid >>= \case
--           Nothing        -> pure seen
--           Just (Left ed) -> foldM (transitiveDependencies code)
--                                   seen
--                                   (getIds $ DD.dependencies (DD.toDataDecl ed))
--           Just (Right dd) -> foldM (transitiveDependencies code)
--                                    seen
--                                    (getIds $ DD.dependencies dd)

toCodeLookup :: Codebase m v a -> CL.CodeLookup v m a
toCodeLookup c = CL.CodeLookup (getTerm c) (getTypeDeclaration c)

getTypeOfTerm :: (Applicative m, Var v, BuiltinAnnotation a) =>
  Codebase m v a -> Reference -> m (Maybe (Type v a))
getTypeOfTerm _c r | debug && trace ("Codebase.getTypeOfTerm " ++ show r) False = undefined
getTypeOfTerm c r = case r of
  Reference.DerivedId h -> getTypeOfTermImpl c h
  r@Reference.Builtin{} ->
    pure $   fmap (const builtinAnnotation)
        <$> Map.lookup r Builtin.termRefTypes

getTypeOfReferent :: (BuiltinAnnotation a, Var v, Monad m)
                  => Codebase m v a -> Referent.Referent -> m (Maybe (Type v a))
getTypeOfReferent c (Referent.Ref r) = getTypeOfTerm c r
getTypeOfReferent c (Referent.Con r cid _) =
  getTypeOfConstructor c r cid

-- | The dependents of a builtin type includes the set of builtin terms which
-- mention that type.
dependents :: Functor m => Codebase m v a -> Reference -> m (Set Reference)
dependents c r
    = Set.union (Builtin.builtinTypeDependents r)
    . Set.map Reference.DerivedId
  <$> dependentsImpl c r

termsOfType :: (Var v, Functor m) => Codebase m v a -> Type v a -> m (Set Referent.Referent)
termsOfType c ty =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByType)
    . Set.map (fmap Reference.DerivedId)
    <$> termsOfTypeImpl c r
  where
  r = Type.toReference ty

termsMentioningType :: (Var v, Functor m) => Codebase m v a -> Type v a -> m (Set Referent.Referent)
termsMentioningType c ty =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByTypeMention)
    . Set.map (fmap Reference.DerivedId)
    <$> termsMentioningTypeImpl c r
  where
  r = Type.toReference ty

-- todo: could have a way to look this up just by checking for a file rather than loading it
isTerm :: (Applicative m, Var v, BuiltinAnnotation a)
       => Codebase m v a -> Reference -> m Bool
isTerm code = fmap isJust . getTypeOfTerm code

isType :: Applicative m => Codebase m v a -> Reference -> m Bool
isType c r = case r of
  Reference.Builtin{} -> pure $ Builtin.isBuiltinType r
  Reference.DerivedId r -> isJust <$> getTypeDeclaration c r

-- class BuiltinAnnotation a where
--   builtinAnnotation :: a

-- instance BuiltinAnnotation Parser.Ann where
--   builtinAnnotation = Parser.Intrinsic

-- -- * Git stuff

-- | Sync elements as needed from a remote codebase into the local one.
-- If `sbh` is supplied, we try to load the specified branch hash;
-- otherwise we try to load the root branch.
importRemoteBranch ::
  forall m v a.
  MonadIO m =>
  Codebase m v a ->
  ReadRemoteNamespace ->
  SyncMode ->
  m (Either GitError (Branch m))
importRemoteBranch codebase ns mode = runExceptT do
  (cleanup, branch, cacheDir) <- ExceptT $ viewRemoteBranch' codebase ns
  withStatus "Importing downloaded files into local codebase..." $
    time "SyncFromDirectory" $
      lift $ syncFromDirectory codebase cacheDir mode branch
  ExceptT
    let h = Branch.headHash branch
        err = Left . GitCodebaseError $ GitError.CouldntLoadSyncedBranch ns h
    in time "load fresh local branch after sync" $
      (getBranchForHash codebase h <&> maybe err Right) <* cleanup

-- | Pull a git branch and view it from the cache, without syncing into the
-- local codebase.
viewRemoteBranch ::
  MonadIO m =>
  Codebase m v a ->
  ReadRemoteNamespace ->
  m (Either GitError (m (), Branch m))
viewRemoteBranch codebase ns = runExceptT do
  (cleanup, branch, _) <- ExceptT $ viewRemoteBranch' codebase ns
  pure (cleanup, branch)