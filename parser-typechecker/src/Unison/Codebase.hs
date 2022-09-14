module Unison.Codebase
  ( Codebase,

    -- * Terms
    getTerm,
    unsafeGetTerm,
    unsafeGetTermWithType,
    getTermComponentWithTypes,
    getTypeOfTerm,
    getDeclType,
    unsafeGetTypeOfTermById,
    isTerm,
    putTerm,

    -- ** Referents (sorta-termlike)
    getTypeOfReferent,

    -- ** Search
    termsOfType,
    termsMentioningType,
    termReferencesByPrefix,
    termReferentsByPrefix,

    -- * Type declarations
    getTypeDeclaration,
    unsafeGetTypeDeclaration,
    getDeclComponent,
    putTypeDeclaration,
    typeReferencesByPrefix,
    isType,

    -- * Branches
    branchExists,
    getBranchForHash,
    putBranch,
    branchHashesByPrefix,
    lca,
    beforeImpl,
    getShallowBranchAtPath,
    getShallowCausalAtPath,
    getShallowCausalForHash,
    getShallowCausalFromRoot,
    getShallowRootBranch,
    getShallowRootCausal,

    -- * Root branch
    getRootBranch,
    getRootBranchExists,
    getRootCausalHash,
    putRootBranch,
    namesAtPath,

    -- * Patches
    patchExists,
    getPatch,
    putPatch,

    -- * Watches
    getWatch,
    lookupWatchCache,
    watches,
    putWatch,
    clearWatches,

    -- * Reflog
    getReflog,

    -- * Unambiguous hash length
    hashLength,
    branchHashLength,

    -- * Dependents
    dependents,
    dependentsOfComponent,

    -- * Sync

    -- ** Local sync
    syncFromDirectory,
    syncToDirectory,

    -- ** Remote sync
    viewRemoteBranch,
    importRemoteBranch,
    Preprocessing (..),
    pushGitBranch,
    PushGitBranchOpts (..),

    -- * Codebase path
    getCodebaseDir,
    CodebasePath,
    SyncToDir,

    -- * Direct codebase access
    runTransaction,
    withConnection,
    withConnectionIO,

    -- * Misc (organize these better)
    addDefsToCodebase,
    componentReferencesForReference,
    installUcmDependencies,
    toCodeLookup,
    typeLookupForDependencies,
    unsafeGetComponentLength,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified U.Codebase.Branch as V2
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import U.Util.Timing (time)
import qualified Unison.Builtin as Builtin
import qualified Unison.Builtin.Terms as Builtin
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation (builtinAnnotation))
import qualified Unison.Codebase.CodeLookup as CL
import Unison.Codebase.Editor.Git (withStatus)
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.RemoteRepo (ReadGitRemoteNamespace)
import qualified Unison.Codebase.GitError as GitError
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Codebase.Type
  ( Codebase (..),
    GitError (GitCodebaseError),
    PushGitBranchOpts (..),
    SyncToDir,
  )
import Unison.CodebasePath (CodebasePath, getCodebaseDir)
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as DD
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2.Convert as Hashing
import qualified Unison.Parser.Ann as Parser
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as IOSource
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import Unison.Typechecker.TypeLookup (TypeLookup (TypeLookup))
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Relation as Rel
import Unison.Var (Var)
import qualified Unison.WatchKind as WK

-- | Run a transaction on a codebase.
runTransaction :: MonadIO m => Codebase m v a -> Sqlite.Transaction b -> m b
runTransaction Codebase {withConnection} action =
  withConnection \conn -> Sqlite.runTransaction conn action

getShallowCausalFromRoot ::
  Monad m =>
  Codebase m v a ->
  -- Optional root branch, if Nothing use the codebase's root branch.
  Maybe V2.CausalHash ->
  Path.Path ->
  m (V2Branch.CausalBranch m)
getShallowCausalFromRoot codebase mayRootHash p = do
  rootCausal <- case mayRootHash of
    Nothing -> getShallowRootCausal codebase
    Just ch -> getShallowCausalForHash codebase ch
  getShallowCausalAtPath codebase p (Just rootCausal)

-- | Get the shallow representation of the root branches without loading the children or
-- history.
getShallowRootBranch :: Monad m => Codebase m v a -> m (V2.Branch m)
getShallowRootBranch codebase = do
  getShallowRootCausal codebase >>= V2Causal.value

-- | Get the shallow representation of the root branches without loading the children or
-- history.
getShallowRootCausal :: Monad m => Codebase m v a -> m (V2.CausalBranch m)
getShallowRootCausal codebase = do
  hash <- getRootCausalHash codebase
  getShallowCausalForHash codebase hash

-- | Recursively descend into causals following the given path,
-- Use the root causal if none is provided.
getShallowCausalAtPath :: Monad m => Codebase m v a -> Path -> Maybe (V2Branch.CausalBranch m) -> m (V2Branch.CausalBranch m)
getShallowCausalAtPath codebase path mayCausal = do
  causal <- whenNothing mayCausal (getShallowRootCausal codebase)
  case path of
    Path.Empty -> pure causal
    (ns Path.:< p) -> do
      b <- V2Causal.value causal
      case (V2Branch.childAt (Cv.namesegment1to2 ns) b) of
        Nothing -> pure (Cv.causalbranch1to2 Branch.empty)
        Just childCausal -> getShallowCausalAtPath codebase p (Just childCausal)

-- | Recursively descend into causals following the given path,
-- Use the root causal if none is provided.
getShallowBranchAtPath :: Monad m => Codebase m v a -> Path -> Maybe (V2Branch.Branch m) -> m (V2Branch.Branch m)
getShallowBranchAtPath codebase path mayBranch = do
  branch <- whenNothing mayBranch (getShallowRootCausal codebase >>= V2Causal.value)
  case path of
    Path.Empty -> pure branch
    (ns Path.:< p) -> do
      case (V2Branch.childAt (Cv.namesegment1to2 ns) branch) of
        Nothing -> pure V2Branch.empty
        Just childCausal -> do
          childBranch <- V2Causal.value childCausal
          getShallowBranchAtPath codebase p (Just childBranch)

-- | Get a branch from the codebase.
getBranchForHash :: Monad m => Codebase m v a -> Branch.CausalHash -> m (Maybe (Branch m))
getBranchForHash codebase h =
  -- Attempt to find the Branch in the current codebase cache and root up to 3 levels deep
  -- If not found, attempt to find it in the Codebase (sqlite)
  let nestedChildrenForDepth :: Int -> Branch m -> [Branch m]
      nestedChildrenForDepth depth b =
        if depth == 0
          then []
          else b : (Map.elems (Branch._children (Branch.head b)) >>= nestedChildrenForDepth (depth - 1))

      headHashEq = (h ==) . Branch.headHash

      find rb = List.find headHashEq (nestedChildrenForDepth 3 rb)
   in do
        rootBranch <- getRootBranch codebase
        maybe (getBranchForHashImpl codebase h) (pure . Just) (find rootBranch)

-- | Get the lowest common ancestor of two branches, i.e. the most recent branch that is an ancestor of both branches.
lca :: Monad m => Codebase m v a -> Branch m -> Branch m -> m (Maybe (Branch m))
lca code b1@(Branch.headHash -> h1) b2@(Branch.headHash -> h2) = case lcaImpl code of
  Nothing -> Branch.lca b1 b2
  Just lca -> do
    eb1 <- branchExists code h1
    eb2 <- branchExists code h2
    if eb1 && eb2
      then do
        lca h1 h2 >>= \case
          Just h -> getBranchForHash code h
          Nothing -> pure Nothing -- no common ancestor
      else Branch.lca b1 b2

debug :: Bool
debug = False

-- | Write all of UCM's dependencies (builtins types and an empty namespace) into the codebase
installUcmDependencies :: forall m. Monad m => Codebase m Symbol Parser.Ann -> m ()
installUcmDependencies c = do
  let uf =
        ( UF.typecheckedUnisonFile
            (Map.fromList Builtin.builtinDataDecls)
            (Map.fromList Builtin.builtinEffectDecls)
            [Builtin.builtinTermsSrc Parser.Intrinsic]
            mempty
        )
  addDefsToCodebase c uf

-- Feel free to refactor this to use some other type than TypecheckedUnisonFile
-- if it makes sense to later.
addDefsToCodebase ::
  forall m v a.
  (Monad m, Var v, Show a) =>
  Codebase m v a ->
  UF.TypecheckedUnisonFile v a ->
  m ()
addDefsToCodebase c uf = do
  traverse_ (goType Right) (UF.dataDeclarationsId' uf)
  traverse_ (goType Left) (UF.effectDeclarationsId' uf)
  -- put terms
  traverse_ goTerm (UF.hashTermsId uf)
  where
    goTerm t | debug && trace ("Codebase.addDefsToCodebase.goTerm " ++ show t) False = undefined
    goTerm (r, Nothing, tm, tp) = putTerm c r tm tp
    goTerm (r, Just WK.TestWatch, tm, tp) = putTerm c r tm tp
    goTerm _ = pure ()
    goType :: Show t => (t -> Decl v a) -> (Reference.Id, t) -> m ()
    goType _f pair | debug && trace ("Codebase.addDefsToCodebase.goType " ++ show pair) False = undefined
    goType f (ref, decl) = putTypeDeclaration c ref (f decl)

getTypeOfConstructor ::
  (Monad m, Ord v) => Codebase m v a -> ConstructorReference -> m (Maybe (Type v a))
getTypeOfConstructor codebase (ConstructorReference (Reference.DerivedId r) cid) = do
  maybeDecl <- getTypeDeclaration codebase r
  pure $ case maybeDecl of
    Nothing -> Nothing
    Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
getTypeOfConstructor _ r =
  error $ "Don't know how to getTypeOfConstructor " ++ show r

-- | Like 'getWatch', but first looks up the given reference as a regular watch, then as a test watch.
--
-- @
-- lookupWatchCache codebase ref =
--   runMaybeT do
--     MaybeT (getWatch codebase RegularWatch ref)
--       <|> MaybeT (getWatch codebase TestWatch ref))
-- @
lookupWatchCache :: (Monad m) => Codebase m v a -> Reference.Id -> m (Maybe (Term v a))
lookupWatchCache codebase h = do
  m1 <- getWatch codebase WK.RegularWatch h
  maybe (getWatch codebase WK.TestWatch h) (pure . Just) m1

typeLookupForDependencies ::
  (Monad m, BuiltinAnnotation a) =>
  Codebase m Symbol a ->
  Set Reference ->
  m (TL.TypeLookup Symbol a)
typeLookupForDependencies codebase s = do
  when debug $ traceM $ "typeLookupForDependencies " ++ show s
  foldM go mempty s
  where
    go tl ref@(Reference.DerivedId id) =
      fmap (tl <>) $
        getTypeOfTerm codebase ref >>= \case
          Just typ -> pure $ TypeLookup (Map.singleton ref typ) mempty mempty
          Nothing ->
            getTypeDeclaration codebase id >>= \case
              Just (Left ed) ->
                pure $ TypeLookup mempty mempty (Map.singleton ref ed)
              Just (Right dd) ->
                pure $ TypeLookup mempty (Map.singleton ref dd) mempty
              Nothing -> pure mempty
    go tl Reference.Builtin {} = pure tl -- codebase isn't consulted for builtins

toCodeLookup :: Monad m => Codebase m Symbol Parser.Ann -> CL.CodeLookup Symbol m Parser.Ann
toCodeLookup c =
  CL.CodeLookup (getTerm c) (getTypeDeclaration c)
    <> Builtin.codeLookup
    <> IOSource.codeLookupM

-- | Get the type of a term.
--
-- Note that it is possible to call 'putTerm', then 'getTypeOfTerm', and receive @Nothing@, per the semantics of
-- 'putTerm'.
getTypeOfTerm ::
  (Applicative m, BuiltinAnnotation a) =>
  Codebase m Symbol a ->
  Reference ->
  m (Maybe (Type Symbol a))
getTypeOfTerm _c r | debug && trace ("Codebase.getTypeOfTerm " ++ show r) False = undefined
getTypeOfTerm c r = case r of
  Reference.DerivedId h -> getTypeOfTermImpl c h
  r@Reference.Builtin {} ->
    pure $
      fmap (const builtinAnnotation)
        <$> Map.lookup r Builtin.termRefTypes

-- | Get the type of a referent.
getTypeOfReferent ::
  (BuiltinAnnotation a, Monad m) =>
  Codebase m Symbol a ->
  Referent.Referent ->
  m (Maybe (Type Symbol a))
getTypeOfReferent c = \case
  Referent.Ref r -> getTypeOfTerm c r
  Referent.Con r _ -> getTypeOfConstructor c r

componentReferencesForReference :: Monad m => Codebase m v a -> Reference -> m (Set Reference)
componentReferencesForReference c = \case
  r@Reference.Builtin {} -> pure (Set.singleton r)
  Reference.Derived h _i ->
    Set.mapMonotonic Reference.DerivedId . Reference.componentFromLength h <$> unsafeGetComponentLength c h

-- | Get the set of terms, type declarations, and builtin types that depend on the given term, type declaration, or
-- builtin type.
dependents :: Functor m => Codebase m v a -> Reference -> m (Set Reference)
dependents c r =
  Set.union (Builtin.builtinTypeDependents r)
    . Set.map Reference.DerivedId
    <$> dependentsImpl c r

dependentsOfComponent :: Functor f => Codebase f v a -> Hash -> f (Set Reference)
dependentsOfComponent c h =
  Set.union (Builtin.builtinTypeDependentsOfComponent h)
    . Set.map Reference.DerivedId
    <$> dependentsOfComponentImpl c h

-- | Get the set of terms-or-constructors that have the given type.
termsOfType :: (Var v, Functor m) => Codebase m v a -> Type v a -> m (Set Referent.Referent)
termsOfType c ty = termsOfTypeByReference c $ Hashing.typeToReference ty

-- | Get all terms which match the exact type the provided reference points to.
termsOfTypeByReference :: (Var v, Functor m) => Codebase m v a -> Reference -> m (Set Referent.Referent)
termsOfTypeByReference c r =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByType)
    . Set.map (fmap Reference.DerivedId)
    <$> termsOfTypeImpl c r

-- | Get the set of terms-or-constructors mention the given type anywhere in their signature.
termsMentioningType :: (Var v, Functor m) => Codebase m v a -> Type v a -> m (Set Referent.Referent)
termsMentioningType c ty =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByTypeMention)
    . Set.map (fmap Reference.DerivedId)
    <$> termsMentioningTypeImpl c r
  where
    r = Hashing.typeToReference ty

-- | Check whether a reference is a term.
isTerm ::
  (Applicative m, BuiltinAnnotation a) =>
  Codebase m Symbol a ->
  Reference ->
  m Bool
isTerm code = fmap isJust . getTypeOfTerm code

isType :: Applicative m => Codebase m v a -> Reference -> m Bool
isType c r = case r of
  Reference.Builtin {} -> pure $ Builtin.isBuiltinType r
  Reference.DerivedId r -> isJust <$> getTypeDeclaration c r

-- * Git stuff

-- | An optional preprocessing step to run on branches
-- before they're imported into the local codebase.
data Preprocessing m
  = Unmodified
  | Preprocessed (Branch m -> m (Branch m))

-- | Sync elements as needed from a remote git codebase into the local one.
-- If `sbh` is supplied, we try to load the specified branch hash;
-- otherwise we try to load the root branch.
importRemoteBranch ::
  forall m v a.
  MonadUnliftIO m =>
  Codebase m v a ->
  ReadGitRemoteNamespace ->
  SyncMode ->
  Preprocessing m ->
  m (Either GitError (Branch m))
importRemoteBranch codebase ns mode preprocess = runExceptT $ do
  branchHash <- ExceptT . viewRemoteBranch' codebase ns Git.RequireExistingBranch $ \(branch, cacheDir) -> do
    withStatus "Importing downloaded files into local codebase..." $ do
      processedBranch <- preprocessOp branch
      time "SyncFromDirectory" $ do
        syncFromDirectory codebase cacheDir mode processedBranch
        pure $ Branch.headHash processedBranch
  time "load fresh local branch after sync" $ do
    lift (getBranchForHash codebase branchHash) >>= \case
      Nothing -> throwE . GitCodebaseError $ GitError.CouldntLoadSyncedBranch ns branchHash
      Just result -> pure $ result
  where
    preprocessOp :: Branch m -> m (Branch m)
    preprocessOp = case preprocess of
      Preprocessed f -> f
      Unmodified -> pure

-- | Pull a git branch and view it from the cache, without syncing into the
-- local codebase.
viewRemoteBranch ::
  MonadIO m =>
  Codebase m v a ->
  ReadGitRemoteNamespace ->
  Git.GitBranchBehavior ->
  (Branch m -> m r) ->
  m (Either GitError r)
viewRemoteBranch codebase ns gitBranchBehavior action =
  viewRemoteBranch' codebase ns gitBranchBehavior (\(b, _dir) -> action b)

unsafeGetComponentLength :: (HasCallStack, Monad m) => Codebase m v a -> Hash -> m Reference.CycleSize
unsafeGetComponentLength codebase h =
  getComponentLength codebase h >>= \case
    Nothing -> error (reportBug "E713350" ("component with hash " ++ show h ++ " not found"))
    Just size -> pure size

-- | Like 'getTerm', for when the term is known to exist in the codebase.
unsafeGetTerm :: (HasCallStack, Monad m) => Codebase m v a -> Reference.Id -> m (Term v a)
unsafeGetTerm codebase rid =
  getTerm codebase rid >>= \case
    Nothing -> error (reportBug "E520818" ("term " ++ show rid ++ " not found"))
    Just term -> pure term

-- | Like 'getTypeDeclaration', for when the type declaration is known to exist in the codebase.
unsafeGetTypeDeclaration :: (HasCallStack, Monad m) => Codebase m v a -> Reference.Id -> m (Decl v a)
unsafeGetTypeDeclaration codebase rid =
  getTypeDeclaration codebase rid >>= \case
    Nothing -> error (reportBug "E129043" ("type decl " ++ show rid ++ " not found"))
    Just decl -> pure decl

-- | Like 'getTypeOfTerm', but for when the term is known to exist in the codebase.
unsafeGetTypeOfTermById :: (HasCallStack, Monad m) => Codebase m v a -> Reference.Id -> m (Type v a)
unsafeGetTypeOfTermById codebase rid =
  getTypeOfTermImpl codebase rid >>= \case
    Nothing -> error (reportBug "E377910" ("type of term " ++ show rid ++ " not found"))
    Just ty -> pure ty

-- | Like 'unsafeGetTerm', but returns the type of the term, too.
unsafeGetTermWithType :: (HasCallStack, Monad m) => Codebase m v a -> Reference.Id -> m (Term v a, Type v a)
unsafeGetTermWithType codebase rid = do
  term <- unsafeGetTerm codebase rid
  ty <-
    -- A term is sometimes stored with a type annotation (specifically, when the annotation is different from the
    -- inferred type). In this case, we can avoid looking up the type separately.
    case term of
      Term.Ann' _ ty -> pure ty
      _ -> unsafeGetTypeOfTermById codebase rid
  pure (term, ty)
