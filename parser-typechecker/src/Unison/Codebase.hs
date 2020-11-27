{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase where

import Control.Lens ((%=), _1, _2)
import Control.Monad.State (State, evalState, get)
import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as Builtin
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.CodeLookup as CL
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.SyncMode (SyncMode)
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Names2 as Names
import qualified Unison.Parser as Parser
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as IOSource
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Typechecker.TypeLookup (TypeLookup (TypeLookup))
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Relation as Rel
import qualified Unison.Util.Set as Set
import Unison.Var (Var)
import qualified Unison.Var as Var

type DataDeclaration v a = DD.DataDeclaration v a

type EffectDeclaration v a = DD.EffectDeclaration v a

-- | this FileCodebase detail lives here, because the interface depends on it 🙃
type CodebasePath = FilePath

data Codebase m v a = Codebase
  { getTerm :: Reference.Id -> m (Maybe (Term v a)),
    getTypeOfTermImpl :: Reference.Id -> m (Maybe (Type v a)),
    getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a)),
    putTerm :: Reference.Id -> Term v a -> Type v a -> m (),
    putTypeDeclaration :: Reference.Id -> Decl v a -> m (),
    getRootBranch :: m (Either GetRootBranchError (Branch m)),
    putRootBranch :: Branch m -> m (),
    rootBranchUpdates :: m (m (), m (Set Branch.Hash)),
    getBranchForHash :: Branch.Hash -> m (Maybe (Branch m)),
    dependentsImpl :: Reference -> m (Set Reference.Id),
    -- This copies all the dependencies of `b` from the specified
    -- FileCodebase into this Codebase, and sets our root branch to `b`
    syncFromDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- This copies all the dependencies of `b` from the this Codebase
    -- into the specified FileCodebase, and sets its _head to `b`
    syncToDirectory :: CodebasePath -> SyncMode -> Branch m -> m (),
    -- Watch expressions are part of the codebase, the `Reference.Id` is
    -- the hash of the source of the watch expression, and the `Term v a`
    -- is the evaluated result of the expression, decompiled to a term.
    watches :: UF.WatchKind -> m [Reference.Id],
    getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Term v a)),
    putWatch :: UF.WatchKind -> Reference.Id -> Term v a -> m (),
    getReflog :: m [Reflog.Entry],
    appendReflog :: Text -> Branch m -> Branch m -> m (),
    -- list of terms of the given type
    termsOfTypeImpl :: Reference -> m (Set Referent.Id),
    -- list of terms that mention the given type anywhere in their signature
    termsMentioningTypeImpl :: Reference -> m (Set Referent.Id),
    -- number of base58 characters needed to distinguish any two references in the codebase
    hashLength :: m Int,
    termReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    typeReferencesByPrefix :: ShortHash -> m (Set Reference.Id),
    termReferentsByPrefix :: ShortHash -> m (Set Referent.Id),
    branchHashLength :: m Int,
    branchHashesByPrefix :: ShortBranchHash -> m (Set Branch.Hash)
  }

data GetRootBranchError
  = NoRootBranch
  | CouldntParseRootBranch String
  | CouldntLoadRootBranch Branch.Hash
  deriving (Show)

data SyncFileCodebaseResult = SyncOk | UnknownDestinationRootBranch Branch.Hash | NotFastForward

bootstrapNames :: Names.Names0
bootstrapNames =
  Builtin.names0 <> UF.typecheckedToNames0 IOSource.typecheckedFile

-- | Write all of the builtins types into the codebase and create empty namespace
initializeCodebase :: forall m. Monad m => Codebase m Symbol Parser.Ann -> m ()
initializeCodebase c = do
  let uf =
        ( UF.typecheckedUnisonFile
            (Map.fromList Builtin.builtinDataDecls)
            (Map.fromList Builtin.builtinEffectDecls)
            mempty
            mempty
        )
  addDefsToCodebase c uf
  putRootBranch c (Branch.one Branch.empty0)

-- Feel free to refactor this to use some other type than TypecheckedUnisonFile
-- if it makes sense to later.
addDefsToCodebase ::
  forall m v a.
  (Monad m, Var v) =>
  Codebase m v a ->
  UF.TypecheckedUnisonFile v a ->
  m ()
addDefsToCodebase c uf = do
  traverse_ (goType Right) (UF.dataDeclarationsId' uf)
  traverse_ (goType Left) (UF.effectDeclarationsId' uf)
  -- put terms
  traverse_ goTerm (UF.hashTermsId uf)
  where
    goTerm (r, tm, tp) = putTerm c r tm tp
    goType :: (t -> Decl v a) -> (Reference.Id, t) -> m ()
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

typeLookupForDependencies ::
  (Monad m, Var v, BuiltinAnnotation a) =>
  Codebase m v a ->
  Set Reference ->
  m (TL.TypeLookup v a)
typeLookupForDependencies codebase = foldM go mempty
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

-- todo: can this be implemented in terms of TransitiveClosure.transitiveClosure?
-- todo: add some tests on this guy?
transitiveDependencies ::
  (Monad m, Var v) =>
  CL.CodeLookup v m a ->
  Set Reference.Id ->
  Reference.Id ->
  m (Set Reference.Id)
transitiveDependencies code seen0 rid =
  if Set.member rid seen0
    then pure seen0
    else
      let seen = Set.insert rid seen0
          getIds = Set.mapMaybe Reference.toId
       in CL.getTerm code rid >>= \case
            Just t ->
              foldM (transitiveDependencies code) seen (getIds $ Term.dependencies t)
            Nothing ->
              CL.getTypeDeclaration code rid >>= \case
                Nothing -> pure seen
                Just (Left ed) ->
                  foldM
                    (transitiveDependencies code)
                    seen
                    (getIds $ DD.dependencies (DD.toDataDecl ed))
                Just (Right dd) ->
                  foldM
                    (transitiveDependencies code)
                    seen
                    (getIds $ DD.dependencies dd)

toCodeLookup :: Codebase m v a -> CL.CodeLookup v m a
toCodeLookup c = CL.CodeLookup (getTerm c) (getTypeDeclaration c)

-- Like the other `makeSelfContained`, but takes and returns a `UnisonFile`.
-- Any watches in the input `UnisonFile` will be watches in the returned
-- `UnisonFile`.
makeSelfContained' ::
  forall m v a.
  (Monad m, Monoid a, Var v) =>
  CL.CodeLookup v m a ->
  UF.UnisonFile v a ->
  m (UF.UnisonFile v a)
makeSelfContained' code uf = do
  let UF.UnisonFileId ds0 es0 bs0 ws0 = uf
      deps0 = getIds . Term.dependencies . snd <$> (UF.allWatches uf <> bs0)
        where
          getIds = Set.mapMaybe Reference.toId
  -- transitive dependencies (from codebase) of all terms (including watches) in the UF
  deps <- foldM (transitiveDependencies code) Set.empty (Set.unions deps0)
  -- load all decls from deps list
  decls <- fmap catMaybes
    . forM (toList deps)
    $ \rid -> fmap (rid,) <$> CL.getTypeDeclaration code rid
  -- partition the decls into effects and data
  let es1 :: [(Reference.Id, DD.EffectDeclaration v a)]
      ds1 :: [(Reference.Id, DD.DataDeclaration v a)]
      (es1, ds1) = partitionEithers [bimap (r,) (r,) d | (r, d) <- decls]
  -- load all terms from deps list
  bs1 <- fmap catMaybes
    . forM (toList deps)
    $ \rid -> fmap (rid,) <$> CL.getTerm code rid
  let allVars :: Set v
      allVars =
        Set.unions
          [ UF.allVars uf,
            Set.unions [DD.allVars dd | (_, dd) <- ds1],
            Set.unions [DD.allVars (DD.toDataDecl ed) | (_, ed) <- es1],
            Set.unions [Term.allVars tm | (_, tm) <- bs1]
          ]
      refVar :: Reference.Id -> State (Set v, Map Reference.Id v) v
      refVar r = do
        m <- snd <$> get
        case Map.lookup r m of
          Just v -> pure v
          Nothing -> do
            v <- ABT.freshenS' _1 (Var.refNamed (Reference.DerivedId r))
            _2 %= Map.insert r v
            pure v
      assignVars :: [(Reference.Id, b)] -> State (Set v, Map Reference.Id v) [(v, (Reference.Id, b))]
      assignVars = traverse (\e@(r, _) -> (,e) <$> refVar r)
      unref :: Term v a -> State (Set v, Map Reference.Id v) (Term v a)
      unref = ABT.visit go
        where
          go t@(Term.Ref' (Reference.DerivedId r)) =
            Just (Term.var (ABT.annotation t) <$> refVar r)
          go _ = Nothing
      unrefb = traverse (\(v, tm) -> (v,) <$> unref tm)
      pair :: forall f a b. Applicative f => f a -> f b -> f (a, b)
      pair = liftA2 (,)
      uf' = flip evalState (allVars, Map.empty) $ do
        datas' <- Map.union ds0 . Map.fromList <$> assignVars ds1
        effects' <- Map.union es0 . Map.fromList <$> assignVars es1
        -- bs0 is terms from the input file
        bs0' <- unrefb bs0
        ws0' <- traverse unrefb ws0
        -- bs1 is dependency terms
        bs1' <- traverse (\(r, tm) -> refVar r `pair` unref tm) bs1
        pure $ UF.UnisonFileId datas' effects' (bs1' ++ bs0') ws0'
  pure uf'

getTypeOfTerm ::
  (Applicative m, Var v, BuiltinAnnotation a) =>
  Codebase m v a ->
  Reference ->
  m (Maybe (Type v a))
getTypeOfTerm c = \case
  Reference.DerivedId h -> getTypeOfTermImpl c h
  r@Reference.Builtin {} ->
    pure $
      fmap (const builtinAnnotation)
        <$> Map.lookup r Builtin.termRefTypes

-- The dependents of a builtin type is the set of builtin terms which
-- mention that type.
dependents :: Functor m => Codebase m v a -> Reference -> m (Set Reference)
dependents c r =
  Set.union (Builtin.builtinTypeDependents r)
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
isTerm ::
  (Applicative m, Var v, BuiltinAnnotation a) =>
  Codebase m v a ->
  Reference ->
  m Bool
isTerm code = fmap isJust . getTypeOfTerm code

isType :: Applicative m => Codebase m v a -> Reference -> m Bool
isType c r = case r of
  Reference.Builtin {} -> pure $ Builtin.isBuiltinType r
  Reference.DerivedId r -> isJust <$> getTypeDeclaration c r

class BuiltinAnnotation a where
  builtinAnnotation :: a

instance BuiltinAnnotation Parser.Ann where
  builtinAnnotation = Parser.Intrinsic
