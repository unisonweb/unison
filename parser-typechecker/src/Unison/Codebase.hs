{-# LANGUAGE OverloadedStrings   #-}

module Unison.Codebase where

import Unison.Prelude

import           Control.Category               ( (>>>) )
import           Control.Lens                   ( _1, _2, (%=) )
import           Control.Monad.State            ( State, evalState, get )
import           Data.Bifunctor                 ( bimap )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Unison.ABT                    as ABT
import qualified Unison.Builtin                as Builtin
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.CodeLookup    as CL
import qualified Unison.Codebase.Reflog        as Reflog
import qualified Unison.DataDeclaration        as DD
import qualified Unison.Names2                 as Names
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent as Referent
import Unison.Referent (Referent, Referent')
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import           Unison.Typechecker.TypeLookup  (TypeLookup(TypeLookup))
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Parser                 as Parser
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Relation          as Rel
import qualified Unison.Var                    as Var
import           Unison.Var                     ( Var )
import qualified Unison.Runtime.IOSource       as IOSource
import           Unison.Symbol                  ( Symbol )
import Unison.DataDeclaration (Decl)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.ShortHash (ShortHash)

--import Debug.Trace

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a

data Codebase m v a =
  Codebase { getTerm            :: Reference.Id -> m (Maybe (Term v a))
           , getTypeOfTermImpl  :: Reference.Id -> m (Maybe (Type v a))
           , getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))

           , putTerm            :: Reference.Id -> Term v a -> Type v a -> m ()
           , putTypeDeclaration :: Reference.Id -> Decl v a -> m ()

           , getRootBranch      :: m (Branch m)
           , putRootBranch      :: Branch m -> m ()
           , rootBranchUpdates  :: m (m (), m (Set Branch.Hash))
           , getBranchForHash   :: Branch.Hash -> m (Branch m)

           , dependentsImpl     :: Reference -> m (Set Reference.Id)
           -- This copies all codebase elements (except _head) from the
           -- specified FileCodebase path into the current one.
           , syncFromDirectory  :: FilePath -> m ()
           -- This returns the merged branch that results from
           -- merging the input branch with the root branch at the
           -- given file path
           , syncToDirectory    :: FilePath -> Branch m -> m (Branch m)

           -- Watch expressions are part of the codebase, the `Reference.Id` is
           -- the hash of the source of the watch expression, and the `Term v a`
           -- is the evaluated result of the expression, decompiled to a term.
           , watches            :: UF.WatchKind -> m [Reference.Id]
           , getWatch           :: UF.WatchKind -> Reference.Id -> m (Maybe (Term v a))
           , putWatch           :: UF.WatchKind -> Reference.Id -> Term v a -> m ()

           , getReflog          :: m [Reflog.Entry]
           , appendReflog       :: Text -> Branch m -> Branch m -> m ()

           -- list of terms of the given type
           , termsOfTypeImpl    :: Reference -> m (Set Referent)
           -- list of terms that mention the given type anywhere in their signature
           , termsMentioningTypeImpl :: Reference -> m (Set Referent)
           -- number of base58 characters needed to distinguish any two references in the codebase
           , hashLength         :: m Int
           , termReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
           , typeReferencesByPrefix :: ShortHash -> m (Set Reference.Id)
           , termReferentsByPrefix :: ShortHash -> m (Set (Referent' Reference.Id))

           , branchHashLength   :: m Int
           , branchHashesByPrefix :: ShortBranchHash -> m (Set Branch.Hash)
           }

bootstrapNames :: Names.Names0
bootstrapNames =
  Builtin.names0 <> UF.typecheckedToNames0 IOSource.typecheckedFile

-- | Write all of the builtins types and IO types into the codebase. Returns the names of builtins
-- but DOES NOT add these names to the namespace.
initializeBuiltinCode :: forall m. Monad m => Codebase m Symbol Parser.Ann -> m ()
initializeBuiltinCode c = do
  let uf = (UF.typecheckedUnisonFile (Map.fromList Builtin.builtinDataDecls)
                                     (Map.fromList Builtin.builtinEffectDecls)
                                     mempty mempty)
  addDefsToCodebase c uf
  addDefsToCodebase c IOSource.typecheckedFile
  pure ()

-- | Write all of the builtins types and IO types into the codebase and put their
-- names under .builtin
initializeCodebase :: forall m. Monad m => Codebase m Symbol Parser.Ann -> m ()
initializeCodebase c = do
  initializeBuiltinCode c
  putRootBranch c (Branch.one Branch.empty0)

-- Feel free to refactor this to use some other type than TypecheckedUnisonFile
-- if it makes sense to later.
addDefsToCodebase :: forall m v a. (Monad m, Var v)
  => Codebase m v a -> UF.TypecheckedUnisonFile v a -> m ()
addDefsToCodebase c uf = do
  traverse_ (goType Right) (UF.dataDeclarations' uf)
  traverse_ (goType Left)  (UF.effectDeclarations' uf)
  -- put terms
  traverse_ goTerm (UF.hashTerms uf)
  where
    goTerm (Reference.DerivedId r, tm, tp) = putTerm c r tm tp
    goTerm b = error $ "tried to write builtin term to codebase: " ++ show b
    goType :: (t -> Decl v a) -> (Reference.Reference, t) -> m ()
    goType f (ref, decl) = case ref of
      Reference.DerivedId id -> putTypeDeclaration c id (f decl)
      Reference.Builtin{}    -> pure ()

getTypeOfConstructor ::
  (Monad m, Ord v) => Codebase m v a -> Reference -> Int -> m (Maybe (Type v a))
getTypeOfConstructor codebase (Reference.DerivedId r) cid = do
  maybeDecl <- getTypeDeclaration codebase r
  pure $ case maybeDecl of
    Nothing -> Nothing
    Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
getTypeOfConstructor _ r cid =
  error $ "Don't know how to getTypeOfConstructor " ++ show r ++ " " ++ show cid

typeLookupForDependencies
  :: (Monad m, Var v, BuiltinAnnotation a)
  => Codebase m v a -> Set Reference -> m (TL.TypeLookup v a)
typeLookupForDependencies codebase = foldM go mempty
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

-- todo: can this be implemented in terms of TransitiveClosure.transitiveClosure?
-- todo: add some tests on this guy?
transitiveDependencies
  :: (Monad m, Var v)
  => CL.CodeLookup v m a
  -> Set Reference
  -> Reference
  -> m (Set Reference)
transitiveDependencies code seen0 r = if Set.member r seen0
  then pure seen0
  else
    let seen = Set.insert r seen0
    in
      case r of
        Reference.DerivedId id -> do
          t <- CL.getTerm code id
          case t of
            Just t ->
              foldM (transitiveDependencies code) seen (Term.dependencies t)
            Nothing -> do
              t <- CL.getTypeDeclaration code id
              case t of
                Nothing        -> pure seen
                Just (Left ed) -> foldM (transitiveDependencies code)
                                        seen
                                        (DD.dependencies (DD.toDataDecl ed))
                Just (Right dd) -> foldM (transitiveDependencies code)
                                         seen
                                         (DD.dependencies dd)
        Reference.Builtin{} -> pure seen

toCodeLookup :: Codebase m v a -> CL.CodeLookup v m a
toCodeLookup c = CL.CodeLookup (getTerm c) (getTypeDeclaration c)

-- Like the other `makeSelfContained`, but takes and returns a `UnisonFile`.
-- Any watches in the input `UnisonFile` will be watches in the returned
-- `UnisonFile`.
makeSelfContained'
  :: forall m v a . (Monad m, Monoid a, Var v)
  => CL.CodeLookup v m a
  -> UF.UnisonFile v a
  -> m (UF.UnisonFile v a)
makeSelfContained' code uf = do
  let UF.UnisonFile ds0 es0 bs0 ws0 = uf
      deps0 = Term.dependencies . snd <$> (UF.allWatches uf <> bs0)
  deps <- foldM (transitiveDependencies code) Set.empty (Set.unions deps0)
  decls <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) -> fmap (r, ) <$> CL.getTypeDeclaration code rid
    Reference.Builtin{}         -> pure Nothing
  let (es1, ds1) = partitionEithers [ bimap (r,) (r,) d | (r, d) <- decls ]
  bs1 <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) ->
      fmap (r, ) <$> CL.getTerm code rid
    Reference.Builtin{} -> pure Nothing
  let
    allVars = Set.unions
      [ UF.allVars uf
      , Set.unions [ DD.allVars dd | (_, dd) <- ds1 ]
      , Set.unions [ DD.allVars (DD.toDataDecl ed) | (_, ed) <- es1 ]
      , Set.unions [ Term.allVars tm | (_, tm) <- bs1 ]
      ]
    refVar :: Reference -> State (Set v, Map Reference v) v
    refVar r = (get >>=) $ snd >>> Map.lookup r >>> \case
      Just v -> pure v
      Nothing -> do
        v <- ABT.freshenS' _1 (Var.refNamed r)
        _2 %=  Map.insert r v
        pure v
    assignVars :: [(Reference, b)] -> State (Set v, Map Reference v) [(v, (Reference, b))]
    assignVars es = traverse (\e@(r, _) -> (,e) <$> refVar r) es
    unref :: Term v a -> State (Set v, Map Reference v) (Term v a)
    unref = ABT.visit go where
      go t@(Term.Ref' r@(Reference.DerivedId _)) =
        Just (Term.var (ABT.annotation t) <$> refVar r)
      go _ = Nothing
    unrefb bs = traverse (\(v, tm) -> (v,) <$> unref tm) bs
    pair = liftA2 (,)
    uf' = flip evalState (allVars, Map.empty) $ do
      datas' <- Map.union ds0 . Map.fromList <$> assignVars ds1
      effects' <- Map.union es0 . Map.fromList <$> assignVars es1
      bs0' <- unrefb bs0
      ws0' <- traverse unrefb ws0
      bs1' <- traverse (\(r, tm) -> refVar r `pair` unref tm) bs1
      pure $ UF.UnisonFile datas' effects' (bs1' ++ bs0') ws0'
  pure uf'

getTypeOfTerm :: (Applicative m, Var v, BuiltinAnnotation a) =>
  Codebase m v a -> Reference -> m (Maybe (Type v a))
getTypeOfTerm c = \case
  Reference.DerivedId h -> getTypeOfTermImpl c h
  r@Reference.Builtin{} ->
    pure $   fmap (const builtinAnnotation)
        <$> Map.lookup r Builtin.termRefTypes


dependents :: Functor m => Codebase m v a -> Reference -> m (Set Reference)
dependents c r
    = Set.union (Builtin.builtinTypeDependents r)
    . Set.map Reference.DerivedId
  <$> dependentsImpl c r

termsOfType :: (Var v, Functor m) => Codebase m v a -> Type v a -> m (Set Referent.Referent)
termsOfType c ty =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByType) <$> termsOfTypeImpl c r
  where
  r = Type.toReference ty

termsMentioningType :: (Var v, Functor m) => Codebase m v a -> Type v a -> m (Set Referent.Referent)
termsMentioningType c ty =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByTypeMention)
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

class BuiltinAnnotation a where
  builtinAnnotation :: a

instance BuiltinAnnotation Parser.Ann where
  builtinAnnotation = Parser.Intrinsic
