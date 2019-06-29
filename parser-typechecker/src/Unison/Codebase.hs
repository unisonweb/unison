{-# LANGUAGE OverloadedStrings   #-}

module Unison.Codebase where

import           Control.Monad                  ( foldM
                                                , forM
                                                )
import           Data.Foldable                  ( toList, traverse_ )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes, isJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Unison.ABT                    as ABT
import qualified Unison.Builtin                as Builtin
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.CodeLookup    as CL
import qualified Unison.DataDeclaration        as DD
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent as Referent
import Unison.Referent (Referent)
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import           Unison.Typechecker.TypeLookup  (TypeLookup(TypeLookup))
import qualified Unison.Typechecker.TypeLookup as TL
import           Unison.Parser                  ( Ann )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Relation          as Rel
import qualified Unison.Var                    as Var
import           Unison.Var                     ( Var )
import qualified Unison.Runtime.IOSource       as IOSource
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Codebase.BranchUtil as BranchUtil

--import Debug.Trace

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a

type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a
type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)


data Codebase m v a =
  Codebase { getTerm            :: Reference.Id -> m (Maybe (Term v a))
           , getTypeOfTerm      :: Reference -> m (Maybe (Type v a))
           , getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))

           , putTerm            :: Reference.Id -> Term v a -> Type v a -> m ()
           , putTypeDeclaration :: Reference.Id -> Decl v a -> m ()

           , getRootBranch      :: m (Branch m)
           , putRootBranch      :: Branch m -> m ()
           , rootBranchUpdates  :: m (m (), m (Set Branch.Hash))

           , dependentsImpl     :: Reference -> m (Set Reference.Id)
           , syncFromDirectory  :: FilePath -> m ()
           , syncToDirectory    :: FilePath -> Branch m -> m ()

           -- Watch expressions are part of the codebase, the `Reference.Id` is
           -- the hash of the source of the watch expression, and the `Term v a`
           -- is the evaluated result of the expression, decompiled to a term.
           , watches            :: UF.WatchKind -> m [Reference.Id]
           , getWatch           :: UF.WatchKind -> Reference.Id -> m (Maybe (Term v a))
           , putWatch           :: UF.WatchKind -> Reference.Id -> Term v a -> m ()

           -- list of terms of the given type
           , termsOfTypeImpl    :: Reference -> m (Set Referent)
           -- list of terms that mention the given type anywhere in their signature
           , termsMentioningTypeImpl :: Reference -> m (Set Referent)
           }

-- | Write all of the builtins types and IO types into the codebase
initializeCodebase :: forall m. Monad m => Codebase m Symbol Ann -> m ()
initializeCodebase c = do
  addDefsToCodebase c
    (UF.typecheckedUnisonFile (Map.fromList Builtin.builtinDataDecls)
                              (Map.fromList Builtin.builtinEffectDecls)
                              mempty mempty)
  addDefsToCodebase c IOSource.typecheckedFile
  let names0 = Builtin.names0 <> UF.typecheckedToNames0 IOSource.typecheckedFile
  let b0 = BranchUtil.addFromNames0 names0 Branch.empty0
  putRootBranch c (Branch.one b0)

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
      _                      -> pure ()

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
  :: Monad m => Codebase m v a -> Set Reference -> m (TL.TypeLookup v a)
typeLookupForDependencies codebase refs = foldM go mempty refs
 where
--  go ::
  go tl ref@(Reference.DerivedId id) = fmap (tl <>) $ do
    getTypeOfTerm codebase ref >>= \case
      Just typ -> pure $ TypeLookup (Map.singleton ref typ) mempty mempty
      Nothing  -> getTypeDeclaration codebase id >>= \case
        Just (Left ed) ->
          pure $ TypeLookup mempty mempty (Map.singleton ref ed)
        Just (Right dd) ->
          pure $ TypeLookup mempty (Map.singleton ref dd) mempty
        Nothing -> pure mempty
  go tl _builtin = pure tl -- codebase isn't consulted for builtins

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
        _ -> pure seen

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
  let deps0 = Term.dependencies . snd <$> (UF.allWatches uf <> UF.terms uf)
  deps <- foldM (transitiveDependencies code) Set.empty (Set.unions deps0)
  let refVar r = Var.typed (Var.RefNamed r)
--  let termName r = PPE.termName pp (Referent.Ref r)
--      typeName r = PPE.typeName pp r
  decls <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) -> fmap (r, ) <$> CL.getTypeDeclaration code rid
    _                           -> pure Nothing
  termsByRef <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) ->
      fmap (r, refVar r, ) <$> CL.getTerm code rid
    _ -> pure Nothing
  let
    unref :: Term v a -> Term v a
    unref t = ABT.visitPure go t
     where
      go t@(Term.Ref' (r@(Reference.DerivedId _))) =
        Just (Term.var (ABT.annotation t) (refVar r))
      go _ = Nothing
    datas1 = Map.fromList
      [ (r, (v, dd)) | (r, Right dd) <- decls, v <- [refVar r] ]
    effects1 = Map.fromList
      [ (r, (v, ed)) | (r, Left ed) <- decls, v <- [refVar r] ]
    ds0 = Map.fromList [ (r, (v, dd)) | (v, (r, dd)) <-
            Map.toList $ UF.dataDeclarations uf ]
    es0 = Map.fromList [ (r, (v, ed)) | (v, (r, ed)) <-
            Map.toList $ UF.effectDeclarations uf ]
    bindings = [ (v, unref t) | (_, v, t) <- termsByRef ]
    (datas', effects') = (Map.union ds0 datas1, Map.union es0 effects1)
    unrefb bs = [ (v, unref b) | (v, b) <- bs ]
    uf' = UF.UnisonFile
      (Map.fromList [ (v, (r,dd)) | (r, (v,dd)) <- Map.toList datas' ])
      (Map.fromList [ (v, (r,dd)) | (r, (v,dd)) <- Map.toList effects' ])
      (bindings ++ unrefb (UF.terms uf))
      (unrefb <$> UF.watches uf)
  pure $ uf'

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
isTerm :: Functor m => Codebase m v a -> Reference -> m Bool
isTerm code = fmap isJust . getTypeOfTerm code

isType :: Applicative m => Codebase m v a -> Reference -> m Bool
isType c r = case r of
  Reference.Builtin{} -> pure $ Builtin.isBuiltinType r
  Reference.DerivedId r -> isJust <$> getTypeDeclaration c r
