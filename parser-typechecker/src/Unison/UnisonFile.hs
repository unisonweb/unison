{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.UnisonFile
  ( -- * UnisonFile
    UnisonFile (..),
    pattern UnisonFile,
    allWatches,
    dataDeclarations,
    declsToTypeLookup,
    dependencies,
    effectDeclarations,
    typecheckingTerm,
    watchesOfKind,

    -- * TypecheckedUnisonFile
    TypecheckedUnisonFile (..),
    allTerms,
    dataDeclarations',
    discardTypes,
    effectDeclarations',
    hashConstructors,
    constructorsForDecls,
    hashTerms,
    indexByReference,
    lookupDecl,
    nonEmpty,
    termSignatureExternalLabeledDependencies,
    topLevelComponents,
    typecheckedUnisonFile,
  )
where

import Control.Lens
import Data.Bifunctor (first, second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import qualified Unison.DataDeclaration as DD
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker.TypeLookup as TL
import Unison.UnisonFile.Type (TypecheckedUnisonFile (..), UnisonFile (..), pattern TypecheckedUnisonFile, pattern UnisonFile)
import qualified Unison.Util.List as List
import Unison.Var (Var)
import Unison.WatchKind (WatchKind, pattern TestWatch)

dataDeclarations :: UnisonFile v a -> Map v (Reference, DataDeclaration v a)
dataDeclarations = fmap (first Reference.DerivedId) . dataDeclarationsId

effectDeclarations :: UnisonFile v a -> Map v (Reference, EffectDeclaration v a)
effectDeclarations = fmap (first Reference.DerivedId) . effectDeclarationsId

watchesOfKind :: WatchKind -> UnisonFile v a -> [(v, a, Term v a)]
watchesOfKind kind uf = Map.findWithDefault [] kind (watches uf)

watchesOfOtherKinds :: WatchKind -> UnisonFile v a -> [(v, a, Term v a)]
watchesOfOtherKinds kind uf =
  join [ws | (k, ws) <- Map.toList (watches uf), k /= kind]

allWatches :: UnisonFile v a -> [(v, a, Term v a)]
allWatches = join . Map.elems . watches

-- Converts a file to a single let rec with a body of `()`, for
-- purposes of typechecking.
typecheckingTerm :: (Var v, Monoid a) => UnisonFile v a -> Term v a
typecheckingTerm uf =
  Term.letRec' True bindings $
    DD.unitTerm mempty
  where
    bindings =
      terms uf <> testWatches <> watchesOfOtherKinds TestWatch uf
    -- we make sure each test has type Test.Result
    f w = let wa = ABT.annotation w in Term.ann wa w (DD.testResultType wa)
    testWatches = map (second f) $ watchesOfKind TestWatch uf

-- backwards compatibility with the old data type
dataDeclarations' :: TypecheckedUnisonFile v a -> Map v (Reference, DataDeclaration v a)
dataDeclarations' = fmap (first Reference.DerivedId) . dataDeclarationsId'

effectDeclarations' :: TypecheckedUnisonFile v a -> Map v (Reference, EffectDeclaration v a)
effectDeclarations' = fmap (first Reference.DerivedId) . effectDeclarationsId'

hashTerms :: TypecheckedUnisonFile v a -> Map v (a, Reference, Maybe WatchKind, Term v a, Type v a)
hashTerms = fmap (over _2 Reference.DerivedId) . hashTermsId

typecheckedUnisonFile ::
  forall v a.
  (Var v) =>
  Map v (Reference.Id, DataDeclaration v a) ->
  Map v (Reference.Id, EffectDeclaration v a) ->
  [[(v, a, Term v a, Type v a)]] ->
  [(WatchKind, [(v, a, Term v a, Type v a)])] ->
  TypecheckedUnisonFile v a
typecheckedUnisonFile datas effects tlcs watches =
  TypecheckedUnisonFileId datas effects tlcs watches hashImpl
  where
    hashImpl :: (Map v (a, Reference.Id, Maybe WatchKind, Term v a, Type v a))
    hashImpl =
      let -- includes watches
          allTerms :: [(v, a, Term v a, Type v a)]
          allTerms = join tlcs ++ join (snd <$> watches)
          types :: Map v (Type v a)
          types = Map.fromList [(v, t) | (v, _a, _, t) <- allTerms]
          watchKinds :: Map v (Maybe WatchKind)
          watchKinds =
            Map.fromList $
              [(v, Nothing) | (v, _a, _e, _t) <- join tlcs]
                ++ [(v, Just wk) | (wk, wkTerms) <- watches, (v, _a, _e, _t) <- wkTerms]
          hcs :: Map v (Reference.Id, Term v a, Type v a, a)
          hcs = Hashing.hashTermComponents $ Map.fromList $ (\(v, a, e, t) -> (v, (e, t, a))) <$> allTerms
       in Map.fromList
            [ (v, (a, r, wk, e, t))
              | (v, (r, e, _typ, a)) <- Map.toList hcs,
                Just t <- [Map.lookup v types],
                wk <- [Map.findWithDefault (error $ show v ++ " missing from watchKinds") v watchKinds]
            ]

lookupDecl ::
  (Ord v) =>
  v ->
  TypecheckedUnisonFile v a ->
  Maybe (Reference.Id, DD.Decl v a)
lookupDecl v uf =
  over _2 Right <$> (Map.lookup v (dataDeclarationsId' uf))
    <|> over _2 Left <$> (Map.lookup v (effectDeclarationsId' uf))

indexByReference ::
  TypecheckedUnisonFile v a ->
  (Map Reference.Id (a, Term v a, Type v a), Map Reference.Id (DD.Decl v a))
indexByReference uf = (tms, tys)
  where
    tys =
      Map.fromList (over _2 Right <$> toList (dataDeclarationsId' uf))
        <> Map.fromList (over _2 Left <$> toList (effectDeclarationsId' uf))
    tms =
      Map.fromList
        [ (r, (a, tm, ty)) | (a, Reference.DerivedId r, _wk, tm, ty) <- Map.elems (hashTerms uf)
        ]

-- | A mapping of all terms in the file by their var name.
-- The returned terms refer to other definitions in the file by their
-- var, not by reference.
-- Includes test watches.
allTerms :: (Ord v) => TypecheckedUnisonFile v a -> Map v (Term v a)
allTerms uf =
  Map.fromList [(v, t) | (v, _a, t, _) <- join $ topLevelComponents uf]

-- | the top level components (no watches) plus test watches.
topLevelComponents ::
  TypecheckedUnisonFile v a ->
  [[(v, a, Term v a, Type v a)]]
topLevelComponents file =
  topLevelComponents' file ++ [comp | (TestWatch, comp) <- watchComponents file]

-- External type references that appear in the types of the file's terms
termSignatureExternalLabeledDependencies ::
  (Ord v) => TypecheckedUnisonFile v a -> Set LabeledDependency
termSignatureExternalLabeledDependencies
  (TypecheckedUnisonFile dataDeclarations' effectDeclarations' _ _ hashTerms) =
    Set.difference
      ( Set.map LD.typeRef
          . foldMap Type.dependencies
          . fmap (\(_a, _r, _wk, _e, t) -> t)
          . toList
          $ hashTerms
      )
      -- exclude any references that are defined in this file
      ( Set.fromList $
          (map (LD.typeRef . fst) . toList) dataDeclarations'
            <> (map (LD.typeRef . fst) . toList) effectDeclarations'
      )

-- Returns the dependencies of the `UnisonFile` input. Needed so we can
-- load information about these dependencies before starting typechecking.
dependencies :: (Monoid a, Var v) => UnisonFile v a -> Set Reference
dependencies (UnisonFile ds es ts ws) =
  foldMap (DD.dependencies . snd) ds
    <> foldMap (DD.dependencies . DD.toDataDecl . snd) es
    <> foldMap (Term.dependencies . view _3) ts
    <> foldMap (foldMap (Term.dependencies . view _3)) ws

discardTypes :: TypecheckedUnisonFile v a -> UnisonFile v a
discardTypes (TypecheckedUnisonFileId datas effects terms watches _) =
  let watches' = g . mconcat <$> List.multimap watches
      g tup3s = [(v, a, e) | (v, a, e, _t) <- tup3s]
   in UnisonFileId datas effects [(v, a, trm) | (v, a, trm, _typ) <- join terms] watches'

declsToTypeLookup :: (Var v) => UnisonFile v a -> TL.TypeLookup v a
declsToTypeLookup uf =
  TL.TypeLookup
    mempty
    (wrangle (dataDeclarations uf))
    (wrangle (effectDeclarations uf))
  where
    wrangle = Map.fromList . Map.elems

-- Returns true if the file has any definitions or watches
nonEmpty :: TypecheckedUnisonFile v a -> Bool
nonEmpty uf =
  not (Map.null (dataDeclarations' uf))
    || not (Map.null (effectDeclarations' uf))
    || any (not . null) (topLevelComponents' uf)
    || any (not . null) (watchComponents uf)

hashConstructors ::
  forall v a. (Ord v) => TypecheckedUnisonFile v a -> Map v Referent.Id
hashConstructors file =
  let ctors1 =
        Map.elems (dataDeclarationsId' file) >>= \(ref, dd) ->
          [(v, Referent.ConId (ConstructorReference ref i) CT.Data) | (v, i) <- DD.constructorVars dd `zip` [0 ..]]
      ctors2 =
        Map.elems (effectDeclarationsId' file) >>= \(ref, dd) ->
          [(v, Referent.ConId (ConstructorReference ref i) CT.Effect) | (v, i) <- DD.constructorVars (DD.toDataDecl dd) `zip` [0 ..]]
   in Map.fromList (ctors1 ++ ctors2)

-- | Returns the set of constructor names for decls whose names in the given Set.
constructorsForDecls :: (Ord v) => Set v -> TypecheckedUnisonFile v a -> Set v
constructorsForDecls types uf =
  let dataConstructors =
        dataDeclarationsId' uf
          & Map.filterWithKey (\k _ -> Set.member k types)
          & Map.elems
          & fmap snd
          & concatMap DD.constructorVars
      effectConstructors =
        effectDeclarationsId' uf
          & Map.filterWithKey (\k _ -> Set.member k types)
          & Map.elems
          & fmap (DD.toDataDecl . snd)
          & concatMap DD.constructorVars
   in Set.fromList (dataConstructors <> effectConstructors)
