{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.UnisonFile where

import           Control.Applicative    ((<|>))
import           Control.Exception      (assert)
import           Control.Monad          (join)
import           Data.Bifunctor         (second)
import           Data.Foldable          (toList, foldl')
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import qualified Data.Set               as Set
import Data.Set (Set)
import qualified Data.Text              as Text
import           Unison.DataDeclaration (DataDeclaration')
import           Unison.DataDeclaration (EffectDeclaration' (..))
import           Unison.DataDeclaration (hashDecls, toDataDecl, withEffectDecl)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Name            as Name
import           Unison.Names           (Names)
import qualified Unison.Names           as Names
import           Unison.Reference       (Reference)
import           Unison.Referent        (Referent)
import qualified Unison.Referent        as Referent
import           Unison.Term            (AnnotatedTerm)
import qualified Unison.Term            as Term
import           Unison.Type            (AnnotatedType)
import qualified Unison.Type            as Type
import           Unison.Util.Relation   (Relation)
import qualified Unison.Util.Relation   as Relation
import           Unison.Var             (Var)
import qualified Unison.Var             as Var
import qualified Unison.Typechecker.TypeLookup as TL

data UnisonFile v a = UnisonFile {
  dataDeclarations   :: Map v (Reference, DataDeclaration' v a),
  effectDeclarations :: Map v (Reference, EffectDeclaration' v a),
  term               :: AnnotatedTerm v a
} deriving (Show)

-- A UnisonFile after typechecking. Terms are split into groups by
-- cycle and the type of each term is known.
data TypecheckedUnisonFile v a =
  -- Giving this an ugly name to encourage use of lowercase smart ctor
  -- which filters out watch expressions
  TypecheckedUnisonFile_ {
    dataDeclarations'   :: Map v (Reference, DataDeclaration' v a),
    effectDeclarations' :: Map v (Reference, EffectDeclaration' v a),
    topLevelComponents  :: [[(v, AnnotatedTerm v a, AnnotatedType v a)]]
  } deriving Show

-- A UnisonFile after typechecking. Inlcludes a top-level term and its type.
data TypecheckedUnisonFile' v a = TypecheckedUnisonFile' {
  dataDeclarations''   :: Map v (Reference, DataDeclaration' v a),
  effectDeclarations'' :: Map v (Reference, EffectDeclaration' v a),
  topLevelComponents'  :: [[(v, AnnotatedTerm v a, AnnotatedType v a)]],
  topLevelTerm :: AnnotatedTerm v a,
  typ :: AnnotatedType v a
} deriving Show

getDecl' :: Ord v => TypecheckedUnisonFile v a -> v -> Maybe (TL.Decl v a)
getDecl' uf v =
  (Right . snd <$> Map.lookup v (dataDeclarations' uf)) <|>
  (Left . snd <$> Map.lookup v (effectDeclarations' uf))

discardTopLevelTerm :: TypecheckedUnisonFile' v a -> TypecheckedUnisonFile v a
discardTopLevelTerm (TypecheckedUnisonFile' datas effects components _ _) =
  TypecheckedUnisonFile_ datas effects components

-- Returns a relation for the dependencies of this file. The domain is
-- the dependent, and the range is its dependencies, thus:
-- `R.lookupDom r (dependencies file)` returns the set of dependencies
-- of the reference `r`.
dependencies' ::
  forall v a. Var v => TypecheckedUnisonFile v a -> Relation Reference Reference
dependencies' file = let
  terms :: Map v (Reference, AnnotatedTerm v a, AnnotatedType v a)
  terms = hashTerms file
  decls :: Map v (Reference, DataDeclaration' v a)
  decls = dataDeclarations' file <>
          fmap (second toDataDecl) (effectDeclarations' file )
  termDeps = foldl' f Relation.empty $ toList terms
  allDeps = foldl' g termDeps $ toList decls
  f acc (r, tm, tp) = acc <> termDeps <> typeDeps
    where termDeps =
            Relation.fromList [ (r, dep) | dep <- toList (Term.dependencies tm)]
          typeDeps =
            Relation.fromList [ (r, dep) | dep <- toList (Type.dependencies tp)]
  g acc (r, decl) = acc <> ctorDeps
    where ctorDeps =
            Relation.fromList [ (r, dep) | (_, _, tp) <- DD.constructors' decl
                                         , dep <- toList (Type.dependencies tp)
                                         ]
  in allDeps

-- Returns the (termRefs, typeRefs) that the input `UnisonFile` depends on.
dependencies :: Var v => UnisonFile v a -> Names -> Set Reference
dependencies uf ns = directReferences <>
                      Set.fromList freeTypeVarRefs <>
                      Set.fromList freeTermVarRefs
  where
    tm = term uf
    directReferences = Term.dependencies tm
    freeTypeVarRefs = -- we aren't doing any special resolution for types
      catMaybes (flip Map.lookup (Names.typeNames ns) . Name.fromVar <$>
                  Set.toList (Term.freeTypeVars tm))
    -- foreach name in Names.termNames,
        -- if the name or unqualified name is in Term.freeVars,
        -- include the reference
    freeTermVarRefs =
      [ Referent.toReference referent
      | (name, referent) <- Map.toList $ Names.termNames ns
      , Name.toVar name `Set.member` Term.freeVars tm
        || Var.unqualified (Name.toVar name) `Set.member` Term.freeVars tm
      ]

discardTypes :: AnnotatedTerm v a -> TypecheckedUnisonFile v a -> UnisonFile v a
discardTypes tm (TypecheckedUnisonFile_ datas effects _) =
  UnisonFile datas effects tm

discardTypes' :: TypecheckedUnisonFile' v a -> UnisonFile v a
discardTypes' (TypecheckedUnisonFile' datas effects _ tm _) =
  UnisonFile datas effects tm

discardTerm :: Var v => TypecheckedUnisonFile' v a -> TypecheckedUnisonFile v a
discardTerm (TypecheckedUnisonFile' datas effects tlcs _ _) =
  typecheckedUnisonFile datas effects tlcs

declsToTypeLookup :: Var v => UnisonFile v a -> TL.TypeLookup v a
declsToTypeLookup uf = TL.TypeLookup mempty
                          (wrangle (dataDeclarations uf))
                          (wrangle (effectDeclarations uf))
  where wrangle = Map.fromList . Map.elems

toNames :: Var v => UnisonFile v a -> Names
toNames (UnisonFile {..}) = datas <> effects
  where
    datas = foldMap DD.dataDeclToNames' (Map.toList dataDeclarations)
    effects = foldMap DD.effectDeclToNames' (Map.toList effectDeclarations)

typecheckedUnisonFile0 :: TypecheckedUnisonFile v a
typecheckedUnisonFile0 = TypecheckedUnisonFile_ Map.empty Map.empty mempty

typecheckedUnisonFile
  :: Var v
  => Map v (Reference, DataDeclaration' v a)
  -> Map v (Reference, EffectDeclaration' v a)
  -> [[(v, AnnotatedTerm v a, AnnotatedType v a)]]
  -> TypecheckedUnisonFile v a
typecheckedUnisonFile ds es cs = TypecheckedUnisonFile_ ds es (removeWatches cs)
 where
  -- todo: more robust way of doing this once we have different kinds of variables
  removeWatches = filter (not . null) . fmap filterDefs
  filterDefs    = filter (\(v, _, _) -> Text.take 1 (Var.name v) /= "_")

hashConstructors
  :: forall v a. Var v => TypecheckedUnisonFile v a -> Map v Referent
hashConstructors file =
  let ctors1 = Map.elems (dataDeclarations' file) >>= \(ref, dd) ->
        [ (v, Referent.Con ref i) | (v,i) <- DD.constructorVars dd `zip` [0 ..] ]
      ctors2 = Map.elems (effectDeclarations' file) >>= \(ref, dd) ->
        [ (v, Referent.Req ref i) | (v,i) <- DD.constructorVars (DD.toDataDecl dd) `zip` [0 ..] ]
  in Map.fromList (ctors1 ++ ctors2)

hashTerms ::
     Var v
  => TypecheckedUnisonFile v a
  -> Map v (Reference, AnnotatedTerm v a, AnnotatedType v a)
hashTerms file = let
  components = topLevelComponents file
  types = Map.fromList [(v,t) | (v,_,t) <- join components ]
  terms0 = Map.fromList [(v,e) | (v,e,_) <- join components ]
  hcs = Term.hashComponents terms0
  in Map.fromList [ (v, (r, e, t)) | (v, (r, e)) <- Map.toList hcs,
                                     Just t <- [Map.lookup v types] ]

type CtorLookup = Map String (Reference, Int)

bindBuiltins :: Var v
             => Names
             -> UnisonFile v a
             -> UnisonFile v a
bindBuiltins names (UnisonFile d e t) =
  UnisonFile
    (second (DD.bindBuiltins names) <$> d)
    (second (withEffectDecl (DD.bindBuiltins names)) <$> e)
    (Names.bindTerm names t)

filterVars
  :: Var v
  => Set v
  -> Set v
  -> TypecheckedUnisonFile v a
  -> TypecheckedUnisonFile v a
filterVars types terms file = TypecheckedUnisonFile_
  (dataDeclarations' file `Map.restrictKeys` types)
  (effectDeclarations' file `Map.restrictKeys` types)
  (filter (any (\(v, _, _) -> Set.member v terms)) $ topLevelComponents file)

data Env v a = Env
  -- Data declaration name to hash and its fully resolved form
  { datas   :: Map v (Reference, DataDeclaration' v a)
  -- Effect declaration name to hash and its fully resolved form
  , effects :: Map v (Reference, EffectDeclaration' v a)
  -- Naming environment
  , names   :: Names
}

-- This function computes hashes for data and effect declarations, and
-- also returns a function for resolving strings to (Reference, ConstructorId)
-- for parsing of pattern matching
--
-- If there are duplicate declarations, the duplicated names are returned on the
-- left.
environmentFor
  :: forall v a . Var v => Names -> Map v (DataDeclaration' v a) -> Map v (EffectDeclaration' v a)
  -> Env v a
environmentFor names0 dataDecls0 effectDecls0 =
  let
    -- ignore builtin types that will be shadowed by user-defined data/effects
    unshadowed n = Map.notMember (Name.toVar n) dataDecls0
                && Map.notMember (Name.toVar n) effectDecls0
    names = Names.filterTypes unshadowed names0
    -- data decls and hash decls may reference each other, and thus must be hashed together
    dataDecls :: Map v (DataDeclaration' v a)
    dataDecls = DD.bindBuiltins names <$> dataDecls0
    effectDecls :: Map v (EffectDeclaration' v a)
    effectDecls = withEffectDecl (DD.bindBuiltins names) <$> effectDecls0
    hashDecls' :: [(v, Reference, DataDeclaration' v a)]
    hashDecls' = hashDecls (Map.union dataDecls (toDataDecl <$> effectDecls))
    -- then we have to pick out the dataDecls from the effectDecls
    allDecls   = Map.fromList [ (v, (r, de)) | (v, r, de) <- hashDecls' ]
    dataDecls' = Map.difference allDecls effectDecls
    effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
    -- ctor and effect terms
    ctors = foldMap DD.dataDeclToNames' (Map.toList dataDecls')
    effects = foldMap DD.effectDeclToNames' (Map.toList effectDecls')
    names' = ctors <> effects <> names
  in
    -- make sure we don't have overlapping data and effect constructor names
    assert (null (Set.intersection (Map.keysSet dataDecls) (Map.keysSet effectDecls))) $
      Env dataDecls' effectDecls' names'
