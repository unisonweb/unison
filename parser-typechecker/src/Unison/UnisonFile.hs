{-# Language ScopedTypeVariables, OverloadedStrings #-}

module Unison.UnisonFile where

import Control.Monad (join)
import           Data.Bifunctor (second)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import           Unison.DataDeclaration (DataDeclaration')
import           Unison.DataDeclaration (EffectDeclaration'(..))
import           Unison.DataDeclaration (hashDecls, toDataDecl, withEffectDecl)
import qualified Unison.DataDeclaration as DD
import           Unison.Reference (Reference)
import qualified Unison.Term as Term
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import           Unison.Var (Var)
import qualified Unison.Var as Var
import Unison.Names (Names)
import qualified Unison.Names as Names

data UnisonFile v a = UnisonFile {
  dataDeclarations :: Map v (Reference, DataDeclaration' v a),
  effectDeclarations :: Map v (Reference, EffectDeclaration' v a),
  term :: AnnotatedTerm v a
} deriving (Show)

-- A UnisonFile after typechecking. Terms are split into groups by
-- cycle and the type of each term is known.
data TypecheckedUnisonFile v a = TypecheckedUnisonFile {
  dataDeclarations' :: Map v (Reference, DataDeclaration' v a),
  effectDeclarations' :: Map v (Reference, EffectDeclaration' v a),
  terms :: [[(v, AnnotatedTerm v a, AnnotatedType v a)]]
}

typecheckedUnisonFile0 :: TypecheckedUnisonFile v a
typecheckedUnisonFile0 = TypecheckedUnisonFile Map.empty Map.empty mempty

typecheckedUnisonFile ::
     Var v
  => Map v (Reference, DataDeclaration' v a)
  -> Map v (Reference, EffectDeclaration' v a)
  -> [[(v, AnnotatedTerm v a, AnnotatedType v a)]]
  -> TypecheckedUnisonFile v a
typecheckedUnisonFile ds es cs =
  TypecheckedUnisonFile ds es (removeWatches cs)
  where
  -- todo: more robust way of doing this once we have different kinds of variables
  removeWatches = filter (not . null) . fmap filterDefs
  filterDefs = filter (\(v, _, _) -> Text.take 1 (Var.name v) /= "_")

hashConstructors :: Var v => TypecheckedUnisonFile v a -> Map v (Reference, AnnotatedTerm v ())
hashConstructors file = let
  ctors1 = Map.elems (dataDeclarations' file) >>= \(ref, dd) ->
              [ (v, Term.constructor() ref i) | (v, i) <- DD.constructorVars dd `zip` [0..] ]
  ctors2 = Map.elems (effectDeclarations' file) >>= \(ref, dd) ->
              [ (v, Term.constructor() ref i) | (v, i) <- DD.constructorVars (DD.toDataDecl dd) `zip` [0..] ]
  in Term.hashComponents (Map.fromList $ ctors1 ++ ctors2)

hashTerms ::
     Var v
  => TypecheckedUnisonFile v a
  -> Map v (Reference, AnnotatedTerm v a, AnnotatedType v a)
hashTerms file = let
  components = terms file
  types = Map.fromList [(v,t) | (v,_,t) <- join components ]
  terms0 = Map.fromList [(v,e) | (v,e,_) <- join components ]
  hcs = Term.hashComponents terms0
  in Map.fromList [ (v, (r, e, t)) | (v, (r, e)) <- Map.toList hcs,
                                     Just t <- [Map.lookup v types] ]

type CtorLookup = Map String (Reference, Int)

bindBuiltins :: Var v
             => Names v a
             -> UnisonFile v a
             -> UnisonFile v a
bindBuiltins names (UnisonFile d e t) =
  UnisonFile
    (second (DD.bindBuiltins names) <$> d)
    (second (withEffectDecl (DD.bindBuiltins names)) <$> e)
    (Names.bindTerm names t)

data Env v a = Env
  -- Data declaration name to hash and its fully resolved form
  { datas   :: Map v (Reference, DataDeclaration' v a)
  -- Effect declaration name to hash and its fully resolved form
  , effects :: Map v (Reference, EffectDeclaration' v a)
  -- Naming environment
  , names :: Names v a
}

resolveTerm :: Var v => Env v a -> AnnotatedTerm v a -> AnnotatedTerm v a
resolveTerm env e =
  Term.prepareTDNR $ Names.bindTerm (names env) e

-- This function computes hashes for data and effect declarations, and
-- also returns a function for resolving strings to (Reference, ConstructorId)
-- for parsing of pattern matching
--
-- If there are duplicate declarations, the duplicated names are returned on the
-- left.
environmentFor
  :: forall v a . Var v => Names v a -> Map v (DataDeclaration' v a) -> Map v (EffectDeclaration' v a)
  -> Env v a
environmentFor names0 dataDecls0 effectDecls0 =
  let
    -- ignore builtin types that will be shadowed by user-defined data/effects
    unshadowed n = Map.notMember (Var.named n) dataDecls0
                && Map.notMember (Var.named n) effectDecls0
    names = Names.filterTypes unshadowed names0
    dataDecls :: Map v (DataDeclaration' v a)
    dataDecls = DD.bindBuiltins names <$> dataDecls0
    effectDecls :: Map v (EffectDeclaration' v a)
    effectDecls = withEffectDecl (DD.bindBuiltins names) <$> effectDecls0
    hashDecls' :: [(v, Reference, DataDeclaration' v a)]
    hashDecls' = hashDecls (Map.union dataDecls (toDataDecl <$> effectDecls))
    allDecls   = Map.fromList [ (v, (r, de)) | (v, r, de) <- hashDecls' ]
    dataDecls' = Map.difference allDecls effectDecls
    effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
    typeEnv :: forall x . Names v x
    typeEnv = Names.fromTypesV $
      Map.toList (fst <$> dataDecls') ++ Map.toList (fst <$> effectDecls')
    ctors = foldMap constructors' hashDecls'
    names' = typeEnv <> ctors <> names
  in Env dataDecls' effectDecls' names'

constructors'
  :: Var v
  => (v, Reference, DataDeclaration' v a)
  -> Names v a
constructors' (typeSymbol, r, dd) = let
  names ((ctor, typ), i) = let
    name = mconcat [Var.qualifiedName typeSymbol, ".", Var.qualifiedName ctor]
    in Names.fromTerms [(name, (Term.constructor (ABT.annotation typ) r i, typ))] <>
       Names.fromPatterns [(name, (r,i))]
  in foldMap names (DD.constructors dd `zip` [0 ..])
