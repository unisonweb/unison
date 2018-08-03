{-# Language ScopedTypeVariables, OverloadedStrings #-}

module Unison.UnisonFile where

import qualified Data.Foldable as Foldable
import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Unison.Reference (Reference)
import Unison.DataDeclaration (DataDeclaration')
import Unison.DataDeclaration (EffectDeclaration'(..))
import Unison.DataDeclaration (hashDecls, toDataDecl, withEffectDecl)
import qualified Unison.DataDeclaration as DD
import qualified Data.Text as Text
import Unison.Term (AnnotatedTerm, AnnotatedTerm2)
import qualified Unison.Term as Term
import qualified Data.Set as Set
import Data.Set (Set)
import Unison.Var (Var)
import qualified Unison.Var as Var

data UnisonFile v a = UnisonFile {
  dataDeclarations :: Map v (Reference, DataDeclaration' v a),
  effectDeclarations :: Map v (Reference, EffectDeclaration' v a),
  term :: AnnotatedTerm v a
} deriving (Show)

type CtorLookup = Map String (Reference, Int)

bindBuiltins :: Var v
             => [(v, AnnotatedTerm v a)]
             -> [(v, Reference)]
             -> UnisonFile v a
             -> UnisonFile v a
bindBuiltins termBuiltins typeBuiltins (UnisonFile d e t) =
  UnisonFile
    (second (DD.bindBuiltins typeBuiltins) <$> d)
    (second (withEffectDecl (DD.bindBuiltins typeBuiltins)) <$> e)
    (Term.bindBuiltins termBuiltins typeBuiltins t)

data Env v a = Env
  -- Data declaration name to hash and its fully resolved form
  { datas   :: Map v (Reference, DataDeclaration' v a)
  -- Effect declaration name to hash and its fully resolved form
  , effects :: Map v (Reference, EffectDeclaration' v a)
  -- Substitutes away any free variables bound by `datas` or `effects`
  --   (for instance, free type variables or free term variables that
  --    reference constructors of `datas` or `effects`)
  , resolveTerm :: AnnotatedTerm v a -> AnnotatedTerm v a
  -- All known types mapped to their hash, indexed by name
  , typesByName :: Map v Reference
  -- `String` to `(Reference, ConstructorId)`
  , constructorLookup :: CtorLookup
  -- All variables declarated in this environment
  , locallyBound :: Set v
}

-- This function computes hashes for data and effect declarations, and
-- also returns a function for resolving strings to (Reference, ConstructorId)
-- for parsing of pattern matching
environmentFor :: forall v a . Var v
               => [(v, AnnotatedTerm v a)]
               -> [(v, Reference)]
               -> Map v (DataDeclaration' v a)
               -> Map v (EffectDeclaration' v a)
               -> Env v a
environmentFor termBuiltins typeBuiltins0 dataDecls0 effectDecls0 =
  let -- ignore builtin types that will be shadowed by user-defined data/effects
      typeBuiltins = [ (v, t) | (v, t) <- typeBuiltins0,
                                Map.notMember v dataDecls0 &&
                                Map.notMember v effectDecls0 ]
      dataDecls :: Map v (DataDeclaration' v a)
      dataDecls = DD.bindBuiltins typeBuiltins <$> dataDecls0
      effectDecls :: Map v (EffectDeclaration' v a)
      effectDecls = withEffectDecl (DD.bindBuiltins typeBuiltins)
                <$> effectDecls0
      hashDecls' :: [(v, Reference, DataDeclaration' v a)]
      hashDecls' = hashDecls (Map.union dataDecls (toDataDecl <$> effectDecls))
      allDecls = Map.fromList [ (v, (r,de)) | (v,r,de) <- hashDecls' ]
      dataDecls' = Map.difference allDecls effectDecls
      effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
      typeEnv :: [(v, Reference)]
      typeEnv = Map.toList (fst <$> dataDecls') ++ Map.toList (fst <$> effectDecls')
      dataDecls'' = second (DD.bindBuiltins typeEnv) <$> dataDecls'
      effectDecls'' =
        second (DD.withEffectDecl (DD.bindBuiltins typeEnv)) <$> effectDecls'
      dataRefs = Set.fromList $ (fst <$> Foldable.toList dataDecls'')
      termFor :: Reference -> Int -> AnnotatedTerm2 v a a v ()
      termFor r cid = if Set.member r dataRefs then Term.constructor() r cid
                      else Term.request() r cid
      -- Map `Optional.None` to `Term.constructor() ...` or `Term.request() ...`
      dataAndEffectCtors :: [(v, AnnotatedTerm2 v a a v ())]
      dataAndEffectCtors = [
        (Var.named (Text.pack s), termFor r cid) | (s, (r,cid)) <- Map.toList ctorLookup ]
      typesByName = Map.toList $ (fst <$> dataDecls'') `Map.union` (fst <$> effectDecls'')
      ctorLookup = Map.fromList (constructors' =<< hashDecls')
  in Env dataDecls'' effectDecls''
         (Term.typeDirectedResolve .
            Term.bindBuiltins termBuiltins [] .
            Term.bindBuiltins dataAndEffectCtors typesByName)
         (Map.fromList $ typeBuiltins ++ typesByName)
         ctorLookup
         (Set.fromList $ (fst <$> termBuiltins) ++ (fst <$> dataAndEffectCtors))

constructors' :: Var v => (v, Reference, DataDeclaration' v a) -> [(String, (Reference, Int))]
constructors' (typeSymbol, r, dd) =
  let qualCtorName ((ctor,_), i) =
       ( Text.unpack $
           mconcat [Var.qualifiedName typeSymbol, ".", Var.qualifiedName ctor]
       , (r, i))
  in qualCtorName <$> DD.constructors dd `zip` [0..]
