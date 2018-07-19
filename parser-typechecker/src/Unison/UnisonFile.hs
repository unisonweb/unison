{-# Language OverloadedStrings #-}

module Unison.UnisonFile where

import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Unison.Reference (Reference)
import Unison.DataDeclaration (DataDeclaration, DataDeclaration')
import Unison.DataDeclaration (EffectDeclaration, EffectDeclaration'(..))
import Unison.DataDeclaration (hashDecls, toDataDecl, withEffectDecl)
import qualified Unison.DataDeclaration as DD
import qualified Data.Text as Text
import qualified Unison.Type as Type
import Unison.Term (Term,AnnotatedTerm)
import qualified Unison.Term as Term
import Unison.Type (Type,AnnotatedType)
import qualified Data.Set as Set
import Unison.Var (Var)
import qualified Unison.Var as Var

data UnisonFile v = UnisonFile {
  dataDeclarations :: Map v (Reference, DataDeclaration v),
  effectDeclarations :: Map v (Reference, EffectDeclaration v),
  term :: Term v
} deriving (Show)

type CtorLookup = Map String (Reference, Int)

bindBuiltins :: Var v => [(v, Reference)] -> [(v, Reference)] -> UnisonFile v
                      -> UnisonFile v
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
  -- Substitutes away any free variables bound by `datas` or `effects`
  , resolveType :: AnnotatedType v a -> AnnotatedType v a
  -- `String` to `(Reference, ConstructorId)`
  , constructorLookup :: CtorLookup
}

-- This function computes hashes for data and effect declarations, and
-- also returns a function for resolving strings to (Reference, ConstructorId)
-- for parsing of pattern matching
environmentFor :: Var v
               => [(v, Reference)]
               -> Map v (DataDeclaration v)
               -> Map v (EffectDeclaration v)
               -> Env v ()
environmentFor typeBuiltins0 dataDecls0 effectDecls0 =
  let -- ignore builtin types that will be shadowed by user-defined data/effects
      typeBuiltins = [ (v, t) | (v, t) <- typeBuiltins0,
                                Map.notMember v dataDecls0 &&
                                Map.notMember v effectDecls0]
      dataDecls = DD.bindBuiltins typeBuiltins <$> dataDecls0
      effectDecls = withEffectDecl (DD.bindBuiltins typeBuiltins)
                <$> effectDecls0
      hashDecls' = hashDecls (Map.union dataDecls (toDataDecl <$> effectDecls))
      allDecls = Map.fromList [ (v, (r,de)) | (v,r,de) <- hashDecls' ]
      dataDecls' = Map.difference allDecls effectDecls
      effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
      typeEnv = Map.toList (fst <$> dataDecls') ++
                Map.toList (fst <$> effectDecls')
      dataDecls'' = second (DD.bindBuiltins typeEnv) <$> dataDecls'
      effectDecls'' = second (DD.withEffectDecl (DD.bindBuiltins typeEnv)) <$> effectDecls'
      dataRefs = Set.fromList $ (fst <$> Map.toList dataDecls'')
      effectRefs = Set.fromList $ (fst <$> Map.toList effectDecls'')
      termFor r cid = if Set.member r dataRefs then Term.constructor() r cid
                      else Term.request() r cid
      termsByName = Map.fromList [
        (Var.named (Text.pack s), termFor r cid) | (s, (r,cid)) <- Map.toList ctorLookup ]
      typesByName = (fst <$> dataDecls'') `Map.union` (fst <$> effectDecls'')
      ctorLookup = Map.fromList (constructors' =<< hashDecls')
  in Env dataDecls'' effectDecls''
         (Term.bindBuiltins termsByName typesByName)
         (Type.bindBuiltins typesByName)
         ctorLookup

constructors' :: Var v => (v, Reference, DataDeclaration v) -> [(String, (Reference, Int))]
constructors' (typeSymbol, r, dd) =
  let qualCtorName ((ctor,_), i) =
       (Text.unpack $ mconcat [Var.qualifiedName typeSymbol, ".", Var.qualifiedName ctor], (r, i))
  in qualCtorName <$> DD.constructors dd `zip` [0..]
