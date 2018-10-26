{-# Language ScopedTypeVariables, OverloadedStrings #-}

module Unison.UnisonFile where

import Control.Monad (join)
import           Data.Bifunctor (second)
import qualified Data.Foldable as Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Unison.DataDeclaration (DataDeclaration')
import           Unison.DataDeclaration (EffectDeclaration'(..))
import           Unison.DataDeclaration (hashDecls, toDataDecl, withEffectDecl)
import qualified Unison.DataDeclaration as DD
import           Unison.Reference (Reference)
import           Unison.Term (AnnotatedTerm, AnnotatedTerm2)
import qualified Unison.Term as Term
import           Unison.Type (AnnotatedType)
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Reference as Reference

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

-- Convenience function for extracting the term and type decl components
-- of a Unison file. This would be presented to the user for adding to
-- the codebase.
components :: Var v => TypecheckedUnisonFile v a ->
  ([[((v, AnnotatedTerm v a, AnnotatedType v a), Reference)]],
   [[((v, Either (EffectDeclaration' v a) (DataDeclaration' v a)), Reference)]])
components uf = let
  tms = [ ((v,e,t), r) | (v, (r,e,t)) <- Map.toList $ hashTerms (terms uf) ]
  ds = [ ((v, Right d), r) | (v, (r, d)) <- Map.toList $ dataDeclarations' uf ] ++
       [ ((v, Left eff), r) | (v, (r, eff)) <- Map.toList $ effectDeclarations' uf ]
  in (Reference.groupByComponent tms, Reference.groupByComponent ds)

hashTerms ::
     Var v
  => [[(v, AnnotatedTerm v a, AnnotatedType v a)]]
  -> Map v (Reference, AnnotatedTerm v a, AnnotatedType v a)
hashTerms components = let
  types = Map.fromList [(v,t) | (v,_,t) <- join components ]
  terms = Map.fromList [(v,e) | (v,e,_) <- join components ]
  ref r = Term.ref() r
  hcs = Reference.hashComponents ref terms
  in Map.fromList [ (v, (r, e, t)) | (v, (r, e)) <- Map.toList hcs,
                                     Just t <- [Map.lookup v types] ]

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
--
-- If there are duplicate declarations, the duplicated names are returned on the
-- left.
environmentFor
  :: forall v a
   . Var v
  => [(v, AnnotatedTerm v a)]
  -> [(v, Reference)]
  -> Map v (DataDeclaration' v a)
  -> Map v (EffectDeclaration' v a)
  -> Env v a
environmentFor termBuiltins typeBuiltins0 dataDecls0 effectDecls0 =
  let -- ignore builtin types that will be shadowed by user-defined data/effects
    typeBuiltins =
      [ (v, t)
      | (v, t) <- typeBuiltins0
      , Map.notMember v dataDecls0 && Map.notMember v effectDecls0
      ]
    dataDecls :: Map v (DataDeclaration' v a)
    dataDecls = DD.bindBuiltins typeBuiltins <$> dataDecls0
    effectDecls :: Map v (EffectDeclaration' v a)
    effectDecls =
      withEffectDecl (DD.bindBuiltins typeBuiltins) <$> effectDecls0
    hashDecls' :: [(v, Reference, DataDeclaration' v a)]
    hashDecls' = hashDecls (Map.union dataDecls (toDataDecl <$> effectDecls))
    allDecls   = Map.fromList [ (v, (r, de)) | (v, r, de) <- hashDecls' ]
    dataDecls' = Map.difference allDecls effectDecls
    effectDecls' =
      second EffectDeclaration <$> Map.difference allDecls dataDecls
    typeEnv :: [(v, Reference)]
    typeEnv =
      Map.toList (fst <$> dataDecls') ++ Map.toList (fst <$> effectDecls')
    dataDecls'' = second (DD.bindBuiltins typeEnv) <$> dataDecls'
    effectDecls'' =
      second (DD.withEffectDecl (DD.bindBuiltins typeEnv))
        <$> effectDecls'
    dataRefs = Set.fromList $ (fst <$> Foldable.toList dataDecls'')
    termFor :: Reference -> Int -> AnnotatedTerm2 v a a v ()
    termFor r cid = if Set.member r dataRefs
      then Term.constructor () r cid
      else Term.request () r cid
    -- Map `Optional.None` to `Term.constructor() ...` or `Term.request() ...`
    dataAndEffectCtors :: [(v, AnnotatedTerm2 v a a v ())]
    dataAndEffectCtors =
      [ (Var.named (Text.pack s), termFor r cid)
      | (s, (r, cid)) <- Map.toList ctorLookup
      ]
    typesByName =
      Map.toList
        $           (fst <$> dataDecls'')
        `Map.union` (fst <$> effectDecls'')
    ctorLookup = Map.fromList (constructors' =<< hashDecls')
  in
    Env
      dataDecls''
      effectDecls''
      ( Term.typeDirectedResolve
      . Term.bindBuiltins termBuiltins []
      . Term.bindBuiltins dataAndEffectCtors typesByName
      )
      (Map.fromList $ typeBuiltins ++ typesByName)
      ctorLookup
      (  Set.fromList
      $  (fst <$> termBuiltins)
      ++ (fst <$> dataAndEffectCtors)
      )

constructors'
  :: Var v
  => (v, Reference, DataDeclaration' v a)
  -> [(String, (Reference, Int))]
constructors' (typeSymbol, r, dd) =
  let qualCtorName ((ctor, _), i) =
        ( Text.unpack $ mconcat
          [Var.qualifiedName typeSymbol, ".", Var.qualifiedName ctor]
        , (r, i)
        )
  in  qualCtorName <$> DD.constructors dd `zip` [0 ..]
