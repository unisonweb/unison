module Unison.UnisonFile.Names where

import Control.Lens
import Data.List.Extra (nubOrd)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Names qualified as DD.Names
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Name qualified as Name
import Unison.Names (Names (..))
import Unison.Names.ResolutionResult qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name
import Unison.Term qualified as Term
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Env (Env (..))
import Unison.UnisonFile.Error (Error (DupDataAndAbility, UnknownType))
import Unison.UnisonFile.Type (TypecheckedUnisonFile (TypecheckedUnisonFileId), UnisonFile (UnisonFileId))
import Unison.Util.List qualified as List
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as WK

toNames :: (Var v) => UnisonFile v a -> Names
toNames uf = datas <> effects
  where
    datas = foldMap (DD.Names.dataDeclToNames' Name.unsafeParseVar) (Map.toList (UF.dataDeclarationsId uf))
    effects = foldMap (DD.Names.effectDeclToNames' Name.unsafeParseVar) (Map.toList (UF.effectDeclarationsId uf))

addNamesFromUnisonFile :: (Var v) => UnisonFile v a -> Names -> Names
addNamesFromUnisonFile unisonFile names = Names.shadowing (toNames unisonFile) names

typecheckedToNames :: (Var v) => TypecheckedUnisonFile v a -> Names
typecheckedToNames uf = Names (terms <> ctors) types
  where
    terms =
      Relation.fromList
        [ (Name.unsafeParseVar v, Referent.Ref r)
          | (v, (_a, r, wk, _, _)) <- Map.toList $ UF.hashTerms uf,
            wk == Nothing || wk == Just WK.TestWatch
        ]
    types =
      Relation.fromList
        [ (Name.unsafeParseVar v, r)
          | (v, r) <-
              Map.toList $
                fmap fst (UF.dataDeclarations' uf)
                  <> fmap fst (UF.effectDeclarations' uf)
        ]
    ctors =
      Relation.fromMap
        . Map.mapKeys Name.unsafeParseVar
        . fmap (fmap Reference.DerivedId)
        . UF.hashConstructors
        $ uf

addNamesFromTypeCheckedUnisonFile :: (Var v) => TypecheckedUnisonFile v a -> Names -> Names
addNamesFromTypeCheckedUnisonFile unisonFile names = Names.shadowing (typecheckedToNames unisonFile) names

typecheckedUnisonFile0 :: (Ord v) => TypecheckedUnisonFile v a
typecheckedUnisonFile0 = TypecheckedUnisonFileId Map.empty Map.empty mempty mempty mempty

-- Substitutes free type and term variables occurring in the terms of this
-- `UnisonFile` using `externalNames`.
--
-- Hash-qualified names are substituted during parsing, but non-HQ names are
-- substituted at the end of parsing, since they can be locally bound. Example, in
-- `x -> x + math.sqrt 2`, we don't know if `math.sqrt` is locally bound until
-- we are done parsing, whereas `math.sqrt#abc` can be resolved immediately
-- as it can't refer to a local definition.
bindNames ::
  (Var v) =>
  Names ->
  UnisonFile v a ->
  Names.ResolutionResult v a (UnisonFile v a)
bindNames names (UnisonFileId d e ts ws) = do
  -- todo: consider having some kind of binding structure for terms & watches
  --    so that you don't weirdly have free vars to tiptoe around.
  --    The free vars should just be the things that need to be bound externally.
  let termVarsSet = Map.keysSet ts <> Set.fromList (Map.elems ws >>= map (view _1))
  -- todo: can we clean up this lambda using something like `second`
  ts' <- traverse (\(a, t) -> (a,) <$> Term.bindNames Name.unsafeParseVar termVarsSet names t) ts
  ws' <- traverse (traverse (\(v, a, t) -> (v,a,) <$> Term.bindNames Name.unsafeParseVar termVarsSet names t)) ws
  pure $ UnisonFileId d e ts' ws'

-- | Given the set of fully-qualified variable names, this computes
-- a Map from unique suffixes to the fully qualified name.
--
-- Example, given [foo.bar, qux.bar, baz.quaffle], this returns:
--
-- Map [ foo.bar -> foo.bar
--     , qux.bar -> qux.bar
--     , baz.quaffle -> baz.quaffle
--     , quaffle -> baz.quaffle
--     ]
--
-- This is used to replace variable references with their canonical
-- fully qualified variables.
--
-- It's used below in `environmentFor` and also during the term resolution
-- process.
variableCanonicalizer :: forall v. (Var v) => [v] -> Map v v
variableCanonicalizer vs =
  done $ List.multimap do
    v <- vs
    let n = Name.unsafeParseVar v
    suffix <- Name.suffixes n
    pure (Var.named (Name.toText suffix), v)
  where
    done xs = Map.fromList [(k, v) | (k, nubOrd -> [v]) <- Map.toList xs] <> Map.fromList [(v, v) | v <- vs]

-- This function computes hashes for data and effect declarations, and
-- also returns a function for resolving strings to (Reference, ConstructorId)
-- for parsing of pattern matching
--
-- If there are duplicate declarations, the duplicated names are returned on the
-- left.
environmentFor ::
  forall v a.
  (Var v) =>
  Names ->
  Map v (DataDeclaration v a) ->
  Map v (EffectDeclaration v a) ->
  Names.ResolutionResult v a (Either [Error v a] (Env v a))
environmentFor names dataDecls0 effectDecls0 = do
  let locallyBoundTypes = variableCanonicalizer (Map.keys dataDecls0 <> Map.keys effectDecls0)
  -- data decls and hash decls may reference each other, and thus must be hashed together
  dataDecls :: Map v (DataDeclaration v a) <-
    traverse (DD.Names.bindNames Name.unsafeParseVar locallyBoundTypes names) dataDecls0
  effectDecls :: Map v (EffectDeclaration v a) <-
    traverse (DD.withEffectDeclM (DD.Names.bindNames Name.unsafeParseVar locallyBoundTypes names)) effectDecls0
  let allDecls0 :: Map v (DataDeclaration v a)
      allDecls0 = Map.union dataDecls (toDataDecl <$> effectDecls)
  hashDecls' :: [(v, Reference.Id, DataDeclaration v a)] <- Hashing.hashDataDecls allDecls0
  -- then we have to pick out the dataDecls from the effectDecls
  let allDecls = Map.fromList [(v, (r, de)) | (v, r, de) <- hashDecls']
      dataDecls' = Map.difference allDecls effectDecls
      effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
      -- ctor and effect terms
      ctors = foldMap (DD.Names.dataDeclToNames' Name.unsafeParseVar) (Map.toList dataDecls')
      effects = foldMap (DD.Names.effectDeclToNames' Name.unsafeParseVar) (Map.toList effectDecls')
      names' = ctors <> effects
      overlaps =
        let w v dd (toDataDecl -> ed) = DupDataAndAbility v (DD.annotation dd) (DD.annotation ed)
         in Map.elems $ Map.intersectionWithKey w dataDecls effectDecls
        where

      okVars = Map.keysSet allDecls0
      unknownTypeRefs =
        Map.elems allDecls0 >>= \dd ->
          let cts = DD.constructorTypes dd
           in cts >>= \ct ->
                [ UnknownType v a | (v, a) <- ABT.freeVarOccurrences mempty ct, not (Set.member v okVars)
                ]
  pure $
    if null overlaps && null unknownTypeRefs
      then pure $ Env dataDecls' effectDecls' names'
      else Left (unknownTypeRefs ++ overlaps)
