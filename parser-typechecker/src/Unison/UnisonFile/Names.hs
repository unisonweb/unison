{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.UnisonFile.Names where

import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import qualified Unison.DataDeclaration as DD
import qualified Unison.DataDeclaration.Names as DD.Names
import qualified Unison.Hashing.V2.Convert as Hashing
import qualified Unison.Name as Name
import qualified Unison.Names.ResolutionResult as Names
import Unison.Names (Names (Names))
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
import Unison.UnisonFile.Env (Env (..))
import Unison.UnisonFile.Error (Error (DupDataAndAbility, UnknownType))
import Unison.UnisonFile.Type (TypecheckedUnisonFile (TypecheckedUnisonFileId), UnisonFile (UnisonFileId))
import qualified Unison.Util.Relation as Relation
import Unison.Var (Var)
import qualified Unison.WatchKind as WK

toNames :: Var v => UnisonFile v a -> Names
toNames uf = datas <> effects
  where
    datas = foldMap DD.Names.dataDeclToNames' (Map.toList (UF.dataDeclarationsId uf))
    effects = foldMap DD.Names.effectDeclToNames' (Map.toList (UF.effectDeclarationsId uf))

typecheckedToNames :: Var v => TypecheckedUnisonFile v a -> Names
typecheckedToNames uf = Names (terms <> ctors) types where
  terms = Relation.fromList
    [ (Name.unsafeFromVar v, Referent.Ref r)
    | (v, (r, wk, _, _)) <- Map.toList $ UF.hashTerms uf, wk == Nothing || wk == Just WK.TestWatch ]
  types = Relation.fromList
    [ (Name.unsafeFromVar v, r)
    | (v, r) <- Map.toList $ fmap fst (UF.dataDeclarations' uf)
                          <> fmap fst (UF.effectDeclarations' uf) ]
  ctors = Relation.fromMap
        . Map.mapKeys Name.unsafeFromVar
        . fmap (fmap Reference.DerivedId)
        . UF.hashConstructors
        $ uf

typecheckedUnisonFile0 :: Ord v => TypecheckedUnisonFile v a
typecheckedUnisonFile0 = TypecheckedUnisonFileId Map.empty Map.empty mempty mempty mempty


-- Substitutes free type and term variables occurring in the terms of this
-- `UnisonFile` using `externalNames`.
--
-- Hash-qualified names are substituted during parsing, but non-HQ names are
-- substituted at the end of parsing, since they can be locally bound. Example, in
-- `x -> x + math.sqrt 2`, we don't know if `math.sqrt` is locally bound until
-- we are done parsing, whereas `math.sqrt#abc` can be resolved immediately
-- as it can't refer to a local definition.
bindNames :: Var v
          => Names
          -> UnisonFile v a
          -> Names.ResolutionResult v a (UnisonFile v a)
bindNames names (UnisonFileId d e ts ws) = do
  -- todo: consider having some kind of binding structure for terms & watches
  --    so that you don't weirdly have free vars to tiptoe around.
  --    The free vars should just be the things that need to be bound externally.
  let termVars = (fst <$> ts) ++ (Map.elems ws >>= map fst)
      termVarsSet = Set.fromList termVars
  -- todo: can we clean up this lambda using something like `second`
  ts' <- traverse (\(v,t) -> (v,) <$> Term.bindNames termVarsSet names t) ts
  ws' <- traverse (traverse (\(v,t) -> (v,) <$> Term.bindNames termVarsSet names t)) ws
  pure $ UnisonFileId d e ts' ws'

-- This function computes hashes for data and effect declarations, and
-- also returns a function for resolving strings to (Reference, ConstructorId)
-- for parsing of pattern matching
--
-- If there are duplicate declarations, the duplicated names are returned on the
-- left.
environmentFor
  :: forall v a . Var v
  => Names
  -> Map v (DataDeclaration v a)
  -> Map v (EffectDeclaration v a)
  -> Names.ResolutionResult v a (Either [Error v a] (Env v a))
environmentFor names dataDecls0 effectDecls0 = do
  let locallyBoundTypes = Map.keysSet dataDecls0 <> Map.keysSet effectDecls0
  -- data decls and hash decls may reference each other, and thus must be hashed together
  dataDecls :: Map v (DataDeclaration v a) <-
    traverse (DD.Names.bindNames locallyBoundTypes names) dataDecls0
  effectDecls :: Map v (EffectDeclaration v a) <-
    traverse (DD.withEffectDeclM (DD.Names.bindNames locallyBoundTypes names)) effectDecls0
  let allDecls0 :: Map v (DataDeclaration v a)
      allDecls0 = Map.union dataDecls (toDataDecl <$> effectDecls)
  hashDecls' :: [(v, Reference.Id, DataDeclaration v a)] <- Hashing.hashDataDecls allDecls0
    -- then we have to pick out the dataDecls from the effectDecls
  let
    allDecls   = Map.fromList [ (v, (r, de)) | (v, r, de) <- hashDecls' ]
    dataDecls' = Map.difference allDecls effectDecls
    effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
    -- ctor and effect terms
    ctors = foldMap DD.Names.dataDeclToNames' (Map.toList dataDecls')
    effects = foldMap DD.Names.effectDeclToNames' (Map.toList effectDecls')
    names' = ctors <> effects
    overlaps = let
      w v dd (toDataDecl -> ed) = DupDataAndAbility v (DD.annotation dd) (DD.annotation ed)
      in Map.elems $ Map.intersectionWithKey w dataDecls effectDecls where
    okVars = Map.keysSet allDecls0
    unknownTypeRefs = Map.elems allDecls0 >>= \dd ->
      let cts = DD.constructorTypes dd
      in cts >>= \ct -> [ UnknownType v a | (v,a) <- ABT.freeVarOccurrences mempty ct
                                          , not (Set.member v okVars) ]
  pure $
    if null overlaps && null unknownTypeRefs
    then pure $ Env dataDecls' effectDecls' names'
    else Left (unknownTypeRefs ++ overlaps)
