{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Unison.Codebase.Editor.Slurp where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.DataDeclaration as DD
import Unison.Hash (Hash)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Reference as Ref
import qualified Unison.Referent as Referent
import qualified Unison.Referent' as Referent
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.UnisonFile.Type as UF
import qualified Unison.Util.Relation as Rel
import qualified Unison.Util.Relation3 as Rel3
import qualified Unison.Util.Set as Set
import Unison.Var (Var)

-- Determine which components we're considering, i.e. find the components of all provided
-- vars, then include any components they depend on.
--
-- Then, compute any deprecations and build the env
-- Then, consider all vars in each component and get status (collision, add, or update)
-- Collect and collapse the statuses of each component.
--   I.e., if any definition has an error, the whole component is an error
--         if any piece needs an update
--
--
--  Does depending on a type also mean depending on all its constructors

data LabeledVar v = LabeledVar v LD.LabeledDependency
  deriving (Eq, Ord)

data SlurpStatus = New | Updated | Duplicate

data SlurpOp = Add | Update

data TypeOrTermVar v = TypeVar v | TermVar v

-- componentHashForLabeledVar :: LabeledVar v -> ComponentHash
-- componentHashForLabeledVar (LabeledVar _ ld) =
--   labeledDepToComponentHash ld

labeledDepToComponentHash :: LD.LabeledDependency -> ComponentHash
labeledDepToComponentHash ld =
  LD.fold unsafeComponentHashForReference (unsafeComponentHashForReference . Referent.toReference') ld
  where
    unsafeComponentHashForReference =
      fromMaybe (error "Builtin encountered when var was expected")
        . componentHashForReference

componentHashForReference :: Ref.Reference -> Maybe Hash
componentHashForReference =
  \case
    Ref.Builtin {} -> Nothing
    Ref.DerivedId (Ref.Id componentHash _ _) -> Just componentHash

unlabeled :: TypeOrTermVar v -> v
unlabeled = \case
  TypeVar v -> v
  TermVar v -> v

data SlurpPrintout v = SlurpPrintout
  { notOk :: Map v SlurpErr,
    ok :: Map v SlurpStatus
  }

data SlurpErr
  = TermCtorCollision
  | CtorTermCollision

data SlurpComponent v = SlurpComponent {types :: Set v, terms :: Set v}
  deriving (Eq, Ord, Show)

data DefinitionNotes
  = DefStatus SlurpStatus
  | DefErr SlurpErr

data SlurpResult v = SlurpResult
  { termNotes :: Map v (DefinitionNotes, Set (LabeledVar v)),
    typeNotes :: Map v (DefinitionNotes, Set (LabeledVar v))
  }

type ComponentHash = Hash

data Components v = Components
  { termComponents :: Map Hash (Set v),
    typeComponents :: Map Hash (Set v)
  }

toSlurpPrintout :: SlurpResult v -> SlurpPrintout v
toSlurpPrintout = undefined

collectComponents :: UF.TypecheckedUnisonFile v Ann -> Components v
collectComponents _uf = Components {termComponents, typeComponents}
  where
    termComponents = undefined
    typeComponents = undefined

computeComponentDependencies :: Components v -> Map Hash (Set Hash)
computeComponentDependencies = undefined

analyzeTypecheckedUnisonFile ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Names ->
  Maybe (Set v) ->
  SlurpResult v
analyzeTypecheckedUnisonFile uf unalteredCodebaseNames maybeDefsToConsider =
  let allInvolvedVars :: Set (LabeledVar v)
      allInvolvedVars = foldMap transitiveVarDeps defsToConsider

      termStatuses, typeStatuses :: Map v (DefinitionNotes, Set (LabeledVar v))
      (termStatuses, typeStatuses) =
        allInvolvedVars
          & Set.toList
          & fmap
            ( \lv ->
                (lv, (definitionStatus lv, transitiveLabeledVarDeps lv))
            )
          & Map.fromList
          & Map.mapEitherWithKey
            ( \lv x -> case lv of
                LabeledVar v ld -> _
            )
          & over both (Map.mapKeys (\(LabeledVar v _) -> v))
   in -- & Map.mapEitherWithKey _
      SlurpResult termStatuses typeStatuses
  where
    fileNames :: Names
    fileNames = UF.typecheckedToNames uf

    transitiveCHDeps :: Map ComponentHash (Set ComponentHash)
    transitiveCHDeps =
      componentTransitiveDeps uf

    -- Find all other file-local vars that a var depends on.
    -- This version is for when you don't know whether a var is a type or term
    -- E.g., if the user types 'add x', we don't know whether x is a term, type, or
    -- constructor, so we add all of them.
    transitiveVarDeps :: v -> Set (LabeledVar v)
    transitiveVarDeps v =
      Rel3.lookupD1 v varRelation
        & Rel.ran
        -- Find all transitive components we rely on
        & ( \chs ->
              chs <> foldMap (\ch -> fold $ Map.lookup ch transitiveCHDeps) chs
          )
        -- Find all variables within all considered components
        & foldMap (\ch -> Rel.ran $ Rel3.lookupD3 ch varRelation)

    transitiveLabeledVarDeps :: LabeledVar v -> Set (LabeledVar v)
    transitiveLabeledVarDeps lv =
      Rel3.lookupD2 lv varRelation
        & Rel.ran
        -- Find all transitive components we rely on
        & ( \chs ->
              chs <> foldMap (\ch -> fold $ Map.lookup ch transitiveCHDeps) chs
          )
        -- Find all variables within all considered components
        & foldMap (\ch -> Rel.ran $ Rel3.lookupD3 ch varRelation)

    defsToConsider :: Set v
    defsToConsider = case maybeDefsToConsider of
      Nothing ->
        varRelation
          & Rel3.d1s
      Just vs -> vs

    -- labeledVars :: Map v (LabeledVar v)
    -- labeledVars = undefined

    definitionStatus :: LabeledVar v -> DefinitionNotes
    definitionStatus (undefined -> tv) =
      let v = unlabeled tv
          existingTypesAtName = Names.typesNamed codebaseNames (Name.unsafeFromVar v)
          existingTermsAtName = Names.termsNamed codebaseNames (Name.unsafeFromVar v)
          existingTermsMatchingReference = Names.termsNamed codebaseNames (Name.unsafeFromVar v)
          varRef = varReferences Map.! v
       in case tv of
            TermVar {} ->
              case Set.toList existingTermsAtName of
                [] -> DefStatus New
                [r] | LD.referent r == varRef -> DefStatus Duplicate
                [Referent.Con {}] | LD.ConReference {} <- varRef -> DefErr TermCtorCollision
                [Referent.Ref {}] | LD.ConReference {} <- varRef -> DefErr CtorTermCollision
                -- This allows us to resolve conflicts with an update.
                _ -> DefStatus Updated
            -- [r] -> DefStatus Updated
            -- _ -> DefStatus Conflicted
            TypeVar {} -> undefined
    varReferences :: Map v LD.LabeledDependency
    varReferences = UF.referencesMap uf

    varRelation :: Rel3.Relation3 v (LabeledVar v) ComponentHash
    varRelation = labelling uf

    -- Get the set of all DIRECT definitions in the file which a definition depends on.
    -- directDeps lv = do
    -- let vComponentHash = componentHashForLabeledVar lv
    -- componentPeers = Rel.ran $ Rel3.lookupD3 vComponentHash varRelation
    -- undefined
    -- let varComponentHash = varToComponentHash Map.! v
    --     componentPeers = componentMapping Map.! varComponentHash
    --     directDeps = case UF.hashTermsId uf Map.!? v of
    --       Nothing -> mempty
    --       Just (_, _, term, _) -> ABT.freeVars term
    --  in Set.delete v (componentPeers <> directDeps)

    -- transitiveVarDeps :: Set v -> v -> Set v
    -- transitiveVarDeps resolved v =
    --   let directDeps = varDeps v
    --    in Foldable.foldl' transitiveVarDeps (Set.insert v resolved) directDeps
    --   where
    --     go resolved nextV =
    --       if Set.member nextV resolved
    --         then resolved
    --         else resolved <> transitiveVarDeps resolved nextV

    -- transitiveVarDeps :: Set (LabeledVar v) -> LabeledVar v -> Set (LabeledVar v)
    -- transitiveVarDeps resolved v =
    --   let directDeps = varDeps v
    --    in Foldable.foldl' transitiveVarDeps (Set.insert v resolved) directDeps
    --   where
    --     go resolved nextV =
    --       if Set.member nextV resolved
    --         then resolved
    --         else resolved <> transitiveVarDeps resolved nextV

    -- varToComponentHash :: Map v ComponentHash
    -- varToComponentHash = Map.fromList $ do
    --   -- List monad
    --   (hash, vars) <- Map.toList componentMapping
    --   v <- Set.toList vars
    --   pure (v, hash)

    codebaseNames :: Names
    codebaseNames =
      -- TODO: make faster
      -- TODO: how does defsToConsider affect deprecations?
      Names.filter (`Set.notMember` deprecatedConstructors) unalteredCodebaseNames
    constructorNamesInFile :: Set Name
    constructorNamesInFile =
      Map.elems (UF.dataDeclarationsId' uf)
        <> (fmap . fmap) DD.toDataDecl (Map.elems (UF.effectDeclarationsId' uf))
          & fmap snd
          & concatMap
            ( \decl ->
                DD.constructors' decl <&> \(_ann, v, _typ) ->
                  Name.unsafeFromVar v
            )
          & Set.fromList

    deprecatedConstructors :: Set Name
    deprecatedConstructors =
      let allRefIds =
            fmap fst (Map.elems (UF.dataDeclarationsId' uf))
              <> fmap fst (Map.elems (UF.effectDeclarationsId' uf))
          existingConstructorsFromEditedTypes = Set.fromList $ do
            -- List Monad
            refId <- allRefIds
            (name, _ref) <- Names.constructorsForType (Ref.DerivedId refId) unalteredCodebaseNames
            pure name
       in -- Compute any constructors which were deleted
          existingConstructorsFromEditedTypes `Set.difference` constructorNamesInFile

slurpErrs :: SlurpResult v -> Map v SlurpErr
slurpErrs (SlurpResult defs _) =
  defs
    & Map.mapMaybe
      ( \case
          (DefErr err, _) -> Just err
          _ -> Nothing
      )

slurpOp ::
  forall v.
  SlurpResult v ->
  (SlurpComponent v, SlurpComponent v, Map v SlurpErr)
slurpOp (SlurpResult sr _) = do
  let (adds, updates, errs) =
        flip execState mempty $
          for (Map.toList sr) $ \(v, (dn, _)) -> do
            case dn of
              DefStatus New -> _1 %= Set.insert v
              DefStatus Updated -> _2 %= Set.insert v
              DefStatus Duplicate -> pure ()
              DefErr err -> _3 . at v ?= err
      adds' = partitionTypesAndTerms adds
      updates' = partitionTypesAndTerms updates
   in (adds', updates', errs)

partitionTypesAndTerms :: Set v -> SlurpComponent v
partitionTypesAndTerms = undefined

componentTransitiveDeps :: UF.TypecheckedUnisonFile v a -> Map ComponentHash (Set ComponentHash)
componentTransitiveDeps uf =
  let deps = Map.unionsWith (<>) [termDeps, dataDeps, effectDeps]
      filteredDeps :: Map ComponentHash (Set ComponentHash)
      filteredDeps =
        deps
          & Map.mapWithKey
            ( \k d ->
                d
                  -- Don't track the component as one of its own deps
                  & Set.delete k
                  -- Filter out any references to components which aren't defined in this file.
                  & Set.filter (\ch -> Map.member ch deps)
            )
      -- Find the fixed point of our dependencies, which will always terminate because
      -- component dependencies are acyclic.
      transitiveDeps =
        filteredDeps
          <&> ( \directDeps ->
                  directDeps
                    <> foldMap (\ch -> fold $ Map.lookup ch transitiveDeps) directDeps
              )
   in transitiveDeps
  where
    termDeps :: Map ComponentHash (Set ComponentHash)
    termDeps =
      UF.hashTermsId uf
        & Map.elems
        & fmap (\(refId, _watchKind, trm, _typ) -> (idToComponentHash refId, termComponentRefs trm))
        & Map.fromListWith (<>)
    dataDeps :: Map ComponentHash (Set ComponentHash)
    dataDeps =
      UF.dataDeclarationsId' uf
        & Map.elems
        & fmap (\(refId, decl) -> (idToComponentHash refId, dataDeclRefs decl))
        & Map.fromListWith (<>)
    effectDeps :: Map ComponentHash (Set ComponentHash)
    effectDeps =
      UF.effectDeclarationsId' uf
        & Map.elems
        & fmap (\(refId, effect) -> (idToComponentHash refId, dataDeclRefs (DD.toDataDecl effect)))
        & Map.fromListWith (<>)

termComponentRefs :: Ord v => Term v a -> Set ComponentHash
termComponentRefs trm =
  Term.dependencies trm
    -- Ignore builtins
    & Set.mapMaybe componentHashForReference

dataDeclRefs :: Ord v => DD.DataDeclaration v a -> Set ComponentHash
dataDeclRefs decl =
  DD.dependencies decl
    -- Ignore builtins
    & Set.mapMaybe componentHashForReference

-- Does not include constructors
labelling :: forall v a. UF.TypecheckedUnisonFile v a -> Rel3.Relation3 v (LabeledVar v) ComponentHash
labelling uf = _ $ decls <> effects <> terms
  where
    terms :: Rel3.Relation3 v LD.LabeledDependency ComponentHash
    terms =
      UF.hashTermsId uf
        & Map.toList
        & fmap (\(v, (refId, _, _, _)) -> (v, LD.derivedTerm refId, idToComponentHash refId))
        & Rel3.fromList
    decls :: Rel3.Relation3 v LD.LabeledDependency ComponentHash
    decls =
      UF.dataDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (v, LD.derivedType refId, idToComponentHash refId))
        & Rel3.fromList

    effects :: Rel3.Relation3 v LD.LabeledDependency ComponentHash
    effects =
      UF.effectDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (v, LD.derivedType refId, idToComponentHash refId))
        & Rel3.fromList

idToComponentHash :: Ref.Id -> ComponentHash
idToComponentHash (Ref.Id componentHash _ _) = componentHash

-- dependencyMap :: UF.TypecheckedUnisonFile -> Map
