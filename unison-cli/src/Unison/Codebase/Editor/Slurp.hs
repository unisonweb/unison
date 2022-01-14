module Unison.Codebase.Editor.Slurp where

import Control.Lens
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Semigroup.Foldable as Semigroup
import qualified Data.Set as Set
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
import qualified Unison.Referent' as Referent
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
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
  deriving (Eq, Ord, Show)

data BlockStatus v
  = Add
  | Duplicated
  | NeedsUpdate v
  | ErrFrom v SlurpErr
  | SelfErr SlurpErr
  deriving (Eq, Ord)

instance Semigroup (BlockStatus v) where
  SelfErr err <> _ = SelfErr err
  _ <> SelfErr err = SelfErr err
  ErrFrom v err <> _ = ErrFrom v err
  _ <> ErrFrom v err = ErrFrom v err
  NeedsUpdate v <> _ = NeedsUpdate v
  _ <> NeedsUpdate v = NeedsUpdate v
  Add <> _ = Add
  _ <> Add = Add
  Duplicated <> Duplicated = Duplicated

data TypeOrTermVar v = TypeVar v | TermVar v
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

data SlurpErr
  = TermCtorCollision
  | CtorTermCollision
  deriving (Eq, Ord, Show)

data SlurpComponent v = SlurpComponent {scTypes :: Set v, scTerms :: Set v}
  deriving (Eq, Ord, Show)

instance Ord v => Semigroup (SlurpComponent v) where
  SlurpComponent typeL termL <> SlurpComponent typeR termR =
    SlurpComponent (typeL <> typeR) (termL <> termR)

instance Ord v => Monoid (SlurpComponent v) where
  mempty = SlurpComponent mempty mempty

data DefinitionNotes
  = DefOk SlurpStatus
  | DefErr SlurpErr

data SlurpResult v = SlurpResult
  { termNotes :: Map v (DefinitionNotes, Set (TypeOrTermVar v)),
    typeNotes :: Map v (DefinitionNotes, Set (TypeOrTermVar v))
  }

type Result v = Map (BlockStatus v) (SlurpComponent v)

-- data Result v = Result
--   { addable :: SlurpComponent v,
--     needUpdate :: SlurpComponent v,
--     duplicate :: SlurpComponent v,
--     blockedTerms :: Map (SlurpErr v) (Set v)
--   }

-- instance Semigroup (Result v) where
--   Result adds1 updates1 duplicates1 tcColl1 ctColl1 <> Result adds2 updates2 duplicates2 tcColl2 ctColl2 =
--     Result (adds1 <> adds2) (updates1 <> updates2) (duplicates1 <> duplicates2) (tcColl1 <> tcColl2) (ctColl1 <> ctColl2)

-- instance Monoid (Result v) where
--   mempty = Result mempty mempty mempty mempty mempty

-- Compute all definitions which can be added, or the reasons why a def can't be added.
results :: forall v. Ord v => SlurpResult v -> Result v
results sr@(SlurpResult terms types) =
  Map.unionWith (<>) analyzedTerms analyzedTypes
  where
    analyzedTerms :: Map (BlockStatus v) (SlurpComponent v)
    analyzedTerms =
      terms
        & Map.toList
        & fmap
          ( \(v, (_, deps)) ->
              ( Semigroup.foldMap1 (getBlockStatus sr) (TermVar v NEList.:| Set.toList deps),
                mempty {scTerms = Set.singleton v}
              )
          )
        & Map.fromListWith (<>)
    analyzedTypes :: Map (BlockStatus v) (SlurpComponent v)
    analyzedTypes =
      types
        & Map.toList
        & fmap
          ( \(v, (_, deps)) ->
              ( Semigroup.foldMap1 (getBlockStatus sr) (TypeVar v NEList.:| Set.toList deps),
                mempty {scTypes = Set.singleton v}
              )
          )
        & Map.fromListWith (<>)

getBlockStatus :: Ord v => SlurpResult v -> TypeOrTermVar v -> BlockStatus v
getBlockStatus (SlurpResult {termNotes, typeNotes}) tv =
  let v = unlabeled tv
      defNotes = case tv of
        TypeVar v -> typeNotes Map.! v
        TermVar v -> termNotes Map.! v
   in case fst defNotes of
        DefOk Updated -> NeedsUpdate v
        DefErr err -> ErrFrom v err
        DefOk New -> Add
        DefOk Duplicate -> Duplicated

-- Need to know:
-- What can be added without errors?
-- What can be updated without errors?
-- What has errors?
-- What is blocked?

type ComponentHash = Hash

data Components v = Components
  { termComponents :: Map Hash (Set v),
    typeComponents :: Map Hash (Set v)
  }

-- groupByOp :: SlurpResult v -> (SlurpComponent v, SlurpComponent v)
-- groupByOp (SlurpResult terms types) =
--   terms
--   & Map.mapEither (\(notes, deps) ->
--       any (== )
--     )

analyzeTypecheckedUnisonFile ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Maybe (Set v) ->
  Names ->
  SlurpResult v
analyzeTypecheckedUnisonFile uf maybeDefsToConsider unalteredCodebaseNames =
  let allInvolvedVars :: Set (LabeledVar v)
      allInvolvedVars = foldMap transitiveVarDeps defsToConsider

      termStatuses, typeStatuses :: Map v (DefinitionNotes, Set (TypeOrTermVar v))
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
                LabeledVar _ (LD.TypeReference {}) -> Left x
                LabeledVar _ (LD.TermReferent {}) -> Right x
            )
          & over both (Map.mapKeys (\(LabeledVar v _) -> v))
          & over
            both
            ( Map.map
                ( fmap
                    ( Set.map
                        ( \case
                            LabeledVar v (LD.TermReferent {}) -> TermVar v
                            LabeledVar v (LD.TypeReference {}) -> TypeVar v
                        )
                    )
                )
            )
   in -- & Map.mapEitherWithKey _
      SlurpResult termStatuses typeStatuses
  where
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

    definitionStatus :: LabeledVar v -> DefinitionNotes
    definitionStatus (LabeledVar v ld) =
      let existingTypesAtName = Names.typesNamed codebaseNames (Name.unsafeFromVar v)
          existingTermsAtName = Names.termsNamed codebaseNames (Name.unsafeFromVar v)
       in case ld of
            LD.TypeReference {} ->
              case Set.toList existingTypesAtName of
                [] -> DefOk New
                [r]
                  | LD.typeRef r == ld -> DefOk Duplicate
                  | otherwise -> DefOk Updated
                -- If there are many existing terms, they must be in conflict, we can update
                -- to resolve the conflict.
                _ -> DefOk Updated
            LD.TermReference {} ->
              case Set.toList existingTermsAtName of
                [] -> DefOk New
                rs | any Referent.isConstructor rs -> DefErr TermCtorCollision
                [r]
                  | LD.referent r == ld -> DefOk Duplicate
                  | otherwise -> DefOk Updated
                -- If there are many existing terms, they must be in conflict, we can update
                -- to resolve the conflict.
                _ -> DefOk Updated
            LD.ConReference {} ->
              case Set.toList existingTermsAtName of
                [] -> DefOk New
                rs | any (not . Referent.isConstructor) rs -> DefErr CtorTermCollision
                [r]
                  | LD.referent r == ld -> DefOk Duplicate
                  | otherwise -> DefOk Updated
                -- If there are many existing terms, they must be in conflict, we can update
                -- to resolve the conflict.
                _ -> DefOk Updated

    varRelation :: Rel3.Relation3 v (LabeledVar v) ComponentHash
    varRelation = labelling uf

    -- Get the set of all DIRECT definitions in the file which a definition depends on.
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

-- slurpOp ::
--   forall v.
--   Ord v =>
--   SlurpResult v ->
--   (SlurpComponent v, SlurpComponent v)
-- slurpOp (SlurpResult terms types) =
--   let (termAdds, termUpdates, termErrs) = partition terms
--       (typeAdds, typeUpdates, typeErrs) = partition types
--    in (SlurpComponent termAdds termUpdates termErrs, SlurpComponent typeAdds typeUpdates typeErrs)
--   where
--     partition :: (Map v (DefinitionNotes, Set (LabeledVar v))) -> (Set v, Set v, Map v SlurpErr)
--     partition sr =
--       let (adds, updates, errs) =
--             flip execState mempty $
--               for (Map.toList sr) $ \(v, (dn, _)) -> do
--                 case dn of
--                   DefOk New -> _1 %= Set.insert v
--                   DefOk Updated -> _2 %= Set.insert v
--                   DefOk Duplicate -> pure ()
--                   DefErr err -> _3 . at v ?= err
--        in (adds, updates, errs)

componentTransitiveDeps :: Ord v => UF.TypecheckedUnisonFile v a -> Map ComponentHash (Set ComponentHash)
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
labelling :: forall v a. Ord v => UF.TypecheckedUnisonFile v a -> Rel3.Relation3 v (LabeledVar v) ComponentHash
labelling uf = decls <> effects <> terms
  where
    terms :: Rel3.Relation3 v (LabeledVar v) ComponentHash
    terms =
      UF.hashTermsId uf
        & Map.toList
        & fmap (\(v, (refId, _, _, _)) -> (v, LabeledVar v (LD.derivedTerm refId), idToComponentHash refId))
        & Rel3.fromList
    decls :: Rel3.Relation3 v (LabeledVar v) ComponentHash
    decls =
      UF.dataDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (v, LabeledVar v (LD.derivedType refId), idToComponentHash refId))
        & Rel3.fromList

    effects :: Rel3.Relation3 v (LabeledVar v) ComponentHash
    effects =
      UF.effectDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (v, LabeledVar v (LD.derivedType refId), idToComponentHash refId))
        & Rel3.fromList

idToComponentHash :: Ref.Id -> ComponentHash
idToComponentHash (Ref.Id componentHash _ _) = componentHash

-- selectVars :: SlurpComponent (LabeledVar v) -> UF.TypecheckedUnisonFile v a -> UF.TypecheckedUnisonFile v a
-- selectVars
--   vs
--   ( UF.TypecheckedUnisonFileId
--       dataDeclarations'
--       effectDeclarations'
--       topLevelComponents'
--       watchComponents
--       hashTerms
--     ) =
--     UF.TypecheckedUnisonFileId datas effects tlcs watches hashTerms'
--     where
--       keepTypes = SC.types keep
--       hashTerms' = Map.restrictKeys hashTerms keepTerms
--       datas = Map.restrictKeys dataDeclarations' keepTypes
--       effects = Map.restrictKeys effectDeclarations' keepTypes
--       tlcs = filter (not . null) $ fmap (List.filter filterTLC) topLevelComponents'
--       watches = filter (not . null . snd) $ fmap (second (List.filter filterTLC)) watchComponents
--       filterTLC (v, _, _) = Set.member v keepTerms

-- dependencyMap :: UF.TypecheckedUnisonFile -> Map
