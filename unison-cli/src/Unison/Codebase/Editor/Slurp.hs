module Unison.Codebase.Editor.Slurp where

import Control.Lens
import Data.Bifunctor (second)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Semigroup.Foldable as Semigroup
import qualified Data.Set as Set
import Debug.Pretty.Simple (pTraceShowId)
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import qualified Unison.Codebase.Editor.SlurpResult as OldSlurp
import Unison.Codebase.Editor.TermsAndTypes (TermedOrTyped (Termed, Typed), TermsAndTypes (TermsAndTypes))
import qualified Unison.Codebase.Editor.TermsAndTypes as TT
import qualified Unison.DataDeclaration as DD
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Reference as Ref
import qualified Unison.Referent' as Referent
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Relation as Rel
import qualified Unison.Util.Set as Set
import Unison.Var (Var)
import Unison.WatchKind (pattern TestWatch)

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

data SlurpOp = AddOp | UpdateOp
  deriving (Eq, Show)

data LabeledVar v = LabeledVar v LD.LabeledDependency
  deriving (Eq, Ord)

isTypeVar :: LabeledVar v -> Bool
isTypeVar (LabeledVar _ LD.TypeReference {}) = True
isTypeVar _ = False

isTermVar :: LabeledVar v -> Bool
isTermVar = not . isTypeVar

data SlurpStatus
  = New
  | Updated
  | Duplicate
  deriving (Eq, Ord, Show)

data BlockStatus v
  = Add
  | Duplicated
  | NeedsUpdate (TermedOrTyped v)
  | Update
  | ErrFrom (TermedOrTyped v) SlurpErr
  | SelfErr SlurpErr
  deriving (Eq, Ord, Show)

instance Semigroup (BlockStatus v) where
  SelfErr err <> _ = SelfErr err
  _ <> SelfErr err = SelfErr err
  ErrFrom v err <> _ = ErrFrom v err
  _ <> ErrFrom v err = ErrFrom v err
  Update <> _ = Update
  _ <> Update = Update
  NeedsUpdate v <> _ = NeedsUpdate v
  _ <> NeedsUpdate v = NeedsUpdate v
  Add <> _ = Add
  _ <> Add = Add
  Duplicated <> _ = Duplicated

data SlurpPrintout v = SlurpPrintout
  { notOk :: Map v SlurpErr,
    ok :: Map v SlurpStatus
  }
  deriving (Eq, Ord, Show)

data SlurpErr
  = TermCtorCollision
  | CtorTermCollision
  deriving (Eq, Ord, Show)

data DefinitionNotes
  = DefOk SlurpStatus
  | DefErr SlurpErr
  deriving (Show)

type SlurpResult v = Map (TermedOrTyped v) (DefinitionNotes, Map (TermedOrTyped v) DefinitionNotes)

type Result v = Map (BlockStatus v) (Set (TermedOrTyped v))

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
results :: forall v. (Ord v, Show v) => SlurpResult v -> Result v
results sr =
  pTraceShowId $ analyzed
  where
    analyzed :: Map (BlockStatus v) (Set (TermedOrTyped v))
    analyzed =
      sr
        & Map.toList
        & fmap
          ( \(tv, (defNotes, deps)) ->
              ( Semigroup.fold1 (getBlockStatus False defNotes tv NEList.:| (Map.toList deps <&> \(depTV, depDefNotes) -> getBlockStatus True depDefNotes depTV)),
                Set.singleton tv
              )
          )
        & Map.fromListWith (<>)

getBlockStatus :: (Ord v, Show v) => Bool -> DefinitionNotes -> TermedOrTyped v -> BlockStatus v
getBlockStatus isDep defNotes tv =
  case defNotes of
    DefOk Updated -> if isDep then NeedsUpdate tv else Update
    DefErr err -> ErrFrom tv err
    DefOk New -> Add
    DefOk Duplicate -> Duplicated

-- Need to know:
-- What can be added without errors?
-- What can be updated without errors?
-- What has errors?
-- What is blocked?

analyzeTypecheckedUnisonFile ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Maybe (Set v) ->
  Names ->
  (Map (TermedOrTyped v) (DefinitionNotes, Map (TermedOrTyped v) (DefinitionNotes)))
analyzeTypecheckedUnisonFile uf maybeDefsToConsider unalteredCodebaseNames =
  let codebaseNames :: Names
      codebaseNames = computeNamesWithDeprecations uf unalteredCodebaseNames
      varDeps :: Map (TermedOrTyped v) (Set (TermedOrTyped v))
      varDeps = computeVarDeps uf maybeDefsToConsider varRelation
      statusMap :: Map (TermedOrTyped v) (DefinitionNotes, Map (TermedOrTyped v) DefinitionNotes)
      statusMap = computeVarStatuses varDeps varRelation codebaseNames
   in pTraceShowId statusMap
  where
    varRelation :: Rel.Relation (TermedOrTyped v) LD.LabeledDependency
    varRelation = undefined $ labelling uf

computeNamesWithDeprecations ::
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Names ->
  Names
computeNamesWithDeprecations uf unalteredCodebaseNames = codebaseNames
  where
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

computeVarStatuses ::
  forall v.
  (Ord v, Var v) =>
  Map (TermedOrTyped v) (Set (TermedOrTyped v)) ->
  Rel.Relation (TermedOrTyped v) LD.LabeledDependency ->
  Names ->
  ( Map
      (TermedOrTyped v)
      (DefinitionNotes, Map (TermedOrTyped v) DefinitionNotes)
  )
computeVarStatuses depMap varRelation codebaseNames = statuses
  where
    statuses :: Map (TermedOrTyped v) (DefinitionNotes, Map (TermedOrTyped v) (DefinitionNotes))
    statuses =
      let withNotes =
            depMap
              & Map.mapWithKey
                ( \tv deps ->
                    (definitionStatus tv, deps)
                )
          withTransitiveNotes ::
            ( Map
                (TermedOrTyped v)
                ( DefinitionNotes,
                  (Map (TermedOrTyped v) DefinitionNotes)
                )
            )
          withTransitiveNotes =
            withNotes
              & (fmap . fmap)
                ( \deps -> Map.fromList $ do
                    tv <- Set.toList deps
                    (notes, _) <- maybeToList (Map.lookup tv withNotes)
                    pure (tv, notes)
                )
       in withTransitiveNotes
    definitionStatus :: TermedOrTyped v -> DefinitionNotes
    definitionStatus tv =
      let ld = case Set.toList (Rel.lookupDom tv varRelation) of
            [r] -> r
            _ -> error $ "Expected exactly one LabeledDependency in relation for var: " <> show tv
          v = TT.unTermedOrTyped tv
          existingTypesAtName = Names.typesNamed codebaseNames (Name.unsafeFromVar v)
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

computeVarDeps ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Maybe (Set v) ->
  Rel.Relation (TermedOrTyped v) LD.LabeledDependency ->
  Map (TermedOrTyped v) (Set (TermedOrTyped v))
computeVarDeps uf maybeDefsToConsider varRelation =
  pTraceShowId $
    let allFileVars :: Set (TermedOrTyped v)
        allFileVars = Rel.dom varRelation

        allInvolvedVars :: Set (TermedOrTyped v)
        allInvolvedVars =
          case maybeDefsToConsider of
            Nothing -> allFileVars
            Just vs ->
              let existingVars :: Set (TermedOrTyped v) = Set.fromList $ do
                    v <- Set.toList vs
                    -- We don't know whether each var is a type or term, so we try both.
                    tv <- [Typed v, Termed v]
                    guard (Rel.memberDom tv varRelation)
                    pure tv
               in varClosure existingVars

        depMap :: (Map (TermedOrTyped v) (Set (TermedOrTyped v)))
        depMap =
          allInvolvedVars
            & Set.toList
            & fmap
              ( \tv -> (tv, varClosure (Set.singleton tv))
              )
            & Map.fromListWith (<>)
     in depMap
  where
    -- Compute the closure of all vars which the provided vars depend on.
    varClosure :: Set (TermedOrTyped v) -> Set (TermedOrTyped v)
    varClosure (sortVars -> toSlurpComponent -> sc) =
      mingleVars . fromSlurpComponent $ SC.closeWithDependencies uf sc

-- TODO: Does this need to contain constructors? Probably.
-- Does not include constructors
labelling :: forall v a. Ord v => UF.TypecheckedUnisonFile v a -> Rel.Relation (TermedOrTyped v) LD.LabeledDependency
labelling uf = decls <> effects <> terms
  where
    terms :: Rel.Relation (TermedOrTyped v) LD.LabeledDependency
    terms =
      UF.hashTermsId uf
        & Map.toList
        -- TODO: ensure we handle watches with assignments correctly.
        -- Filter out watches
        & mapMaybe
          ( \case
              (v, (refId, w, _, _))
                | w == Just TestWatch || w == Nothing ->
                  Just (Termed v, LD.derivedTerm refId)
              _ -> Nothing
          )
        & Rel.fromList
    decls :: Rel.Relation (TermedOrTyped v) LD.LabeledDependency
    decls =
      UF.dataDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (Typed v, LD.derivedType refId))
        & Rel.fromList

    effects :: Rel.Relation (TermedOrTyped v) LD.LabeledDependency
    effects =
      UF.effectDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (Typed v, (LD.derivedType refId)))
        & Rel.fromList

selectDefinitions :: Ord v => SlurpComponent v -> UF.TypecheckedUnisonFile v a -> UF.TypecheckedUnisonFile v a
selectDefinitions
  (SlurpComponent {terms, types})
  ( UF.TypecheckedUnisonFileId
      dataDeclarations'
      effectDeclarations'
      topLevelComponents'
      watchComponents
      hashTerms
    ) =
    UF.TypecheckedUnisonFileId datas effects tlcs watches hashTerms'
    where
      hashTerms' = Map.restrictKeys hashTerms terms
      datas = Map.restrictKeys dataDeclarations' types
      effects = Map.restrictKeys effectDeclarations' types
      tlcs = filter (not . null) $ fmap (List.filter filterTLC) topLevelComponents'
      watches = filter (not . null . snd) $ fmap (second (List.filter filterTLC)) watchComponents
      filterTLC (v, _, _) = Set.member v terms

toSlurpResult ::
  forall v.
  (Ord v, Show v) =>
  UF.TypecheckedUnisonFile v Ann ->
  SlurpOp ->
  Maybe (Set v) ->
  Result v ->
  OldSlurp.SlurpResult v
toSlurpResult uf op mvs r =
  pTraceShowId $
    -- TODO: Do a proper partition to speed this up.
    OldSlurp.SlurpResult
      { OldSlurp.originalFile = uf,
        OldSlurp.extraDefinitions =
          case mvs of
            Nothing -> mempty
            Just vs ->
              let allVars = fold r
                  desired =
                    vs
                      & Set.flatMap (\v -> Set.fromList [Typed v, Termed v])
               in toSlurpComponent . sortVars $ Set.difference allVars desired,
        OldSlurp.adds = adds,
        OldSlurp.duplicates = duplicates,
        OldSlurp.collisions = if op == AddOp then updates else mempty,
        OldSlurp.conflicts = mempty,
        OldSlurp.updates = if op == UpdateOp then updates else mempty,
        OldSlurp.termExistingConstructorCollisions =
          let SlurpComponent types terms = termCtorColl
           in types <> terms,
        OldSlurp.constructorExistingTermCollisions =
          let SlurpComponent types terms = ctorTermColl
           in types <> terms,
        OldSlurp.termAlias = mempty,
        OldSlurp.typeAlias = mempty,
        OldSlurp.defsWithBlockedDependencies = blocked
      }
  where
    adds, duplicates, updates, termCtorColl, ctorTermColl, blocked :: SlurpComponent v
    (adds, duplicates, updates, termCtorColl, (ctorTermColl, blocked)) =
      r
        & ifoldMap
          ( \k tvs ->
              let sc = toSlurpComponent . sortVars $ tvs
               in case k of
                    Add -> (sc, mempty, mempty, mempty, (mempty, mempty))
                    Duplicated -> (mempty, sc, mempty, mempty, (mempty, mempty))
                    Update -> (mempty, mempty, sc, mempty, (mempty, mempty))
                    NeedsUpdate v ->
                      case op of
                        AddOp ->
                          (mempty, mempty, singletonSC v, mempty, (mempty, sc `SC.difference` singletonSC v))
                        UpdateOp ->
                          (sc, mempty, mempty, mempty, (mempty, mempty))
                    ErrFrom v TermCtorCollision -> (mempty, mempty, mempty, singletonSC v, (mempty, sc `SC.difference` singletonSC v))
                    ErrFrom v CtorTermCollision -> (mempty, mempty, mempty, mempty, (singletonSC v, sc `SC.difference` singletonSC v))
                    SelfErr TermCtorCollision -> (mempty, mempty, mempty, sc, (mempty, mempty))
                    SelfErr CtorTermCollision -> (mempty, mempty, mempty, mempty, (sc, mempty))
          )
    singletonSC = \case
      Typed v -> SlurpComponent {terms = mempty, types = Set.singleton v}
      Termed v -> SlurpComponent {terms = Set.singleton v, types = mempty}

anyErrors :: Result v -> Bool
anyErrors r =
  any isError . Map.keys $ Map.filter (not . null) r
  where
    isError :: BlockStatus v -> Bool
    isError = \case
      Add -> False
      Duplicated -> False
      Update {} -> False
      -- NeedsUpdate is an error only if we're trying to Add
      NeedsUpdate {} -> True
      ErrFrom {} -> True
      SelfErr {} -> True

toSlurpComponent :: TermsAndTypes (Set v) -> SlurpComponent v
toSlurpComponent TermsAndTypes {TT.terms = terms, TT.types = types} =
  SlurpComponent {terms = terms, types = types}

fromSlurpComponent :: SlurpComponent v -> TermsAndTypes (Set v)
fromSlurpComponent SlurpComponent {terms = terms, types = types} =
  TermsAndTypes {TT.terms = terms, TT.types = types}

sortVars :: (Foldable f, Ord v) => f (TermedOrTyped v) -> TermsAndTypes (Set v)
sortVars =
  foldMap
    ( \case
        Typed v -> TT.fromTypes (Set.singleton v)
        Termed v -> TT.fromTerms (Set.singleton v)
    )

mingleVars :: Ord v => TermsAndTypes (Set v) -> Set (TermedOrTyped v)
mingleVars = Set.fromList . fold . TT.labeledF . fmap Set.toList
