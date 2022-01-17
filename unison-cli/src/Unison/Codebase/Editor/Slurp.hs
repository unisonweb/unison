module Unison.Codebase.Editor.Slurp
  ( SlurpOp (..),
    VarsByStatus,
    BlockStatus (..),
    anyErrors,
    results,
    analyzeTypecheckedUnisonFile,
    selectDefinitions,
    toSlurpResult,
    sortVars,
  )
where

import Control.Lens
import Data.Bifunctor (second)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map as Map
import qualified Data.Semigroup.Foldable as Semigroup
import qualified Data.Set as Set
import Debug.Pretty.Simple (pTraceShow, pTraceShowId)
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import qualified Unison.Codebase.Editor.SlurpResult as OldSlurp
import qualified Unison.Codebase.Editor.SlurpResult as SR
import Unison.Codebase.Editor.TermsAndTypes (TermedOrTyped (Termed, Typed))
import qualified Unison.Codebase.Editor.TermsAndTypes as TT
import qualified Unison.Codebase.Path as Path
import qualified Unison.DataDeclaration as DD
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Referent' as Referent
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.Util.Relation as Rel
import qualified Unison.Util.Set as Set
import Unison.Var (Var)
import qualified Unison.Var as Var
import Unison.WatchKind (pattern TestWatch)

data SlurpOp = AddOp | UpdateOp
  deriving (Eq, Show)

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
  | Conflict
  deriving (Eq, Ord, Show)

data DefinitionNotes
  = DefOk SlurpStatus
  | DefErr SlurpErr
  deriving (Show)

type SlurpAnalysis v = Map (TermedOrTyped v) (DefinitionNotes, Map (TermedOrTyped v) DefinitionNotes)

type VarsByStatus v = Map (BlockStatus v) (Set (TermedOrTyped v))

-- Compute all definitions which can be added, or the reasons why a def can't be added.
results :: forall v. (Ord v, Show v) => SlurpAnalysis v -> VarsByStatus v
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
  Set v ->
  Maybe SlurpOp ->
  Names ->
  Path.Absolute ->
  SR.SlurpResult v
analyzeTypecheckedUnisonFile uf defsToConsider slurpOp unalteredCodebaseNames currentPath =
  let varRelation :: Rel.Relation (TermedOrTyped v) LD.LabeledDependency
      varRelation = labelling uf
      involvedVars :: Set (TermedOrTyped v)
      involvedVars = computeInvolvedVars uf defsToConsider varRelation
      codebaseNames :: Names
      codebaseNames = computeNamesWithDeprecations uf unalteredCodebaseNames involvedVars
      varDeps :: Map (TermedOrTyped v) (Set (TermedOrTyped v))
      varDeps = computeVarDeps uf involvedVars
      analysis :: SlurpAnalysis v
      analysis = computeVarStatuses varDeps varRelation codebaseNames
      varsByStatus :: VarsByStatus v
      varsByStatus = results analysis
      slurpResult :: SR.SlurpResult v
      slurpResult =
        toSlurpResult uf (fromMaybe UpdateOp slurpOp) defsToConsider varsByStatus
          & addAliases codebaseNames currentPath
   in pTraceShowId slurpResult

computeNamesWithDeprecations ::
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Names ->
  Set (TermedOrTyped v) ->
  Names
computeNamesWithDeprecations uf unalteredCodebaseNames involvedVars =
  pTraceShow ("Deprecated constructors", deprecatedConstructors)
    . pTraceShow ("constructorNamesInFile", constructorsUnderConsideration)
    $ codebaseNames
  where
    -- Get the set of all DIRECT definitions in the file which a definition depends on.
    codebaseNames :: Names
    codebaseNames =
      Names.filter (`Set.notMember` deprecatedConstructors) unalteredCodebaseNames
    constructorsUnderConsideration :: Set Name
    constructorsUnderConsideration =
      Map.toList (UF.dataDeclarationsId' uf)
        <> (fmap . fmap . fmap) DD.toDataDecl (Map.toList (UF.effectDeclarationsId' uf))
          & filter (\(typeV, _) -> Set.member (Typed typeV) involvedVars)
          & concatMap (\(_typeV, (_refId, decl)) -> DD.constructors' decl)
          & fmap
            ( \(_ann, v, _typ) -> Name.unsafeFromVar v
            )
          & Set.fromList

    deprecatedConstructors :: Set Name
    deprecatedConstructors =
      let oldRefsForEditedTypes = Set.unions $ do
            let declNames = Map.keys (UF.dataDeclarationsId' uf)
            let effectNames = Map.keys (UF.effectDeclarationsId' uf)
            typeName <- declNames <> effectNames
            when (not . null $ involvedVars) (guard (Typed typeName `Set.member` involvedVars))
            pure $ Names.typesNamed unalteredCodebaseNames (Name.unsafeFromVar typeName)
          existingConstructorsFromEditedTypes = Set.fromList $ do
            -- List Monad
            ref <- Set.toList oldRefsForEditedTypes
            (name, _ref) <- Names.constructorsForType ref unalteredCodebaseNames
            pure name
       in -- Compute any constructors which were deleted
          pTraceShow ("defsToConsider", involvedVars)
            . pTraceShow ("codebaseNames", unalteredCodebaseNames)
            . pTraceShow ("allRefIds", oldRefsForEditedTypes)
            . pTraceShow ("existingConstructorsFromEditedTypes", existingConstructorsFromEditedTypes)
            $ existingConstructorsFromEditedTypes `Set.difference` constructorsUnderConsideration

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
computeVarStatuses depMap varRelation codebaseNames =
  pTraceShow ("Statuses", statuses) $
    statuses
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
            actual -> error $ "Expected exactly one LabeledDependency in relation for var: " <> show tv <> " but got: " <> show actual
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
                -- If there are many existing terms, they must be in conflict.
                -- Currently we treat conflicts as errors rather than resolving them.
                _ -> DefErr Conflict
            LD.TermReference {} ->
              case Set.toList existingTermsAtName of
                [] -> DefOk New
                rs | any Referent.isConstructor rs -> DefErr TermCtorCollision
                [r]
                  | LD.referent r == ld -> DefOk Duplicate
                  | otherwise -> DefOk Updated
                -- If there are many existing terms, they must be in conflict.
                -- Currently we treat conflicts as errors rather than resolving them.
                _ -> DefErr Conflict
            LD.ConReference {} ->
              case Set.toList existingTermsAtName of
                [] -> DefOk New
                rs | any (not . Referent.isConstructor) rs -> DefErr CtorTermCollision
                [r]
                  | LD.referent r == ld -> DefOk Duplicate
                  | otherwise -> DefOk Updated
                -- If there are many existing terms, they must be in conflict.
                -- Currently we treat conflicts as errors rather than resolving them.
                _ -> DefErr Conflict

computeInvolvedVars ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Set v ->
  Rel.Relation (TermedOrTyped v) LD.LabeledDependency ->
  Set (TermedOrTyped v)
computeInvolvedVars uf defsToConsider varRelation
  | Set.null defsToConsider = Rel.dom varRelation
  | otherwise = allInvolvedVars
  where
    allInvolvedVars :: Set (TermedOrTyped v)
    allInvolvedVars =
      let existingVars :: Set (TermedOrTyped v) = Set.fromList $ do
            v <- Set.toList defsToConsider
            -- We don't know whether each var is a type or term, so we try both.
            tv <- [Typed v, Termed v]
            guard (Rel.memberDom tv varRelation)
            pure tv
       in varClosure uf existingVars

computeVarDeps ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Set (TermedOrTyped v) ->
  Map (TermedOrTyped v) (Set (TermedOrTyped v))
computeVarDeps uf allInvolvedVars =
  let depMap :: (Map (TermedOrTyped v) (Set (TermedOrTyped v)))
      depMap =
        allInvolvedVars
          & Set.toList
          & fmap
            ( \tv -> (tv, Set.delete tv $ varClosure uf (Set.singleton tv))
            )
          & Map.fromListWith (<>)
   in pTraceShow ("all involved variables", allInvolvedVars)
        . pTraceShow ("depmap", depMap)
        $ depMap

-- Compute the closure of all vars which the provided vars depend on.
varClosure :: Ord v => UF.TypecheckedUnisonFile v a -> Set (TermedOrTyped v) -> Set (TermedOrTyped v)
varClosure uf (sortVars -> sc) =
  mingleVars $ SC.closeWithDependencies uf sc

-- TODO: Does this need to contain constructors? Maybe.
-- Does not include constructors
labelling :: forall v a. (Ord v, Show v) => UF.TypecheckedUnisonFile v a -> Rel.Relation (TermedOrTyped v) LD.LabeledDependency
labelling uf =
  let result = decls <> effects <> terms
   in pTraceShow ("varRelation", result) $ result
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
  Set v ->
  VarsByStatus v ->
  OldSlurp.SlurpResult v
toSlurpResult uf op requestedVars varsByStatus =
  pTraceShowId $
    OldSlurp.SlurpResult
      { OldSlurp.originalFile = uf,
        OldSlurp.extraDefinitions =
          if Set.null requestedVars
            then mempty
            else
              let allVars = fold varsByStatus
                  desired =
                    requestedVars
                      & Set.flatMap (\v -> Set.fromList [Typed v, Termed v])
               in sortVars $ Set.difference allVars desired,
        OldSlurp.adds = adds,
        OldSlurp.duplicates = duplicates,
        OldSlurp.collisions = if op == AddOp then updates else mempty,
        OldSlurp.conflicts = conflicts,
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
    adds, duplicates, updates, termCtorColl, ctorTermColl, blocked, conflicts :: SlurpComponent v
    (adds, duplicates, updates, termCtorColl, (ctorTermColl, blocked, conflicts)) =
      varsByStatus
        & ifoldMap
          ( \k tvs ->
              let sc = sortVars $ tvs
               in case k of
                    Add -> (sc, mempty, mempty, mempty, (mempty, mempty, mempty))
                    Duplicated -> (mempty, sc, mempty, mempty, (mempty, mempty, mempty))
                    Update -> (mempty, mempty, sc, mempty, (mempty, mempty, mempty))
                    NeedsUpdate v ->
                      case op of
                        AddOp ->
                          (mempty, mempty, singletonSC v, mempty, (mempty, sc `SC.difference` singletonSC v, mempty))
                        UpdateOp ->
                          (sc, mempty, mempty, mempty, (mempty, mempty, mempty))
                    ErrFrom v TermCtorCollision -> (mempty, mempty, mempty, singletonSC v, (mempty, sc `SC.difference` singletonSC v, mempty))
                    ErrFrom v CtorTermCollision -> (mempty, mempty, mempty, mempty, (singletonSC v, sc `SC.difference` singletonSC v, mempty))
                    ErrFrom v Conflict -> (mempty, mempty, mempty, mempty, (mempty, sc `SC.difference` singletonSC v, singletonSC v))
                    SelfErr TermCtorCollision -> (mempty, mempty, mempty, sc, (mempty, mempty, mempty))
                    SelfErr CtorTermCollision -> (mempty, mempty, mempty, mempty, (sc, mempty, mempty))
                    SelfErr Conflict -> (mempty, mempty, mempty, mempty, (mempty, mempty, sc))
          )
    singletonSC = \case
      Typed v -> SlurpComponent {terms = mempty, types = Set.singleton v}
      Termed v -> SlurpComponent {terms = Set.singleton v, types = mempty}

anyErrors :: VarsByStatus v -> Bool
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

sortVars :: (Foldable f, Ord v) => f (TermedOrTyped v) -> SlurpComponent v
sortVars =
  foldMap
    ( \case
        Typed v -> SC.fromTypes (Set.singleton v)
        Termed v -> SC.fromTerms (Set.singleton v)
    )

mingleVars :: Ord v => SlurpComponent v -> Set (TermedOrTyped v)
mingleVars SlurpComponent {terms, types} =
  Set.map Typed types
    <> Set.map Termed terms

addAliases :: forall v. (Ord v, Var v) => Names -> Path.Absolute -> SR.SlurpResult v -> SR.SlurpResult v
addAliases existingNames curPath sr = sr {SR.termAlias = termAliases, SR.typeAlias = typeAliases}
  where
    fileNames = UF.typecheckedToNames $ SR.originalFile sr
    buildAliases ::
      Rel.Relation Name Referent ->
      Rel.Relation Name Referent ->
      Set v ->
      Map v SR.Aliases
    buildAliases existingNames namesFromFile dups =
      Map.fromList
        [ ( var n,
            if null aliasesOfOld
              then SR.AddAliases aliasesOfNew
              else SR.UpdateAliases aliasesOfOld aliasesOfNew
          )
          | (n, r@Referent.Ref {}) <- Rel.toList namesFromFile,
            -- All the refs whose names include `n`, and are not `r`
            let refs = Set.delete r $ Rel.lookupDom n existingNames
                aliasesOfNew =
                  Set.map (Path.unprefixName curPath) . Set.delete n $
                    Rel.lookupRan r existingNames
                aliasesOfOld =
                  Set.map (Path.unprefixName curPath) . Set.delete n . Rel.dom $
                    Rel.restrictRan existingNames refs,
            not (null aliasesOfNew && null aliasesOfOld),
            Set.notMember (var n) dups
        ]

    termAliases :: Map v SR.Aliases
    termAliases =
      buildAliases
        (Names.terms existingNames)
        (Names.terms fileNames)
        (SC.terms (SR.duplicates sr))

    typeAliases :: Map v SR.Aliases
    typeAliases =
      buildAliases
        (Rel.mapRan Referent.Ref $ Names.types existingNames)
        (Rel.mapRan Referent.Ref $ Names.types fileNames)
        (SC.types (SR.duplicates sr))

var :: Var v => Name -> v
var name = Var.named (Name.toText name)
