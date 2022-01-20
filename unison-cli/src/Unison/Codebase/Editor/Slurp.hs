module Unison.Codebase.Editor.Slurp
  ( SlurpOp (..),
    slurpFile,
  )
where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Debug.Pretty.Simple (pTraceShow, pTraceShowId)
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import qualified Unison.Codebase.Editor.SlurpResult as SR
import qualified Unison.ConstructorReference as CR
import qualified Unison.DataDeclaration as DD
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Reference as Ref
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

-- | The operation which is being performed or checked.
data SlurpOp = AddOp | UpdateOp
  deriving (Eq, Show)

-- | Tag a variable as representing a term, type, or constructor
data TaggedVar v = TermVar v | TypeVar v | ConstructorVar v
  deriving (Eq, Ord, Show)

-- | Extract the var from a TaggedVar
untagged :: TaggedVar v -> v
untagged (TermVar v) = v
untagged (TypeVar v) = v
untagged (ConstructorVar v) = v

-- | A definition's status with relation to the codebase.
data SlurpOk
  = New
  | Updated
  | Duplicated
  deriving (Eq, Ord, Show)

-- | A definition's final status, incorporating the statuses of all of its dependencies.
data SummarizedStatus v
  = Ok SlurpOk
  | NeedsUpdate (TaggedVar v)
  | ErrFrom (TaggedVar v) SlurpErr
  | SelfErr SlurpErr
  deriving (Eq, Ord, Show)

-- | Ideally we would display all available information about each var to the end-user,
-- but for now we simply pick the "most important" issue to maintain backwards compatibility
-- with current behaviour.
pickPriorityStatus :: SummarizedStatus v -> SummarizedStatus v -> SummarizedStatus v
pickPriorityStatus a b =
  case (a, b) of
    -- If the definition has its own error, that takes highest priority.
    (SelfErr err, _) -> SelfErr err
    (_, SelfErr err) -> SelfErr err
    -- Next we care if a dependency has an error
    (ErrFrom v err, _) -> ErrFrom v err
    (_, ErrFrom v err) -> ErrFrom v err
    -- If our definition needs its own update then we don't care if dependencies need updates.
    (Ok Updated, _) -> Ok Updated
    (_, Ok Updated) -> Ok Updated
    (NeedsUpdate v, _) -> NeedsUpdate v
    (_, NeedsUpdate v) -> NeedsUpdate v
    -- 'New' definitions take precedence over duplicated dependencies
    (Ok New, _) -> Ok New
    (_, Ok New) -> Ok New
    (Ok Duplicated, _) -> Ok Duplicated

-- | Possible error conditions for a definition.
data SlurpErr
  = -- | A term in the scratch file conflicts with a Ctor in the codebase
    TermCtorCollision
  | -- | A constructor in the scratch file conflicts with a term in the codebase
    CtorTermCollision
  | -- | The name of this term is conflicted in the codebase.
    Conflict
  deriving (Eq, Ord, Show)

-- | Possible statuses for a given definition
data DefnStatus
  = DefOk SlurpOk
  | DefErr SlurpErr
  deriving (Show)

-- | A map of variables to their status, and all of their dependencies' statuses.
type SlurpAnalysis v = Map (TaggedVar v) (DefnStatus, Map (TaggedVar v) DefnStatus)

-- | A mapping of each status to the vars which have that status.
type VarsByStatus v = Map (SummarizedStatus v) (NESet (TaggedVar v))

-- | Compute all definitions which can be added, or the reasons why a def can't be added.
groupByStatus :: forall v. (Ord v, Show v) => SlurpAnalysis v -> VarsByStatus v
groupByStatus sr =
  pTraceShowId $ analyzed
  where
    analyzed :: Map (SummarizedStatus v) (NESet (TaggedVar v))
    analyzed =
      sr
        & Map.toList
        & fmap
          ( \(tv, (defNotes, deps)) ->
              ( let selfStatus = toSummary False defNotes tv
                    summaryOfDeps = (Map.toList deps <&> \(depTV, depDefNotes) -> toSummary True depDefNotes depTV)
                 in foldl' pickPriorityStatus selfStatus summaryOfDeps,
                NESet.singleton tv
              )
          )
        & Map.fromListWith (<>)

    toSummary :: (Ord v, Show v) => Bool -> DefnStatus -> TaggedVar v -> SummarizedStatus v
    toSummary isDep defNotes tv =
      case defNotes of
        DefOk Updated -> if isDep then NeedsUpdate tv else Ok Updated
        DefErr err ->
          if isDep
            then ErrFrom tv err
            else SelfErr err
        DefOk New -> Ok New
        DefOk Duplicated -> Ok Duplicated

-- | Analyze a file and determine the status of all of its definitions with respect to a set
-- of vars to analyze and an operation you wish to perform.
slurpFile ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Set v ->
  Maybe SlurpOp ->
  Names ->
  SR.SlurpResult v
slurpFile uf defsToConsider maybeSlurpOp unalteredCodebaseNames =
  let varRelation :: Map (TaggedVar v) LD.LabeledDependency
      varRelation = fileDefinitions uf
      involvedVars :: Set (TaggedVar v)
      involvedVars = computeInvolvedVars uf defsToConsider varRelation
      codebaseNames :: Names
      codebaseNames = computeNamesWithDeprecations uf unalteredCodebaseNames involvedVars slurpOp
      varDeps :: Map (TaggedVar v) (Set (TaggedVar v))
      varDeps = computeVarDeps uf involvedVars
      analysis :: SlurpAnalysis v
      analysis = computeSlurpAnalysis varDeps varRelation fileNames codebaseNames
      varsByStatus :: VarsByStatus v
      varsByStatus = groupByStatus analysis
      slurpResult :: SR.SlurpResult v
      slurpResult =
        toSlurpResult uf slurpOp defsToConsider involvedVars fileNames codebaseNames varsByStatus
   in pTraceShowId slurpResult
  where
    slurpOp :: SlurpOp
    slurpOp = fromMaybe UpdateOp maybeSlurpOp

    fileNames :: Names
    fileNames = UF.typecheckedToNames uf

-- | Return a modified set of names with constructors which would be deprecated by possible
-- updates are removed.
computeNamesWithDeprecations ::
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Names ->
  Set (TaggedVar v) ->
  SlurpOp ->
  Names
computeNamesWithDeprecations _uf unalteredCodebaseNames _involvedVars AddOp =
  -- If we're 'adding', there won't be any deprecations.
  unalteredCodebaseNames
computeNamesWithDeprecations uf unalteredCodebaseNames involvedVars UpdateOp =
  codebaseNames
  where
    -- Get the set of all DIRECT definitions in the file which a definition depends on.
    codebaseNames :: Names
    codebaseNames =
      Names.filter (`Set.notMember` deprecatedConstructors) unalteredCodebaseNames
    constructorsUnderConsideration :: Set Name
    constructorsUnderConsideration =
      Map.toList (UF.dataDeclarationsId' uf)
        <> (fmap . fmap . fmap) DD.toDataDecl (Map.toList (UF.effectDeclarationsId' uf))
          & filter (\(typeV, _) -> Set.member (TypeVar typeV) involvedVars)
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
            when (not . null $ involvedVars) (guard (TypeVar typeName `Set.member` involvedVars))
            pure $ Names.typesNamed unalteredCodebaseNames (Name.unsafeFromVar typeName)
          existingConstructorsFromEditedTypes = Set.fromList $ do
            -- List Monad
            ref <- Set.toList oldRefsForEditedTypes
            (name, _ref) <- Names.constructorsForType ref unalteredCodebaseNames
            pure name
       in -- Compute any constructors which were deleted
          pTraceShow ("defsToConsider", involvedVars) $
            existingConstructorsFromEditedTypes `Set.difference` constructorsUnderConsideration

-- | Compute a mapping of each definition to its status, and its dependencies' statuses.
computeSlurpAnalysis ::
  forall v.
  (Ord v, Var v) =>
  Map (TaggedVar v) (Set (TaggedVar v)) ->
  Map (TaggedVar v) LD.LabeledDependency ->
  Names ->
  Names ->
  SlurpAnalysis v
computeSlurpAnalysis depMap varRelation _fileNames codebaseNames =
  pTraceShow ("Statuses", statuses) $
    statuses
  where
    statuses :: Map (TaggedVar v) (DefnStatus, Map (TaggedVar v) (DefnStatus))
    statuses =
      let withNotes =
            depMap
              & Map.mapWithKey
                ( \tv deps ->
                    (definitionStatus tv, deps)
                )
          withTransitiveNotes ::
            ( Map
                (TaggedVar v)
                ( DefnStatus,
                  (Map (TaggedVar v) DefnStatus)
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
    definitionStatus :: TaggedVar v -> DefnStatus
    definitionStatus tv =
      let ld = case Map.lookup tv varRelation of
            Just r -> r
            Nothing -> error $ "Expected LabeledDependency in map for var: " <> show tv
          v = untagged tv
          existingTypesAtName = Names.typesNamed codebaseNames (Name.unsafeFromVar v)
          existingTermsOrCtorsAtName = Names.termsNamed codebaseNames (Name.unsafeFromVar v)
       in case ld of
            LD.TypeReference _typeRef ->
              case Set.toList existingTypesAtName of
                [] -> DefOk New
                [r]
                  | LD.typeRef r == ld -> DefOk Duplicated
                  | otherwise -> DefOk Updated
                -- If there are many existing types, they must be in conflict.
                -- Currently we treat conflicts as errors rather than resolving them.
                _ -> DefErr Conflict
            LD.TermReference {} ->
              case Set.toList existingTermsOrCtorsAtName of
                [] -> DefOk New
                rs | any Referent.isConstructor rs -> DefErr TermCtorCollision
                [r]
                  | LD.referent r == ld -> DefOk Duplicated
                  | otherwise -> DefOk Updated
                -- If there are many existing terms, they must be in conflict.
                -- Currently we treat conflicts as errors rather than resolving them.
                _ -> DefErr Conflict
            LD.ConReference {} ->
              case Set.toList existingTermsOrCtorsAtName of
                [] -> DefOk New
                rs | any (not . Referent.isConstructor) rs -> DefErr CtorTermCollision
                [r]
                  | LD.referent r == ld -> DefOk Duplicated
                  | otherwise -> DefOk Updated
                -- If there are many existing terms, they must be in conflict.
                -- Currently we treat conflicts as errors rather than resolving them.
                _ -> DefErr Conflict

-- | Determine all variables which should be considered in analysis.
-- I.e. any variable requested by the user and all of their dependencies,
-- component peers, and component peers of dependencies.
computeInvolvedVars ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Set v ->
  Map (TaggedVar v) LD.LabeledDependency ->
  Set (TaggedVar v)
computeInvolvedVars uf defsToConsider varRelation
  | Set.null defsToConsider = Set.fromList $ Map.keys varRelation
  | otherwise = allInvolvedVars
  where
    allInvolvedVars :: Set (TaggedVar v)
    allInvolvedVars =
      let requestedVarsWhichActuallyExist :: Set (TaggedVar v)
          requestedVarsWhichActuallyExist = Set.fromList $ do
            v <- Set.toList defsToConsider
            -- We don't know whether each var is a type or term, so we try both.
            tv <- [TypeVar v, TermVar v]
            guard (Map.member tv varRelation)
            pure tv
       in varClosure uf requestedVarsWhichActuallyExist

-- | Compute transitive dependencies for all relevant variables.
computeVarDeps ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Set (TaggedVar v) ->
  Map (TaggedVar v) (Set (TaggedVar v))
computeVarDeps uf allInvolvedVars =
  let depMap :: (Map (TaggedVar v) (Set (TaggedVar v)))
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

-- | Compute the closure of all vars which the provided vars depend on.
-- A type depends on its constructors.
varClosure :: (Var v) => UF.TypecheckedUnisonFile v a -> Set (TaggedVar v) -> Set (TaggedVar v)
varClosure uf (partitionVars -> sc) =
  let deps = SC.closeWithDependencies uf sc
   in mingleVars deps

-- | Collect a relation of term or type var to labelled dependency for all definitions mentioned in a file.
fileDefinitions :: forall v a. (Ord v, Show v) => UF.TypecheckedUnisonFile v a -> Map (TaggedVar v) LD.LabeledDependency
fileDefinitions uf =
  let result = decls <> effects <> terms <> constructors
   in pTraceShow ("varRelation", result) $ result
  where
    terms :: Map (TaggedVar v) LD.LabeledDependency
    terms =
      UF.hashTermsId uf
        & Map.toList
        -- TODO: ensure we handle watches with assignments correctly.
        -- Filter out watches
        & mapMaybe
          ( \case
              (v, (refId, w, _, _))
                | w == Just TestWatch || w == Nothing ->
                  Just (TermVar v, LD.derivedTerm refId)
              _ -> Nothing
          )
        & Map.fromList
    decls :: Map (TaggedVar v) LD.LabeledDependency
    decls =
      UF.dataDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (TypeVar v, LD.derivedType refId))
        & Map.fromList

    effects :: Map (TaggedVar v) LD.LabeledDependency
    effects =
      UF.effectDeclarationsId' uf
        & Map.toList
        & fmap (\(v, (refId, _)) -> (TypeVar v, (LD.derivedType refId)))
        & Map.fromList

    constructors :: Map (TaggedVar v) LD.LabeledDependency
    constructors =
      let effectConstructors :: Map (TaggedVar v) LD.LabeledDependency
          effectConstructors = Map.fromList $ do
            (_, (typeRefId, effect)) <- Map.toList (UF.effectDeclarationsId' uf)
            let decl = DD.toDataDecl effect
            (conId, constructorV) <- zip (DD.constructorIds decl) (DD.constructorVars decl)
            pure $ (ConstructorVar constructorV, LD.effectConstructor (CR.ConstructorReference (Ref.fromId typeRefId) conId))

          dataConstructors :: Map (TaggedVar v) LD.LabeledDependency
          dataConstructors = Map.fromList $ do
            (_, (typeRefId, decl)) <- Map.toList (UF.dataDeclarationsId' uf)
            (conId, constructorV) <- zip (DD.constructorIds decl) (DD.constructorVars decl)
            pure $ (ConstructorVar constructorV, LD.dataConstructor (CR.ConstructorReference (Ref.fromId typeRefId) conId))
       in effectConstructors <> dataConstructors

-- A helper type just used by 'toSlurpResult' for partitioning results.
data SlurpingSummary v = SlurpingSummary
  { adds :: SlurpComponent v,
    duplicates :: SlurpComponent v,
    updates :: SlurpComponent v,
    termCtorColl :: SlurpComponent v,
    ctorTermColl :: SlurpComponent v,
    blocked :: SlurpComponent v,
    conflicts :: SlurpComponent v
  }

instance (Ord v) => Semigroup (SlurpingSummary v) where
  SlurpingSummary a b c d e f g
    <> SlurpingSummary a' b' c' d' e' f' g' =
      SlurpingSummary
        (a <> a')
        (b <> b')
        (c <> c')
        (d <> d')
        (e <> e')
        (f <> f')
        (g <> g')

instance (Ord v) => Monoid (SlurpingSummary v) where
  mempty = SlurpingSummary mempty mempty mempty mempty mempty mempty mempty

-- | Convert a 'VarsByStatus' mapping into a 'SR.SlurpResult'
toSlurpResult ::
  forall v.
  (Var v) =>
  UF.TypecheckedUnisonFile v Ann ->
  SlurpOp ->
  Set v ->
  Set (TaggedVar v) ->
  Names ->
  Names ->
  VarsByStatus v ->
  SR.SlurpResult v
toSlurpResult uf op requestedVars involvedVars fileNames codebaseNames varsByStatus =
  pTraceShowId $
    SR.SlurpResult
      { SR.originalFile = uf,
        SR.extraDefinitions =
          if Set.null requestedVars
            then mempty
            else
              let desired =
                    requestedVars
                      & Set.flatMap (\v -> Set.fromList [TypeVar v, TermVar v])
               in partitionVars $ Set.difference involvedVars desired,
        SR.adds = adds,
        SR.duplicates = duplicates,
        SR.collisions = if op == AddOp then updates else mempty,
        SR.conflicts = conflicts,
        SR.updates = if op == UpdateOp then updates else mempty,
        SR.termExistingConstructorCollisions =
          let SlurpComponent {types, terms, ctors} = termCtorColl
           in types <> terms <> ctors,
        SR.constructorExistingTermCollisions =
          let SlurpComponent {types, terms, ctors} = ctorTermColl
           in types <> terms <> ctors,
        SR.termAlias = termAliases,
        SR.typeAlias = typeAliases,
        SR.defsWithBlockedDependencies = blocked
      }
  where
    -- Monoid instances only go up to 5-tuples :/
    SlurpingSummary {adds, duplicates, updates, termCtorColl, ctorTermColl, blocked, conflicts} =
      varsByStatus
        & ifoldMap
          ( \k tvs ->
              let scWithConstructors = partitionVars tvs
                  scWithoutConstructors = partitionVars tvs
               in case k of
                    Ok New -> mempty {adds = scWithoutConstructors}
                    Ok Duplicated -> mempty {duplicates = scWithoutConstructors}
                    Ok Updated -> mempty {updates = scWithoutConstructors}
                    NeedsUpdate _ ->
                      case op of
                        AddOp ->
                          mempty {blocked = scWithoutConstructors}
                        UpdateOp ->
                          mempty {adds = scWithoutConstructors}
                    ErrFrom _ TermCtorCollision -> mempty {blocked = scWithoutConstructors}
                    ErrFrom _ CtorTermCollision -> mempty {blocked = scWithoutConstructors}
                    ErrFrom _ Conflict -> mempty {blocked = scWithoutConstructors}
                    SelfErr TermCtorCollision -> mempty {termCtorColl = scWithConstructors}
                    SelfErr CtorTermCollision -> mempty {ctorTermColl = scWithConstructors}
                    SelfErr Conflict -> mempty {conflicts = scWithConstructors}
          )

    buildAliases ::
      Rel.Relation Name Referent ->
      Rel.Relation Name Referent ->
      Set v ->
      Map v SR.Aliases
    buildAliases existingNames namesFromFile dups =
      Map.fromList
        [ ( varFromName n,
            if null aliasesOfOld
              then SR.AddAliases aliasesOfNew
              else SR.UpdateAliases aliasesOfOld aliasesOfNew
          )
          | (n, r@Referent.Ref {}) <- Rel.toList namesFromFile,
            -- All the refs whose names include `n`, and are not `r`
            let refs = Set.delete r $ Rel.lookupDom n existingNames
                aliasesOfNew =
                  Set.delete n $
                    Rel.lookupRan r existingNames
                aliasesOfOld =
                  Set.delete n . Rel.dom $
                    Rel.restrictRan existingNames refs,
            not (null aliasesOfNew && null aliasesOfOld),
            Set.notMember (varFromName n) dups
        ]

    termAliases :: Map v SR.Aliases
    termAliases =
      buildAliases
        (Names.terms codebaseNames)
        (Names.terms fileNames)
        (SC.terms duplicates)

    typeAliases :: Map v SR.Aliases
    typeAliases =
      buildAliases
        (Rel.mapRan Referent.Ref $ Names.types codebaseNames)
        (Rel.mapRan Referent.Ref $ Names.types fileNames)
        (SC.types duplicates)

    varFromName :: Var v => Name -> v
    varFromName name = Var.named (Name.toText name)

-- | Sort out a set of variables by whether it is a term or type.
partitionVars :: (Foldable f, Ord v) => f (TaggedVar v) -> SlurpComponent v
partitionVars =
  foldMap
    ( \case
        TypeVar v -> SC.fromTypes (Set.singleton v)
        TermVar v -> SC.fromTerms (Set.singleton v)
        ConstructorVar v -> SC.fromCtors (Set.singleton v)
    )

-- | Collapse a SlurpComponent into a tagged set.
mingleVars :: Ord v => SlurpComponent v -> Set (TaggedVar v)
mingleVars SlurpComponent {terms, types, ctors} =
  Set.map TypeVar types
    <> Set.map TermVar terms
    <> Set.map ConstructorVar ctors
