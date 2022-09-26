module Unison.Codebase.Editor.Slurp
  ( SlurpOp (..),
    slurpFile,
  )
where

import Control.Lens
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Referent' as Referent
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Relation as Rel
import qualified Unison.Util.Set as Set
import Unison.Var (Var)
import qualified Unison.Var as Var
import Unison.WatchKind (pattern TestWatch)

-- | The operation which is being performed or checked.
data SlurpOp
  = AddOp
  | UpdateOp
  | -- Run when the user saves the scratch file.
    CheckOp
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
data DefnStatus
  = -- | A constructor in the scratch file conflicts with a term in the codebase
    CtorTermCollision
  | Duplicated
  | New
  | -- | A term in the scratch file conflicts with a Ctor in the codebase
    TermCtorCollision
  | -- | The name of the term is already in the codebase (maybe more than once, i.e. conflicted)
    Updated
  deriving (Eq, Ord, Show)

-- | A coarser, totally-ordered variant of a defnintion's status, which summarizes its own status and the statuses of
-- all of its transitive dependencies.
--
-- For example, if any transitive dependency of a defnition requires an `update`, then so does the definition itself,
-- even if it's new (and thus ok to `add`).
--
-- Note: these must be defined in descending severity order, per @mostSevereDepStatus@!
data DepStatus
  = -- | Part of a term/ctor or ctor/term collision: neither `add` nor `update` ok
    DepCollision
  | -- | Requires an update: `add` not ok, `update` ok
    DepNeedsUpdate
  | -- | `add` or `update` both ok
    DepOk
  deriving stock (Eq, Ord, Show)

-- | Classify a definition status into a coarser dependency status.
defnStatusToDepStatus :: DefnStatus -> DepStatus
defnStatusToDepStatus = \case
  CtorTermCollision -> DepCollision
  Duplicated -> DepOk
  New -> DepOk
  TermCtorCollision -> DepCollision
  Updated -> DepNeedsUpdate

-- | DepCollision more severe than DepNeedsUpdate more severe than DepOk
mostSevereDepStatus :: DepStatus -> DepStatus -> DepStatus
mostSevereDepStatus =
  min

-- | Analyze a file and determine the status of all of its definitions with respect to a set
-- of vars to analyze and an operation you wish to perform.
slurpFile ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Set v ->
  SlurpOp ->
  Names ->
  SR.SlurpResult v
slurpFile uf defsToConsider slurpOp unalteredCodebaseNames =
  let -- A mapping of all vars in the file to their references.
      -- TypeVars are keyed to Type references
      -- TermVars are keyed to Term references
      -- ConstructorVars are keyed to Constructor references
      varReferences :: Map (TaggedVar v) LD.LabeledDependency
      varReferences = buildVarReferences uf
      -- All variables which were either:
      -- 1. specified explicitly by the end-user
      -- 2. An in-file transitive dependency (within the file) of a var specified by the end-user.
      involvedVars :: Set (TaggedVar v)
      involvedVars = computeInvolvedVars uf defsToConsider varReferences
      -- The set of names after removing any constructors which would
      -- be removed by the requested operation.
      codebaseNames :: Names
      codebaseNames = computeNamesWithDeprecations uf unalteredCodebaseNames involvedVars slurpOp
      -- A mapping of every involved variable to its transitive dependencies.
      -- Dependency here is any type or term referenced within the definition (transitively).
      -- This also includes all Constructors of any type used by a term.
      varDeps :: Map (TaggedVar v) (Set (TaggedVar v))
      varDeps = computeVarDeps uf involvedVars
      -- Compute the status of each definition on its own.
      -- This doesn't consider the vars dependencies.
      selfStatuses :: Map (TaggedVar v) DefnStatus
      selfStatuses = computeSelfStatuses involvedVars varReferences codebaseNames
      -- A mapping from each definition's name to the most severe status of it plus its transitive dependencies.
      depStatuses :: Map (TaggedVar v) DepStatus
      depStatuses = computeDepStatuses varDeps selfStatuses
   in toSlurpResult uf slurpOp defsToConsider involvedVars fileNames codebaseNames selfStatuses depStatuses
  where
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
computeNamesWithDeprecations uf unalteredCodebaseNames involvedVars = \case
  -- If we're 'adding', there won't be any deprecations to worry about.
  AddOp -> unalteredCodebaseNames
  CheckOp -> codebaseNames
  UpdateOp -> codebaseNames
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
          existingConstructorsFromEditedTypes `Set.difference` constructorsUnderConsideration

-- | Compute a mapping of each definition to its status.
computeSelfStatuses ::
  forall v.
  (Ord v, Var v) =>
  Set (TaggedVar v) ->
  Map (TaggedVar v) LD.LabeledDependency ->
  Names ->
  Map (TaggedVar v) DefnStatus
computeSelfStatuses vars varReferences codebaseNames =
  Map.fromSet definitionStatus vars
  where
    definitionStatus :: TaggedVar v -> DefnStatus
    definitionStatus tv =
      let ld = case Map.lookup tv varReferences of
            Just r -> r
            Nothing -> error $ "Expected LabeledDependency in map for var: " <> show tv
          v = untagged tv
          existingTypesAtName = Names.typesNamed codebaseNames (Name.unsafeFromVar v)
          existingTermsOrCtorsAtName = Names.termsNamed codebaseNames (Name.unsafeFromVar v)
       in case ld of
            LD.TypeReference _typeRef ->
              case Set.toList existingTypesAtName of
                [] -> New
                [r] | LD.typeRef r == ld -> Duplicated
                _ -> Updated
            LD.TermReference {} ->
              case Set.toList existingTermsOrCtorsAtName of
                [] -> New
                rs | any Referent.isConstructor rs -> TermCtorCollision
                [r] | LD.referent r == ld -> Duplicated
                _ -> Updated
            LD.ConReference {} ->
              case Set.toList existingTermsOrCtorsAtName of
                [] -> New
                rs | any (not . Referent.isConstructor) rs -> CtorTermCollision
                [r] | LD.referent r == ld -> Duplicated
                _ -> Updated

computeDepStatuses :: Ord k => Map k (Set k) -> Map k DefnStatus -> Map k DepStatus
computeDepStatuses varDeps selfStatuses =
  selfStatuses & Map.mapWithKey \name status -> do
    varDeps
      & Map.findWithDefault Set.empty name
      & Set.toList
      & mapMaybe (\depName -> defnStatusToDepStatus <$> Map.lookup depName selfStatuses)
      & Foldable.foldr mostSevereDepStatus (defnStatusToDepStatus status)

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
computeInvolvedVars uf defsToConsider varReferences
  -- If nothing was specified, consider every var in the file.
  | Set.null defsToConsider = Map.keysSet varReferences
  | otherwise = varClosure uf requestedVarsWhichActuallyExist
  where
    -- The user specifies _untyped_ names, which may not even exist in the file.
    -- We need to figure out which vars exist, and what type they are if they do.
    requestedVarsWhichActuallyExist :: Set (TaggedVar v)
    requestedVarsWhichActuallyExist = Set.fromList $ do
      v <- Set.toList defsToConsider
      -- We don't know whether each var is a type or term, so we try both.
      -- We don't test ConstructorVar because you can't request to add/update a Constructor in
      -- ucm, you add/update the type instead.
      tv <- [TypeVar v, TermVar v]
      guard (Map.member tv varReferences)
      pure tv

-- | Compute transitive dependencies for all relevant variables.
computeVarDeps ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Set (TaggedVar v) ->
  Map (TaggedVar v) (Set (TaggedVar v))
computeVarDeps uf allInvolvedVars =
  allInvolvedVars
    & Set.toList
    & fmap
      ( \tv -> (tv, Set.delete tv $ varClosure uf (Set.singleton tv))
      )
    & Map.fromAscList

-- | Compute the closure of all vars which the provided vars depend on.
-- A type depends on its constructors.
varClosure :: (Var v) => UF.TypecheckedUnisonFile v a -> Set (TaggedVar v) -> Set (TaggedVar v)
varClosure uf (partitionVars -> sc) =
  let deps = SC.closeWithDependencies uf sc
   in mingleVars deps

-- | Collect a relation of term or type var to labelled dependency for all definitions mentioned in a file.
buildVarReferences :: forall v a. (Ord v, Show v) => UF.TypecheckedUnisonFile v a -> Map (TaggedVar v) LD.LabeledDependency
buildVarReferences uf =
  decls <> effects <> terms <> constructors
  where
    terms :: Map (TaggedVar v) LD.LabeledDependency
    terms =
      UF.hashTermsId uf
        -- Filter out non-test watch expressions
        & Map.filter
          ( \case
              (_a, _, w, _, _)
                | w == Just TestWatch || w == Nothing -> True
                | otherwise -> False
          )
        & Map.bimap
          TermVar
          (\(_a, refId, _, _, _) -> LD.derivedTerm refId)
    decls :: Map (TaggedVar v) LD.LabeledDependency
    decls =
      UF.dataDeclarationsId' uf
        & Map.bimap
          TypeVar
          (\(refId, _) -> LD.derivedType refId)

    effects :: Map (TaggedVar v) LD.LabeledDependency
    effects =
      UF.effectDeclarationsId' uf
        & Map.bimap
          TypeVar
          (\(refId, _) -> LD.derivedType refId)

    constructors :: Map (TaggedVar v) LD.LabeledDependency
    constructors =
      let effectConstructors :: Map (TaggedVar v) LD.LabeledDependency
          effectConstructors = Map.fromList $ do
            (_, (typeRefId, effect)) <- Map.toList (UF.effectDeclarations' uf)
            let decl = DD.toDataDecl effect
            (conId, constructorV) <- zip (DD.constructorIds decl) (DD.constructorVars decl)
            pure $ (ConstructorVar constructorV, LD.effectConstructor (CR.ConstructorReference typeRefId conId))

          dataConstructors :: Map (TaggedVar v) LD.LabeledDependency
          dataConstructors = Map.fromList $ do
            (_, (typeRefId, decl)) <- Map.toList (UF.dataDeclarations' uf)
            (conId, constructorV) <- zip (DD.constructorIds decl) (DD.constructorVars decl)
            pure $ (ConstructorVar constructorV, LD.dataConstructor (CR.ConstructorReference typeRefId conId))
       in effectConstructors <> dataConstructors

-- A helper type just used by 'toSlurpResult' for partitioning results.
data SlurpingSummary v = SlurpingSummary
  { adds :: !(SlurpComponent v),
    duplicates :: !(SlurpComponent v),
    updates :: !(SlurpComponent v),
    termCtorColl :: !(SlurpComponent v),
    ctorTermColl :: !(SlurpComponent v),
    blocked :: !(SlurpComponent v)
  }

instance Ord v => Semigroup (SlurpingSummary v) where
  SlurpingSummary a b c d e f
    <> SlurpingSummary a' b' c' d' e' f' =
      SlurpingSummary
        (a <> a')
        (b <> b')
        (c <> c')
        (d <> d')
        (e <> e')
        (f <> f')

instance Ord v => Monoid (SlurpingSummary v) where
  mempty = SlurpingSummary mempty mempty mempty mempty mempty mempty

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
  Map (TaggedVar v) DefnStatus ->
  Map (TaggedVar v) DepStatus ->
  SR.SlurpResult v
toSlurpResult uf op requestedVars involvedVars fileNames codebaseNames selfStatuses depStatuses =
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
      SR.updates = if op /= AddOp then updates else mempty,
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
    SlurpingSummary {adds, duplicates, updates, termCtorColl, ctorTermColl, blocked} =
      ifoldMap summarize1 selfStatuses

    -- Compute a singleton summary for a single definition, per its own status and the most severe status of its
    -- transitive dependencies.
    summarize1 :: TaggedVar v -> DefnStatus -> SlurpingSummary v
    summarize1 name = \case
      CtorTermCollision -> mempty {ctorTermColl = sc}
      Duplicated -> mempty {duplicates = sc}
      TermCtorCollision -> mempty {termCtorColl = sc}
      New ->
        case depStatus of
          DepOk -> mempty {adds = sc}
          DepNeedsUpdate ->
            case op of
              AddOp -> mempty {blocked = sc}
              CheckOp -> mempty {adds = sc}
              UpdateOp -> mempty {adds = sc}
          DepCollision -> mempty {blocked = sc}
      Updated ->
        case depStatus of
          DepOk -> mempty {updates = sc}
          DepNeedsUpdate -> mempty {updates = sc}
          DepCollision -> mempty {blocked = sc}
      where
        sc :: SlurpComponent v
        sc =
          scFromTaggedVar name

        depStatus :: DepStatus
        depStatus =
          Map.findWithDefault DepOk name depStatuses

    scFromTaggedVar :: TaggedVar v -> SlurpComponent v
    scFromTaggedVar = \case
      TermVar v -> SC.fromTerms (Set.singleton v)
      TypeVar v -> SC.fromTypes (Set.singleton v)
      ConstructorVar v -> SC.fromCtors (Set.singleton v)

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
