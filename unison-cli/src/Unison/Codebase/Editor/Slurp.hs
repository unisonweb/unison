module Unison.Codebase.Editor.Slurp
  ( slurpFile,
  )
where

import Control.Lens
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import Unison.Codebase.Editor.SlurpComponent qualified as SC
import Unison.Codebase.Editor.SlurpResult qualified as SR
import Unison.ConstructorReference qualified as CR
import Unison.DataDeclaration qualified as DD
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toVar, unsafeParseVar)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Map qualified as Map
import Unison.Util.Relation qualified as Rel
import Unison.WatchKind (watchKindShouldBeStoredInDatabase)

-- | Tag a variable as representing a term, type, or constructor
data TaggedVar = TermVar Symbol | TypeVar Symbol | ConstructorVar Symbol
  deriving (Eq, Ord, Show)

-- | Extract the var from a TaggedVar
untagged :: TaggedVar -> Symbol
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
-- even if it's new.
--
-- Note: these must be defined in descending severity order, per @mostSevereDepStatus@!
data DepStatus
  = -- | Part of a term/ctor or ctor/term collision: `update` not ok
    DepCollision
  | -- | Requires an update: `add.run` not ok
    DepNeedsUpdate
  | -- | `update` ok
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
  UF.TypecheckedUnisonFile Symbol Ann ->
  Symbol ->
  Names ->
  SR.SlurpResult
slurpFile uf requestedVar unalteredCodebaseNames =
  let -- A mapping of all vars in the file to their references.
      -- TypeVars are keyed to Type references
      -- TermVars are keyed to Term references
      -- ConstructorVars are keyed to Constructor references
      varReferences :: Map TaggedVar LD.LabeledDependency
      varReferences = buildVarReferences uf
      -- All variables which were either:
      -- 1. specified explicitly by the end-user
      -- 2. An in-file transitive dependency (within the file) of a var specified by the end-user.
      involvedVars :: Set TaggedVar
      involvedVars = varClosure uf (TermVar requestedVar)
      -- The set of names after removing any constructors which would
      -- be removed by the requested operation.
      codebaseNames :: Names
      codebaseNames = unalteredCodebaseNames
      -- A mapping of every involved variable to its transitive dependencies.
      -- Dependency here is any type or term referenced within the definition (transitively).
      -- This also includes all Constructors of any type used by a term.
      varDeps :: Map TaggedVar (Set TaggedVar)
      varDeps = computeVarDeps uf involvedVars
      -- Compute the status of each definition on its own.
      -- This doesn't consider the vars dependencies.
      selfStatuses :: Map TaggedVar DefnStatus
      selfStatuses = computeSelfStatuses involvedVars varReferences codebaseNames
      -- A mapping from each definition's name to the most severe status of it plus its transitive dependencies.
      depStatuses :: Map TaggedVar DepStatus
      depStatuses = computeDepStatuses varDeps selfStatuses
   in toSlurpResult uf requestedVar involvedVars fileNames codebaseNames selfStatuses depStatuses
  where
    fileNames :: Names
    fileNames = UF.typecheckedToNames uf

-- | Compute a mapping of each definition to its status.
computeSelfStatuses ::
  Set TaggedVar ->
  Map TaggedVar LD.LabeledDependency ->
  Names ->
  Map TaggedVar DefnStatus
computeSelfStatuses vars varReferences codebaseNames =
  Map.fromSet definitionStatus vars
  where
    definitionStatus :: TaggedVar -> DefnStatus
    definitionStatus tv =
      let ld = case Map.lookup tv varReferences of
            Just r -> r
            Nothing -> error $ "Expected LabeledDependency in map for var: " <> show tv
          v = untagged tv
          existingTypesAtName = Names.typesNamed codebaseNames (Name.unsafeParseVar v)
          existingTermsOrCtorsAtName = Names.termsNamed codebaseNames (Name.unsafeParseVar v)
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

computeDepStatuses :: (Ord k) => Map k (Set k) -> Map k DefnStatus -> Map k DepStatus
computeDepStatuses varDeps selfStatuses =
  selfStatuses & Map.mapWithKey \name status -> do
    varDeps
      & Map.findWithDefault Set.empty name
      & Set.toList
      & mapMaybe (\depName -> defnStatusToDepStatus <$> Map.lookup depName selfStatuses)
      & Foldable.foldr mostSevereDepStatus (defnStatusToDepStatus status)

-- | Compute transitive dependencies for all relevant variables.
computeVarDeps ::
  UF.TypecheckedUnisonFile Symbol Ann ->
  Set TaggedVar ->
  Map TaggedVar (Set TaggedVar)
computeVarDeps uf allInvolvedVars =
  allInvolvedVars
    & Set.toList
    & fmap (\tv -> (tv, Set.delete tv $ varClosure uf tv))
    & Map.fromAscList

-- | Compute the closure of all vars which the provided var depends on.
-- A type depends on its constructors.
varClosure :: UF.TypecheckedUnisonFile Symbol a -> TaggedVar -> Set TaggedVar
varClosure uf var =
  mingleVars (SC.closeWithDependencies uf (partitionVars [var]))

-- | Collect a relation of term or type var to labelled dependency for all definitions mentioned in a file.
buildVarReferences :: UF.TypecheckedUnisonFile Symbol a -> Map TaggedVar LD.LabeledDependency
buildVarReferences uf =
  decls <> effects <> terms <> constructors
  where
    terms :: Map TaggedVar LD.LabeledDependency
    terms =
      UF.hashTermsId uf
        -- Filter out non-test watch expressions
        & Map.filter (\(_, _, w, _, _) -> watchKindShouldBeStoredInDatabase w)
        & Map.bimap
          TermVar
          (\(_, refId, _, _, _) -> LD.derivedTerm refId)
    decls :: Map TaggedVar LD.LabeledDependency
    decls =
      UF.dataDeclarationsId' uf
        & Map.bimap
          TypeVar
          (\(refId, _) -> LD.derivedType refId)

    effects :: Map TaggedVar LD.LabeledDependency
    effects =
      UF.effectDeclarationsId' uf
        & Map.bimap
          TypeVar
          (\(refId, _) -> LD.derivedType refId)

    constructors :: Map TaggedVar LD.LabeledDependency
    constructors =
      let effectConstructors :: Map TaggedVar LD.LabeledDependency
          effectConstructors = Map.fromList $ do
            (_, (typeRefId, effect)) <- Map.toList (UF.effectDeclarations' uf)
            let decl = DD.toDataDecl effect
            (conId, constructorV) <- zip (DD.constructorIds decl) (DD.constructorVars decl)
            pure $ (ConstructorVar constructorV, LD.effectConstructor (CR.ConstructorReference typeRefId conId))

          dataConstructors :: Map TaggedVar LD.LabeledDependency
          dataConstructors = Map.fromList $ do
            (_, (typeRefId, decl)) <- Map.toList (UF.dataDeclarations' uf)
            (conId, constructorV) <- zip (DD.constructorIds decl) (DD.constructorVars decl)
            pure $ (ConstructorVar constructorV, LD.dataConstructor (CR.ConstructorReference typeRefId conId))
       in effectConstructors <> dataConstructors

-- A helper type just used by 'toSlurpResult' for partitioning results.
data SlurpingSummary = SlurpingSummary
  { adds :: !SlurpComponent,
    duplicates :: !SlurpComponent,
    updates :: !SlurpComponent,
    termCtorColl :: !SlurpComponent,
    ctorTermColl :: !SlurpComponent,
    blocked :: !SlurpComponent
  }

instance Semigroup SlurpingSummary where
  SlurpingSummary a b c d e f
    <> SlurpingSummary a' b' c' d' e' f' =
      SlurpingSummary
        (a <> a')
        (b <> b')
        (c <> c')
        (d <> d')
        (e <> e')
        (f <> f')

instance Monoid SlurpingSummary where
  mempty = SlurpingSummary mempty mempty mempty mempty mempty mempty

-- | Convert a 'VarsByStatus' mapping into a 'SR.SlurpResult'
toSlurpResult ::
  UF.TypecheckedUnisonFile Symbol Ann ->
  Symbol ->
  Set TaggedVar ->
  Names ->
  Names ->
  Map TaggedVar DefnStatus ->
  Map TaggedVar DepStatus ->
  SR.SlurpResult
toSlurpResult uf requestedVar involvedVars fileNames codebaseNames selfStatuses depStatuses =
  SR.SlurpResult
    { SR.originalFile = uf,
      SR.extraDefinitions = partitionVars $ Set.delete (TermVar requestedVar) involvedVars,
      SR.adds = adds,
      SR.duplicates = duplicates,
      SR.collisions = updates,
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
    summarize1 :: TaggedVar -> DefnStatus -> SlurpingSummary
    summarize1 name = \case
      CtorTermCollision -> mempty {ctorTermColl = sc}
      Duplicated -> mempty {duplicates = sc}
      TermCtorCollision -> mempty {termCtorColl = sc}
      New ->
        case depStatus of
          DepOk -> mempty {adds = sc}
          DepNeedsUpdate -> mempty {blocked = sc}
          DepCollision -> mempty {blocked = sc}
      Updated ->
        case depStatus of
          DepOk -> mempty {updates = sc}
          DepNeedsUpdate -> mempty {updates = sc}
          DepCollision -> mempty {blocked = sc}
      where
        sc :: SlurpComponent
        sc =
          scFromTaggedVar name

        depStatus :: DepStatus
        depStatus =
          Map.findWithDefault DepOk name depStatuses

    scFromTaggedVar :: TaggedVar -> SlurpComponent
    scFromTaggedVar = \case
      TermVar v -> SC.fromTerms (Set.singleton v)
      TypeVar v -> SC.fromTypes (Set.singleton v)
      ConstructorVar v -> SC.fromCtors (Set.singleton v)

    buildAliases ::
      Rel.Relation Name Referent ->
      Rel.Relation Name Referent ->
      Set Symbol ->
      Map Symbol SR.Aliases
    buildAliases existingNames namesFromFile dups =
      Map.fromList
        [ ( Name.toVar n,
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
            Set.notMember (Name.toVar n) dups
        ]

    termAliases :: Map Symbol SR.Aliases
    termAliases =
      buildAliases
        (Names.terms codebaseNames)
        (Names.terms fileNames)
        (SC.terms duplicates)

    typeAliases :: Map Symbol SR.Aliases
    typeAliases =
      buildAliases
        (Rel.mapRan Referent.Ref $ Names.types codebaseNames)
        (Rel.mapRan Referent.Ref $ Names.types fileNames)
        (SC.types duplicates)

-- | Sort out a set of variables by whether it is a term or type.
partitionVars :: (Foldable f) => f TaggedVar -> SlurpComponent
partitionVars =
  foldMap
    ( \case
        TypeVar v -> SC.fromTypes (Set.singleton v)
        TermVar v -> SC.fromTerms (Set.singleton v)
        ConstructorVar v -> SC.fromCtors (Set.singleton v)
    )

-- | Collapse a SlurpComponent into a tagged set.
mingleVars :: SlurpComponent -> Set TaggedVar
mingleVars SlurpComponent {terms, types, ctors} =
  Set.map TypeVar types
    <> Set.map TermVar terms
    <> Set.map ConstructorVar ctors
