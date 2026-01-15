module Unison.Codebase.Editor.HandleInput.Dependents
  ( handleDependents,
  )
where

import Control.Lens (review)
import Control.Monad.Reader (ask)
import Data.Bifoldable (binull)
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.These (These (..))
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQName)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.ConstructorReference (ConstructorReferenceId)
import Unison.DataDeclaration (DataDeclaration, Decl, EffectDeclaration)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LabeledDependency
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NamesUtils qualified as NamesUtils
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent'
import Unison.Syntax.HashQualifiedPrime qualified as HQ'
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.UnisonFile.Names qualified as UnisonFile
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, defnsAreEmpty, zipDefnsWith)
import Unison.Util.Map qualified as Map
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Unison.WatchKind (WatchKind)
import Unison.WatchKind qualified as WatchKind

handleDependents :: HQ.HashQualified Name -> Cli ()
handleDependents hq = do
  codebaseRefs <- resolveHQName hq

  -- If the given name doesn't match anything in the codebase, then as a fallback, we look at the latest Unison file to
  -- report dependents. This covers the common case that something (and all of its dependents) were removed from the
  -- underlying namespace and placed in a file, e.g. when resolving a failed update.
  if defnsAreEmpty codebaseRefs
    then handleFileDependents hq
    else handleCodebaseDependents codebaseRefs

handleCodebaseDependents :: DefnsF Set Referent TypeReference -> Cli ()
handleCodebaseDependents dependenciesRefs = do
  namespace <- Cli.getCurrentProjectRoot0
  let ppe =
        let names = Branch.toNames (Branch.deleteLibdeps namespace)
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  dependents0 <-
    Cli.runTransaction do
      Operations.directDependentsWithinScope
        (Branch.deepDefnsIds namespace)
        (NamesUtils.referentsToRefs dependenciesRefs)

  -- Here we have the dependents of all of the dependencies, having treated all constructors as the entire type, because
  -- that's all our dependencies index currently supports.
  --
  -- So, we have some special logic: if the dependencies actually contained *any* constructors (common case: just 1),
  -- then we do a slower thing of hydrating all dependents and keeping only the ones whose direct dependencies overlaps
  -- with the input set.
  dependents :: DefnsF Set TermReferenceId TypeReferenceId <-
    case Foldable.find Referent'.isConstructor dependenciesRefs.terms of
      Nothing -> pure dependents0
      Just _ -> do
        env <- ask
        Cli.runTransaction do
          dependentTermRefs :: Set TermReferenceId <-
            Set.filterM
              ( \dependentTermRef ->
                  Codebase.getTerm env.codebase dependentTermRef <&> \case
                    Just dependentTerm ->
                      let (dependentTermTermDependencies, dependentTermTypeDependencies) =
                            Set.unalignWith
                              ( \case
                                  LabeledDependency.TermReferent x -> This x
                                  LabeledDependency.TypeReference x -> That x
                              )
                              (Term.labeledDependencies dependentTerm)
                       in Set.intersects dependentTermTermDependencies dependenciesRefs.terms
                            || Set.intersects dependentTermTypeDependencies dependenciesRefs.types
                    Nothing -> False
              )
              dependents0.terms
          dependentTypeRefs :: Set TypeReferenceId <-
            if Set.null dependenciesRefs.types
              then pure Set.empty
              else
                Set.filterM
                  ( \dependentTypeRef ->
                      Codebase.getTypeDeclaration env.codebase dependentTypeRef <&> \case
                        Just dependentType ->
                          Set.intersects
                            (DataDeclaration.typeDependencies (DataDeclaration.asDataDecl dependentType))
                            dependenciesRefs.types
                        Nothing -> False
                  )
                  dependents0.types
          pure Defns {terms = dependentTermRefs, types = dependentTypeRefs}

  let dependentNames ::
        DefnsF
          []
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
      dependentNames =
        nameDependents ppe dependents

  -- Set numbered args
  (dependentNames.types ++ dependentNames.terms)
    & map (SA.HashQualified . HQ'.toHQ . fst)
    & Cli.setNumberedArgs

  let ppe =
        let names = Branch.toNames namespace
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  -- Name the dependencies, for output
  let namedDependencies :: DefnsF2 Set HQ.HashQualified Name Name
      namedDependencies =
        nameDependencies ppe dependenciesRefs

  maybeUnisonFile <-
    Cli.getLatestTypecheckedFile

  -- Determine whether we want to put an "(in codebase)" next to each dependency. We do when that name is also found
  -- in the latest typechecked file (since we're reporting on the codebase version).
  let namedDependencies1 :: DefnsF2 (Map (HQ.HashQualified Name)) Maybe Bool Bool
      namedDependencies1 =
        case maybeUnisonFile of
          Nothing -> let f = Map.fromSet \_ -> Nothing in bimap f f namedDependencies
          Just unisonFile ->
            let f :: Map Name ref -> Set (HQ.HashQualified Name) -> Map (HQ.HashQualified Name) (Maybe Bool)
                f defnsInFile =
                  Map.fromSet \case
                    HQ.NameOnly name | Map.member name defnsInFile -> Just False
                    _ -> Nothing
             in zipDefnsWith f f (fileToReferentsIds unisonFile) namedDependencies

  Cli.respond (ListDependents namedDependencies1 dependentNames)

handleFileDependents :: HQ.HashQualified Name -> Cli ()
handleFileDependents hq = do
  -- If searching with a hash, don't bother looking in the file.
  name <-
    case hq of
      HQ.NameOnly name -> pure name
      _ -> notFound

  -- Require a latest typechecked file to search.
  unisonFile <-
    Cli.getLatestTypecheckedFile & onNothingM do
      notFound

  -- Search the file for dependencies that match the given name.
  let dependenciesRefs :: DefnsF Set Referent.Id TypeReferenceId
      dependenciesRefs =
        unisonFile
          & fileToReferentsIds
          & bimap search search
        where
          search :: (Ord ref) => Map Name ref -> Set ref
          search defns =
            Name.gsearchBySuffix
              (maybe Set.empty Set.singleton . (`Map.lookup` defns))
              (\order -> Map.search (\_ -> Set.singleton) order defns)
              name

  when (binull dependenciesRefs) do
    notFound

  let dependenciesRefs1 :: DefnsF Set TermReferenceId TypeReferenceId
      dependenciesRefs1 =
        NamesUtils.referentsToRefs dependenciesRefs

  namespace <- Cli.getCurrentProjectRoot0
  let ppe =
        let names =
              namespace
                & Branch.deleteLibdeps
                & Branch.toNames
                & UnisonFile.addNamesFromTypeCheckedUnisonFile unisonFile
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  let dependents :: DefnsF Set TermReferenceId TypeReferenceId
      dependents =
        identifyFileDependents dependenciesRefs1 unisonFile

  let dependentNames ::
        DefnsF
          []
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
      dependentNames =
        nameDependents ppe dependents

  -- Set numbered args
  (dependentNames.types ++ dependentNames.terms)
    & map (SA.HashQualified . HQ'.toHQ . fst)
    & Cli.setNumberedArgs

  Cli.respond $
    ListDependents
      ( let f = Map.fromSet \_ -> Just True
         in bimap
              f
              f
              ( nameDependencies
                  ppe
                  ( bimap
                      (Set.map Referent.fromId)
                      (Set.map Reference.fromId)
                      dependenciesRefs
                  )
              )
      )
      dependentNames
  where
    notFound :: Cli a
    notFound =
      Cli.returnEarly (LabeledReferenceNotFound hq)

-- Extract the referents ids out of a unison file. This probably doesn't belong in this module; it's just kind of a
-- variant of 'toNames', but more efficient (no relations, just maps) and with fewer impossible cases (e.g. conflicted
-- names, builtins).
fileToReferentsIds :: forall a v. (Var v) => TypecheckedUnisonFile v a -> DefnsF (Map Name) Referent.Id TypeReferenceId
fileToReferentsIds unisonFile =
  Defns
    { terms =
        Map.empty
          & addTerms unisonFile.hashTermsId
          & addConstructors (UnisonFile.constructorsId unisonFile),
      types =
        Map.empty
          & addDecls unisonFile.dataDeclarationsId'
          & addDecls unisonFile.effectDeclarationsId'
    }
  where
    addTerms ::
      Map v (a, TermReferenceId, Maybe WatchKind, Term v a, Type v a) ->
      Map Name Referent.Id ->
      Map Name Referent.Id
    addTerms terms acc =
      Map.foldlWithKey' f acc terms
      where
        f acc var (_, ref, _, _, _) =
          Map.insert
            (Name.unsafeParseVar var)
            (review Referent'.termReference_ ref)
            acc

    addConstructors :: Map v (ConstructorReferenceId, Decl v a) -> Map Name Referent.Id -> Map Name Referent.Id
    addConstructors constructors acc =
      Map.foldlWithKey' f acc constructors
      where
        f acc var (ref, decl) =
          Map.insert
            (Name.unsafeParseVar var)
            (Referent'.Con' ref (DataDeclaration.constructorType decl))
            acc

    addDecls :: Map v (TypeReferenceId, decl) -> Map Name TypeReferenceId -> Map Name TypeReferenceId
    addDecls decls acc =
      Map.foldlWithKey' f acc decls
      where
        f acc var (ref, _) =
          Map.insert (Name.unsafeParseVar var) ref acc

identifyFileDependents ::
  forall a v.
  (Ord v) =>
  DefnsF Set TermReferenceId TypeReferenceId ->
  TypecheckedUnisonFile v a ->
  DefnsF Set TermReferenceId TypeReferenceId
identifyFileDependents dependencies unisonFile =
  Defns
    { terms =
        Set.union
          (foldMap (lookupSet termTermDependents) dependencies.terms)
          (foldMap (lookupSet typeTermDependents) dependencies.types),
      types = foldMap (lookupSet typeTypeDependents) dependencies.types
    }
  where
    termTermDependents :: Map TermReferenceId (NESet TermReferenceId)
    typeTermDependents :: Map TypeReferenceId (NESet TermReferenceId)
    (termTermDependents, typeTermDependents) =
      termDependenciesByDependent dependencies unisonFile.hashTermsId

    typeTypeDependents :: Map TypeReferenceId (NESet TypeReferenceId)
    typeTypeDependents =
      Map.union
        (typeDependenciesByDependent dependencies unisonFile.dataDeclarationsId')
        ( typeDependenciesByDependent
            dependencies
            ( coerce
                @(Map v (TypeReferenceId, EffectDeclaration v a))
                @(Map v (TypeReferenceId, DataDeclaration v a))
                unisonFile.effectDeclarationsId'
            )
        )

    lookupSet :: forall k a. (Ord k) => Map k (NESet a) -> k -> Set a
    lookupSet m k =
      maybe Set.empty Set.NonEmpty.toSet (Map.lookup k m)

termDependenciesByDependent ::
  (Ord v) =>
  DefnsF Set TermReferenceId TypeReferenceId ->
  Map v (a, TermReferenceId, Maybe WatchKind, Term v a, Type v a) ->
  (Map TermReferenceId (NESet TermReferenceId), Map TypeReferenceId (NESet TermReferenceId))
termDependenciesByDependent dependenciesRefs1 =
  Map.foldl' f (Map.empty, Map.empty)
  where
    f (accTerms, accTypes) (_, x, wk, term, _)
      | WatchKind.watchKindShouldBeStoredInDatabase wk =
          ( Set.foldl' dependsOnTerm accTerms dependencies.terms,
            Set.foldl' dependsOnType accTypes dependencies.types
          )
      | otherwise = (accTerms, accTypes)
      where
        dependencies :: DefnsF Set TermReference TypeReference
        dependencies =
          Term.dependencies term

        -- If `term x` depends on `term y`, and `term y` is in the set of things we want to report dependents of,
        -- then record `term y` => {`term x`} in our term dependents map.
        dependsOnTerm acc y =
          fromMaybe acc do
            y' <- Reference.toId y
            guard (Set.member y' dependenciesRefs1.terms)
            Just (Map.upsert (maybe (Set.NonEmpty.singleton x) (Set.NonEmpty.insert x)) y' acc)

        -- If `term x` depends on `type y`, and `type y` is in the set of things we want to report dependents of,
        -- then record `type y` => {`term x`} in our type dependents map.
        dependsOnType acc y =
          fromMaybe acc do
            y' <- Reference.toId y
            guard (Set.member y' dependenciesRefs1.types)
            Just (Map.upsert (maybe (Set.NonEmpty.singleton x) (Set.NonEmpty.insert x)) y' acc)

typeDependenciesByDependent ::
  (Ord v) =>
  DefnsF Set TermReferenceId TypeReferenceId ->
  Map v (TypeReferenceId, DataDeclaration v a) ->
  Map TypeReferenceId (NESet TypeReferenceId)
typeDependenciesByDependent dependencies =
  Map.foldl' f Map.empty
  where
    f acc (x, dataDecl) =
      Set.foldl (g x) acc (DataDeclaration.typeDependencies dataDecl)

    -- If `type x` depends on `type y`, and `type y` is in the set of things we want to report dependents of,
    -- either directly or because we want to report dependents of one of its constructors, then record
    -- `type y` => {`type x`} in our type dependents map.
    g ::
      TypeReferenceId ->
      Map TypeReferenceId (NESet TypeReferenceId) ->
      TypeReference ->
      Map TypeReferenceId (NESet TypeReferenceId)
    g x acc y =
      fromMaybe acc do
        y' <- Reference.toId y
        guard (Set.member y' dependencies.types)
        Just (Map.upsert (maybe (Set.NonEmpty.singleton x) (Set.NonEmpty.insert x)) y' acc)

nameDependencies ::
  PPE.PrettyPrintEnv ->
  DefnsF Set Referent TypeReference ->
  DefnsF2 Set HQ.HashQualified Name Name
nameDependencies ppe =
  bimap
    (Set.map (PPE.termNameOrHashOnly ppe))
    (Set.map (PPE.typeNameOrHashOnly ppe))

nameDependents ::
  PPE.PrettyPrintEnv ->
  DefnsF Set TermReferenceId TypeReferenceId ->
  DefnsF
    []
    (HQ'.HashQualified Name, HQ'.HashQualified Name)
    (HQ'.HashQualified Name, HQ'.HashQualified Name)
nameDependents ppe =
  bimap
    (f (Referent.fromTermReferenceId >>> PPE.termNames ppe))
    (f (Reference.fromId >>> PPE.typeNames ppe))
  where
    f g =
      Set.toList
        >>> mapMaybe (g >>> listToMaybe)
        >>> Name.sortByText (fst >>> HQ'.toText)
