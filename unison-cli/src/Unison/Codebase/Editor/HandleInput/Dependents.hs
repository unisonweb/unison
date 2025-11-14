module Unison.Codebase.Editor.HandleInput.Dependents
  ( handleDependents,
  )
where

import Control.Lens (review)
import Data.Bifoldable (bifoldMap, binull)
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
import Unison.ConstructorReference (ConstructorReferenceId, GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names (..))
import Unison.NamesUtils qualified as NamesUtils
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent'
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualifiedPrime qualified as HQ'
import Unison.Syntax.Name qualified as Name
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.UnisonFile qualified as UnisonFile
import Unison.UnisonFile.Names qualified as UnisonFile
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2)
import Unison.Util.Map qualified as Map
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.WatchKind qualified as WatchKind

handleDependents :: HQ.HashQualified Name -> Cli ()
handleDependents hq = do
  codebaseRefs <- resolveHQName hq

  -- If the given name doesn't match anything in the codebase, then as a fallback, we look at the latest Unison file to
  -- report dependents. This covers the common case that something (and all of its dependents) were removed from the
  -- underlying namespace and placed in a file, e.g. when resolving a failed update.
  if binull codebaseRefs
    then handleFileDependents hq
    else handleCodebaseDependents codebaseRefs

handleCodebaseDependents :: DefnsF Set Referent TypeReference -> Cli ()
handleCodebaseDependents dependenciesRefs = do
  namespace <- Cli.getCurrentProjectRoot0
  let ppe =
        let names = Branch.toNames (Branch.deleteLibdeps namespace)
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  dependents <-
    Cli.runTransaction do
      Operations.directDependentsWithinScope
        (Branch.deepDefnsIds namespace)
        (NamesUtils.referentsToRefs dependenciesRefs)

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

  Cli.respond (ListDependents (nameDependencies ppe dependenciesRefs) dependentNames)

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

  let fileConstructors :: Map Symbol (ConstructorReferenceId, Decl Symbol Ann)
      fileConstructors =
        UnisonFile.constructorsId unisonFile

  let fileTermReferences :: Relation Name Referent.Id
      fileTermReferences =
        Relation.empty
          & addRefs
          & addCons
        where
          addRefs :: Relation Name Referent.Id -> Relation Name Referent.Id
          addRefs acc =
            Map.foldlWithKey' f acc unisonFile.hashTermsId
            where
              f acc var (_, ref, _, _, _) =
                Relation.insert
                  (Name.unsafeParseVar var)
                  (review Referent'.termReference_ ref)
                  acc

          addCons :: Relation Name Referent.Id -> Relation Name Referent.Id
          addCons acc =
            Map.foldlWithKey' f acc fileConstructors
            where
              f ::
                Relation Name Referent.Id ->
                Symbol ->
                (ConstructorReferenceId, Decl Symbol Ann) ->
                Relation Name Referent.Id
              f acc var (ref, decl) =
                Relation.insert
                  (Name.unsafeParseVar var)
                  (Referent'.Con' ref (DataDeclaration.constructorType decl))
                  acc

  let fileTypeReferences :: Relation Name TypeReferenceId
      fileTypeReferences =
        Relation.empty
          & g unisonFile.dataDeclarationsId'
          & g unisonFile.effectDeclarationsId'
        where
          g :: Map Symbol (TypeReferenceId, decl) -> Relation Name TypeReferenceId -> Relation Name TypeReferenceId
          g decls acc =
            Map.foldlWithKey' f acc decls

          f :: Relation Name TypeReferenceId -> Symbol -> (TypeReferenceId, decl) -> Relation Name TypeReferenceId
          f acc var (ref, _) =
            Relation.insert (Name.unsafeParseVar var) ref acc

  -- Search the file for dependencies that match the given name.
  let dependenciesRefs :: DefnsF Set Referent.Id TypeReferenceId
      dependenciesRefs =
        Defns
          { terms = Name.searchByRankedSuffix name fileTermReferences,
            types = Name.searchByRankedSuffix name fileTypeReferences
          }

  when (binull dependenciesRefs) do
    notFound

  let dependenciesRefs1 :: DefnsF Set TermReferenceId TypeReferenceId
      dependenciesRefs1 =
        NamesUtils.referentsToRefs dependenciesRefs

  let termDependents :: Map TermReferenceId (NESet TermReferenceId)
      typeTermDependents :: Map TypeReferenceId (NESet TermReferenceId)
      (termDependents, typeTermDependents) =
        Map.foldl' f (Map.empty, Map.empty) unisonFile.hashTermsId
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
                  guard (Set.member (Referent'.Ref' y') dependenciesRefs.terms)
                  Just (Map.upsert (maybe (Set.NonEmpty.singleton x) (Set.NonEmpty.insert x)) y' acc)

              -- If `term x` depends on `type y`, and `type y` is in the set of things we want to report dependents of,
              -- then record `type y` => {`term x`} in our type dependents map.
              dependsOnType acc y =
                fromMaybe acc do
                  y' <- Reference.toId y
                  guard (Set.member y' dependenciesRefs.types)
                  Just (Map.upsert (maybe (Set.NonEmpty.singleton x) (Set.NonEmpty.insert x)) y' acc)

  let typeTypeDependents :: Map TypeReferenceId (NESet TypeReferenceId)
      typeTypeDependents =
        Map.foldl' g (Map.foldl' f Map.empty unisonFile.dataDeclarationsId') unisonFile.effectDeclarationsId'
        where
          f acc (x, dataDecl) =
            Set.foldl (h2 x) acc (DataDeclaration.typeDependencies dataDecl)

          g acc (x, effectDecl) =
            f acc (x, DataDeclaration.toDataDecl effectDecl)

          -- If `type x` depends on `type y`, and `type y` is in the set of things we want to report dependents of,
          -- either directly or because we want to report dependents of one of its constructors, then record
          -- `type y` => {`type x`} in our type dependents map.
          h2 ::
            TypeReferenceId ->
            Map TypeReferenceId (NESet TypeReferenceId) ->
            TypeReference ->
            Map TypeReferenceId (NESet TypeReferenceId)
          h2 x acc y =
            fromMaybe acc do
              y' <- Reference.toId y
              guard (Set.member y' dependenciesRefs1.types)
              Just (Map.upsert (maybe (Set.NonEmpty.singleton x) (Set.NonEmpty.insert x)) y' acc)

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
        let f deps ref =
              maybe Set.empty Set.NonEmpty.toSet (Map.lookup ref deps)
         in Defns
              { terms =
                  Set.union
                    (foldMap (f termDependents) dependenciesRefs1.terms)
                    (foldMap (f typeTermDependents) dependenciesRefs1.types),
                types = foldMap (f typeTypeDependents) dependenciesRefs1.types
              }

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
      ( nameDependencies
          ppe
          ( bimap
              (Set.map Referent.fromId)
              (Set.map Reference.fromId)
              dependenciesRefs
          )
      )
      dependentNames
  where
    notFound :: Cli a
    notFound =
      Cli.returnEarly (LabeledReferenceNotFound hq)

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
