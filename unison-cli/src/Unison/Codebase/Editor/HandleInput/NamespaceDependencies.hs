module Unison.Codebase.Editor.HandleInput.NamespaceDependencies
  ( namespaceDependencies,
  )
where

import Control.Monad.Trans.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch0)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.HandleInput.LoopState (Action, eval)
import qualified Unison.Codebase.Editor.HandleInput.LoopState as LoopState (askCodebase)
import qualified Unison.DataDeclaration as DD
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Relation3 as Relation3
import qualified Unison.Util.Relation4 as Relation4

-- | Check the dependencies of all types, terms, and metadata in the current namespace,
-- returns a map of dependencies which do not have a name within the current namespace,
-- alongside the names of all of that thing's dependents.
--
-- This is non-transitive, i.e. only the first layer of external dependencies is returned.
--
-- So if my namespace depends on .base.Bag.map; which depends on base.Map.mapKeys, only
-- .base.Bag.map is returned unless some other definition inside my namespace depends
-- on base.Map.mapKeys directly.
--
-- Returns a Set of names rather than using the PPE since we already have the correct names in
-- scope on this branch, and also want to list ALL names of dependents, including aliases.
namespaceDependencies :: forall m i v. Ord v => Branch0 m -> Action i v (Map LabeledDependency (Set Name))
namespaceDependencies branch = do
  codebase <- LoopState.askCodebase

  typeDeps <- for (Map.toList currentBranchTypeRefs) $ \(typeRef, names) -> fmap (fromMaybe Map.empty) . runMaybeT $ do
    refId <- MaybeT . pure $ Reference.toId typeRef
    decl <- MaybeT $ eval (Eval (Codebase.getTypeDeclaration codebase refId))
    let typeDeps = Set.map LD.typeRef $ DD.dependencies (DD.asDataDecl decl)
    pure $ foldMap (`Map.singleton` names) typeDeps

  termDeps <- for (Map.toList currentBranchTermRefs) $ \(termRef, names) -> fmap (fromMaybe Map.empty) . runMaybeT $ do
    refId <- MaybeT . pure $ Referent.toReferenceId termRef
    term <- MaybeT $ eval (Eval (Codebase.getTerm codebase refId))
    let termDeps = Term.labeledDependencies term
    pure $ foldMap (`Map.singleton` names) termDeps

  let dependenciesToDependents :: Map LabeledDependency (Set Name)
      dependenciesToDependents =
        Map.unionsWith (<>) (metadata : typeDeps ++ termDeps)
  let onlyExternalDeps :: Map LabeledDependency (Set Name)
      onlyExternalDeps =
        Map.filterWithKey
          ( \x _ ->
              LD.fold
                (`Map.notMember` currentBranchTypeRefs)
                (`Map.notMember` currentBranchTermRefs)
                x
          )
          dependenciesToDependents
  pure onlyExternalDeps
  where
    currentBranchTermRefs :: Map Referent (Set Name)
    currentBranchTermRefs = Relation.domain (Branch.deepTerms branch)
    currentBranchTypeRefs :: Map Reference (Set Name)
    currentBranchTypeRefs = Relation.domain (Branch.deepTypes branch)

    -- Since metadata is only linked by reference, not by name,
    -- it's possible that the metadata itself is external to the branch.
    metadata :: Map LabeledDependency (Set Name)
    metadata =
      let typeMetadataRefs :: Map LabeledDependency (Set Name)
          typeMetadataRefs =
            (Branch.deepTypeMetadata branch)
              & Relation4.d234 -- Select only the type and value portions of the metadata
              & \rel ->
                let types = Map.mapKeys LD.typeRef $ Relation.range (Relation3.d12 rel)
                    terms = Map.mapKeys LD.termRef $ Relation.range (Relation3.d13 rel)
                 in Map.unionWith (<>) types terms
          termMetadataRefs :: Map LabeledDependency (Set Name)
          termMetadataRefs =
            (Branch.deepTermMetadata branch)
              & Relation4.d234 -- Select only the type and value portions of the metadata
              & \rel ->
                let types = Map.mapKeys LD.typeRef $ Relation.range (Relation3.d12 rel)
                    terms = Map.mapKeys LD.termRef $ Relation.range (Relation3.d13 rel)
                 in Map.unionWith (<>) types terms
       in Map.unionWith (<>) typeMetadataRefs termMetadataRefs
