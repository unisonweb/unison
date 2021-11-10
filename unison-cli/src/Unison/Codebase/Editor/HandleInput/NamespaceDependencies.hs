module Unison.Codebase.Editor.HandleInput.NamespaceDependencies
  ( namespaceDependencies,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.Codebase.Branch (Branch0)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.HandleInput.Action (Action, eval)
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as Relation

-- | Check the dependencies of all types, terms, and metadata in the current namespace,
-- returns a map of dependencies which do not have a name within the current namespace,
-- alongside the names of all of that thing's dependants.
--
-- This is non-transitive, i.e. only the first layer of external dependencies is returned.
--
-- So if my namespace depends on .base.Bag.map; which depends on base.Map.mapKeys, only
-- .base.Bag.map is returned unless some other definition inside my namespace depends
-- on .base.Bag.map directly.
namespaceDependencies :: forall m i v. Branch0 m -> Action m i v (Map Referent (Set Name))
namespaceDependencies branch = do
  let refToDeps :: Reference.Id -> Action m i v (Map Referent (Set Name))
      refToDeps refId = do
        dependencies <- fmap Referent.fromReference . Set.toList <$> eval (GetDependencies refId)
        ( dependencies
            & fmap \dep -> Map.singleton dep (localNameByReference (Reference.fromId refId))
          )
          & Map.unionsWith (<>)
          & pure
  let allDependenciesOf :: Set Reference -> Action m i v (Map Referent (Set Name))
      allDependenciesOf refs =
        refs
          & Set.toList
          & mapMaybe Reference.toId
          & traverse refToDeps
          <&> Map.unionsWith (<>)
  dependenciesToDependants :: Map Referent (Set Name) <-
    Map.unionsWith (<>)
      <$> sequenceA
        [ allDependenciesOf currentBranchTypes,
          allDependenciesOf currentBranchTerms
        ]
  let onlyExternalDeps :: Map Referent (Set Name)
      onlyExternalDeps =
        Map.filterWithKey
          (\k _ -> k `Set.notMember` typeAndTermRefsInCurrentBranch)
          dependenciesToDependants
  externalConstructors :: Map Referent (Set Name) <-
    Map.unions . concat
      <$> ( for (Map.toList onlyExternalDeps) $ \case
              (ref, deps) -> do
                constructors <- eval (ConstructorsOfType (Referent.toReference ref))
                let externalConstrs = constructors `Set.difference` currentBranchReferents
                pure $
                  [ Map.singleton ref deps,
                    Map.fromListWith (<>) ((,deps) <$> Set.toList externalConstrs)
                  ]
          )
  let allDependenciesToDependants = Map.unionWith (<>) externalConstructors onlyExternalDeps
  pure allDependenciesToDependants
  where
    currentBranchReferents :: Set Referent
    currentBranchReferents = Relation.dom (Branch.deepTerms branch)
    localNameByReference :: Reference -> (Set Name)
    localNameByReference ref = Relation.lookupDom (Referent.fromReference ref) (Branch.deepTerms branch)
    currentBranchTerms :: Set Reference
    currentBranchTerms = Set.map Referent.toReference currentBranchReferents
    currentBranchTypes :: Set Reference
    currentBranchTypes = Relation.dom (Branch.deepTypes branch)
    typeAndTermRefsInCurrentBranch :: Set Referent
    typeAndTermRefsInCurrentBranch =
      Set.map Referent.fromReference (Relation.dom (Branch.deepTypes branch))
        <> currentBranchReferents
-- TODO:
-- <> _termMetadata
-- <> _typeMetadata
