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
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as Relation
import Unison.Name (Name)

-- | Check the dependencies of all types, terms, and metadata in the current namespace,
-- returns dependencies which do not have a name within the current namespace, alongside their
-- type (for terms and constructors).
--
-- This is non-transitive, i.e. only the first layer of external dependencies is returned.
--
-- So if my namespace depends on .base.Bag.map; which depends on base.Map.mapKeys, only
-- .base.Bag.map is returned unless some other definition inside my namespace depends
-- on .base.Bag.map directly.
namespaceDependencies :: forall m i v. Branch0 m -> Action m i v (Map LabeledDependency (Set Name))
namespaceDependencies branch = do
  let refToDeps :: Reference.Id -> Action m i v (Map Reference (Set Name))
      refToDeps refId = do
        dependencies <- Set.toList <$> eval (GetDependencies refId)
        (dependencies
          & fmap \dep -> Map.singleton dep (localNameByReference (Reference.fromId refId)))
          & Map.unionsWith (<>)
          & pure
  let allDependenciesOf :: Set Reference -> Action m i v (Map Reference (Set Name))
      allDependenciesOf refs = refs
                             & Set.toList
                             & mapMaybe Reference.toId
                             & traverse refToDeps
                             <&> Map.unionsWith (<>)
  dependenciesToDependants :: Map LabeledDependency (Set Name) <-
    Map.unionsWith (<>)
      <$> sequenceA
        [ Map.mapKeys LD.typeRef <$> allDependenciesOf currentBranchTypes,
          Map.mapKeys LD.termRef <$> allDependenciesOf currentBranchTerms
        ]
  let onlyExternalDeps :: Map LabeledDependency (Set Name)
      onlyExternalDeps =
        Map.filterWithKey
          (\k _ -> either id id (LD.toReference k) `Set.notMember` refsInCurrentBranch)
          dependenciesToDependants
  externalConstructors :: Map LabeledDependency (Set Name) <-
    Map.unions . concat
      <$> ( for (Map.toList onlyExternalDeps) $ \case
              (LD.toReference -> Left typeRef, deps) -> do
                constructors <- eval (ConstructorsOfType typeRef)
                let externalConstrs = constructors `Set.difference` currentBranchReferents
                pure $
                  [ Map.singleton (LD.typeRef typeRef) deps,
                    Map.fromListWith (<>) ((,deps) . LD.referent <$> Set.toList externalConstrs)
                  ]
              _ -> pure []
          )
  let allDependenciesToDependants = Map.unionWith (<>) externalConstructors onlyExternalDeps
  pure allDependenciesToDependants
  where
    -- termsTypesAndConstructors :: (Map Reference (Type v Ann), Map Reference (Set Referent))
    --      <- _ <$> for (Set.toList externalDependencies) \ref -> do
    --           typeOfTerm <- eval (LoadTypeOfTerm ref)
    --           pure $ case typeOfTerm of
    --             -- If we got a type, this must be a term reference
    --             Just typ -> (Map.singleton ref typ, mempty)
    --             -- If we didn't, it must be a type declaration, so we include its constructors.
    --             Nothing -> (mempty, Map.singleton ref (_ $ Map.lookup ref externalConstructors))
    -- pure termsTypesAndConstructors

    currentBranchReferents :: Set Referent
    currentBranchReferents = Relation.dom (Branch.deepTerms branch)
    localNameByReference :: Reference -> (Set Name)
    localNameByReference ref = Relation.lookupDom (Referent.fromReference ref) (Branch.deepTerms branch)
    currentBranchTerms :: Set Reference
    currentBranchTerms = Set.map Referent.toReference currentBranchReferents
    currentBranchTypes :: Set Reference
    currentBranchTypes = Relation.dom (Branch.deepTypes branch)
    refsInCurrentBranch :: Set Reference
    refsInCurrentBranch =
      Relation.dom (Branch.deepTypes branch)
        <> Set.map Referent.toReference currentBranchReferents

-- TODO:
-- <> _termMetadata
-- <> _typeMetadata
