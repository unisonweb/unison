module Unison.Codebase.Editor.HandleInput.NamespaceDependencies
  ( namespaceDependencies
  ) where
import Unison.Reference (Reference)
import Unison.Type (Type)
import Unison.Parser.Ann (Ann (..))
import Unison.Referent (Referent)
import Unison.Codebase.Branch (Branch0)
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Unison.Util.Relation as Relation
import qualified Unison.Referent as Referent
import Unison.Codebase.Editor.HandleInput.Action (Action)
import Unison.LabeledDependency (LabeledDependency)
import Unison.Util.Free


-- data ReferenceType v
--   = TypeRef Reference
--   | TermRef Reference (Type v Ann)
--   | ConstructorRef Referent
--   deriving (Eq, Ord)

-- | Check the dependencies of all types, terms, and metadata in the current namespace,
-- returns dependencies which do not have a name within the current namespace, alongside their
-- type (for terms and constructors).
--
-- This is non-transitive, i.e. only the first layer of external dependencies is returned.
--
-- So if my namespace depends on .base.Bag.map; which depends on base.Map.mapKeys, only
-- .base.Bag.map is returned unless some other definition inside my namespace depends
-- on .base.Bag.map directly.
namespaceDependencies :: forall m i v. Branch0 m -> Action m i v (Map LabeledDependency (Set Reference))
namespaceDependencies branch = do
  dependenciesToDependants :: Map Reference (Set Reference)
    <- fold <$> traverse (eval . GetDependencies) (mapMaybe Reference.toId $ Set.toList refsInCurrentBranch)
  externalConstructors  :: Map Reference (Set Referent)
    <- Map.unions <$> (for (Set.toList dependenciesToDependants) $ \ref -> do
                    constructors <- eval (ConstructorsOfType ref)
                    let externalConstrs = constructors `Set.difference` currentBranchReferents
                    pure $ Map.singleton ref externalConstrs
              )
  let externalDependenciesToDependants :: Map Reference (Set Reference)
      externalDependenciesToDependants = Map.filterWithKey (\k _ -> k `Set.notMember` refsInCurrentBranch) dependenciesToDependants
  termsTypesAndConstructors :: (Map Reference (Type v Ann), Map Reference (Set Referent))
       <- fold <$> for (Set.toList externalDependencies) \ref -> do
            typeOfTerm <- eval (LoadTypeOfTerm ref)
            pure $ case typeOfTerm of
              -- If we got a type, this must be a term reference
              Just typ -> (Map.singleton ref typ, mempty)
              -- If we didn't, it must be a type declaration, so we include its constructors.
              Nothing -> (mempty, Map.singleton ref (fold $ Map.lookup ref externalConstructors))
  pure termsTypesAndConstructors
  where
    currentBranchReferents :: Set Referent
    currentBranchReferents = Relation.dom (deepTerms branch)
    refsInCurrentBranch :: Set Reference
    refsInCurrentBranch =
         Relation.dom (deepTypes branch)
      <> Set.map Referent.toReference currentBranchReferents
      -- TODO:
      -- <> _termMetadata
      -- <> _typeMetadata
