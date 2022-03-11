module Unison.Codebase.Editor.HandleInput.DependencyGraph
  ( dependencyGraph,
    renderDottyGraph,
  )
where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import qualified Data.Set as Set
import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.HandleInput.LoopState (Action, eval)
import qualified Unison.DataDeclaration as DD
import qualified Unison.HashQualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Prelude
import Unison.PrettyPrintEnv
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import qualified Unison.Util.Map as Map

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
dependencyGraph :: forall m i v. (Ord v, Monad m) => Set LabeledDependency -> Action m i v Graph
dependencyGraph srcs = do
  depGraph <- execStateT (for_ srcs collectDependencies) mempty
  pure (srcs, depGraph)

type DependencyGraph =
  (Map LabeledDependency (Set LabeledDependency))

type Graph =
  (Set LabeledDependency, Map LabeledDependency (Set LabeledDependency))

collectDependencies :: forall m i v. (Monad m, Ord v) => LabeledDependency -> StateT DependencyGraph (Action m i v) ()
collectDependencies src = do
  whenM (gets (hasn't (ix src))) $ do
    case src of
      LD.TypeReference typeRef -> typeDeps typeRef
      LD.TermReferent termRef -> termDeps termRef
  where
    typeDeps :: Reference -> StateT DependencyGraph (Action m i v) ()
    typeDeps typeRef = fmap (fromMaybe ()) . runMaybeT $ do
      refId <- MaybeT . pure $ Reference.toId typeRef
      decl <- MaybeT $ eval (LoadType refId)
      let typeDeps = Set.map LD.typeRef $ DD.dependencies (DD.asDataDecl decl)
      (at (LD.TypeReference typeRef) . non Empty) <>= typeDeps
      lift $ for_ typeDeps collectDependencies

    termDeps :: Referent -> StateT DependencyGraph (Action m i v) ()
    termDeps termRef = fmap (fromMaybe ()) . runMaybeT $ do
      refId <- MaybeT . pure $ Referent.toReferenceId termRef
      term <- MaybeT $ eval (LoadTerm refId)
      let termDeps = Term.labeledDependencies term
      (at (LD.TermReferent termRef) . non Empty) <>= termDeps
      lift $ for_ termDeps collectDependencies

renderDottyGraph :: PrettyPrintEnv -> Graph -> Text
renderDottyGraph ppe (origins, graph) = execWriter $ do
  line "digraph deps {"
  line "  fontname=\"Helvetica,Arial,sans-serif\""
  line "  node [fontname=\"Helvetica,Arial,sans-serif\"]"
  line "  edge [fontname=\"Helvetica,Arial,sans-serif\"]"
  line "  node [shape=box];"
  -- Pre-declaring nodes ensures they're at the top of the graph.
  for_ (Set.map renderLabeled origins) (\s -> line $ "  " <> quoted s)
  ifor prettyDepGraph $ \src dests -> do
    for_ dests $ \dest -> do
      line $ "  " <> quoted src <> " -> " <> quoted dest

  line "}"
  where
    renderLabeled = HQ.toText . labeledRefName ppe
    prettyDepGraph :: (Map Text (Set Text))
    prettyDepGraph = Map.bimap renderLabeled (Set.map renderLabeled) graph
    line :: Text -> Writer Text ()
    line s = tell (s <> "\n")
    quoted s = "\"" <> s <> "\""
