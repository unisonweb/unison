{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-top-binds -fno-warn-unused-local-binds #-}

module Unison.Codebase.Editor.HandleInput.DependencyGraph
  ( dependencyGraph,
    renderDottyMarkup,
    renderDottyPng,
  )
where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.IO (openTempFile)
import System.IO.Temp (emptySystemTempFile, writeSystemTempFile)
import System.Process.Extra (runProcess)
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
import qualified Unison.Util.List as List
import qualified Unison.Util.Map as Map
import qualified UnliftIO
import UnliftIO.Process (callProcess, readProcess)
import qualified UnliftIO.Process as Process

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
dependencyGraph :: forall m i v. (Ord v, Monad m) => EdgeFilter -> Set LabeledDependency -> Action m i v Graph
dependencyGraph edgeFilter srcs = do
  depGraph <- execStateT (for_ srcs (collectDependencies edgeFilter)) mempty
  pure (srcs, depGraph)

type DependencyGraph =
  (Map LabeledDependency (Set LabeledDependency))

type Graph =
  (Set LabeledDependency, Map LabeledDependency (Set LabeledDependency))

type EdgeFilter = (LabeledDependency, LabeledDependency) -> Bool

collectDependencies ::
  forall m i v.
  (Monad m, Ord v) =>
  EdgeFilter ->
  LabeledDependency ->
  StateT DependencyGraph (Action m i v) ()
collectDependencies edgeFilter src = do
  alreadyProcessed <- gets (has (ix src))
  when (not alreadyProcessed) $ do
    case src of
      LD.TypeReference typeRef -> typeDeps typeRef
      LD.TermReferent termRef -> termDeps termRef
  where
    typeDeps :: Reference -> StateT DependencyGraph (Action m i v) ()
    typeDeps typeRef = fmap (fromMaybe ()) . runMaybeT $ do
      refId <- MaybeT . pure $ Reference.toId typeRef
      decl <- MaybeT $ eval (LoadType refId)
      let typeDeps =
            DD.dependencies (DD.asDataDecl decl)
              & Set.map LD.typeRef
              & Set.filter (\ref -> edgeFilter (LD.TypeReference typeRef, ref))
      (at (LD.TypeReference typeRef) . non Empty) <>= typeDeps
      lift $ for_ typeDeps (collectDependencies edgeFilter)

    termDeps :: Referent -> StateT DependencyGraph (Action m i v) ()
    -- Do we care about tracking deps on constructors or just on types?
    termDeps termRef = fmap (fromMaybe ()) . runMaybeT $ do
      refId <- MaybeT . pure $ Referent.toReferenceId termRef
      term <- MaybeT $ eval (LoadTerm refId)
      let termDeps =
            Term.labeledDependencies term
              & Set.filter (\ref -> edgeFilter (LD.TermReferent termRef, ref))
      (at (LD.TermReferent termRef) . non Empty) <>= termDeps
      lift $ for_ termDeps (collectDependencies edgeFilter)

renderDottyMarkup :: PrettyPrintEnv -> Graph -> Text
renderDottyMarkup ppe (_origins, graph) = execWriter $ do
  line "digraph deps {"
  line "  ratio=1"
  line "  fontname=\"Helvetica,Arial,sans-serif\""
  line "  node [fontname=\"Helvetica,Arial,sans-serif\"]"
  line "  edge [fontname=\"Helvetica,Arial,sans-serif\"]"
  -- line "  node [shape=box];"
  -- We declare the origin nodes first so they'll appear prominently at the top of the graph.
  -- for_ origins declareNode
  -- for_ ((Map.keysSet graph <> fold graph) `Set.difference` origins) declareNode
  ifor_ (groupByNamespace $ Map.keysSet graph <> fold graph) declareNamespace
  ifor prettyDepGraph $ \src dests -> do
    for_ dests $ \dest -> do
      line $ "  " <> quoted src <> " -> " <> quoted dest
  line "}"
  where
    declareNamespace :: Text -> [LabeledDependency] -> Writer Text ()
    declareNamespace ns deps = do
      line $ "  subgraph " <> quoted ("cluster_" <> ns) <> " {"
      line $ "  label=" <> quoted ns <> ";"
      for_ deps declareNode
      line "  };"

    declareNode :: LabeledDependency -> Writer Text ()
    declareNode ref =
      let s = renderLabeled ref
       in case ref of
            LD.TypeReference {} -> line $ "  " <> quoted s <> typeNode
            LD.TermReference {} -> line $ "  " <> quoted s <> termNode
            LD.ConReference {} -> line $ "  " <> quoted s <> constrNode
    typeNode, termNode, constrNode :: Text
    typeNode = " [shape=ellipse, color=red]"
    termNode = " [shape=box, color=green]"
    constrNode = " [shape=box, style=rounded, color=orange]"
    renderLabeled :: LabeledDependency -> Text
    renderLabeled = HQ.toText . labeledRefName ppe

    prettyDepGraph :: (Map Text (Set Text))
    prettyDepGraph = Map.bimap renderLabeled (Set.map renderLabeled) graph

    line :: Text -> Writer Text ()
    line s = tell (s <> "\n")
    quoted :: Text -> Text
    quoted s = "\"" <> s <> "\""

    getNamespace :: Text -> Text
    getNamespace = Text.intercalate "." . init . Text.splitOn "."

    groupByNamespace :: Set LabeledDependency -> Map Text [LabeledDependency]
    groupByNamespace =
      List.groupBy (getNamespace . renderLabeled)

renderDottyPng :: Text -> IO ()
renderDottyPng input = do
  pngFile <- emptySystemTempFile "graph.png"
  let dotty = Process.shell $ "unflatten -l 3 | dot -Tpng -o " <> pngFile
  void $ Process.readCreateProcess dotty (Text.unpack input)
  -- void $ Process.readProcess "dot" ["-Tpng", "-o", pngFile] (Text.unpack input)
  -- pngFile <- writeSystemTempFile "graph" pngOut
  -- Add mac/linux support
  callProcess "open" [pngFile]
  putStrLn $ "Opening " <> pngFile
