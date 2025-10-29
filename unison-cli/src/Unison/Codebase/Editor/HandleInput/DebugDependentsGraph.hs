module Unison.Codebase.Editor.HandleInput.DebugDependentsGraph
  ( handleDebugDependentsGraph,
  )
where

import Algebra.Graph.AdjacencyMap qualified as Graph
import Data.List qualified as List
import Data.Text.IO qualified as Text
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Builtin qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.NamePrinter (prettyHashQualified)
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (DefnsF)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set

handleDebugDependentsGraph :: Cli ()
handleDebugDependentsGraph = do
  currentNamespace <- Cli.getCurrentBranch0

  let currentNamespaceSansLib =
        Branch.deleteLibdeps currentNamespace

  let query :: DefnsF Set TermReference TypeReference
      query =
        bimap (Set.mapMaybe Referent.toTermReference . Relation.dom) Relation.dom (Branch.deepDefns currentNamespaceSansLib)

  let scope :: DefnsF Set TermReferenceId TypeReferenceId
      scope =
        bimap (Set.mapMaybe Reference.toId) (Set.mapMaybe Reference.toId) query

  edges <-
    Cli.runTransaction (Operations.transitiveDependentsGraphWithinScope Builtin.isBuiltinType scope query)

  let graph :: Graph.AdjacencyMap (Defn TermReference TypeReference)
      graph =
        List.foldl
          ( \acc edge ->
              Graph.overlay acc case edge of
                Operations.TermDependsOnTerm dependent dependency ->
                  Graph.edge (TermDefn (Reference.fromId dependent)) (TermDefn dependency)
                Operations.TermDependsOnType dependent dependency ->
                  Graph.edge (TermDefn (Reference.fromId dependent)) (TypeDefn dependency)
                Operations.TypeDependsOnType dependent dependency ->
                  Graph.edge (TypeDefn (Reference.fromId dependent)) (TypeDefn dependency)
          )
          Graph.empty
          edges

  let adjacency =
        Graph.adjacencyList graph

  let ppe =
        (Branch.toPrettyPrintEnvDecl 10 currentNamespace).suffixifiedPPE

  let prettyDefn :: Defn TermReference TypeReference -> HashQualified Name
      prettyDefn = \case
        TermDefn ref -> PPE.termName ppe (Referent.fromTermReference ref)
        TypeDefn ref -> PPE.typeName ppe ref

  let output =
        Pretty.lines
          ( map
              ( \(dependent, dependencies) ->
                  prettyHashQualified (prettyDefn dependent)
                    <> " depends on: "
                    <> Pretty.commas (map (prettyHashQualified . prettyDefn) dependencies)
              )
              adjacency
          )

  liftIO (Text.putStrLn (Pretty.toANSI 80 (Pretty.syntaxToColor output)))
