module Unison.Codebase.Editor.HandleInput.DebugDependentsGraph
  ( handleDebugDependentsGraph,
  )
where

import Algebra.Graph.AdjacencyMap qualified as Graph
import Data.List qualified as List
import U.Codebase.Sqlite.Operations qualified as Operations
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
import Unison.Reference (Reference, TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.NamePrinter (prettyHashQualified)
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set

handleDebugDependentsGraph :: Cli ()
handleDebugDependentsGraph = do
  currentNamespace <- Cli.getCurrentBranch0

  let currentNamespaceSansLib =
        Branch.deleteLibdeps currentNamespace

  let refToDefn :: Reference -> Maybe (Defn TermReference TypeReference)
      refToDefn =
        let defns = Branch.deepDefns currentNamespace
         in \ref ->
              if Relation.memberDom (Referent.fromTermReference ref) defns.terms
                then Just (TermDefn ref)
                else
                  if Relation.memberDom ref defns.types
                    then Just (TypeDefn ref)
                    else Nothing

  let refIdToDefn :: Reference.Id -> Maybe (Defn TermReference TypeReference)
      refIdToDefn =
        refToDefn . Reference.DerivedId

  let scope :: DefnsF Set TermReference TypeReference
      scope =
        bimap (Set.mapMaybe Referent.toTermReference . Relation.dom) Relation.dom (Branch.deepDefns currentNamespaceSansLib)

  let query :: DefnsF Set TermReferenceId TypeReferenceId
      query =
        bimap (Set.mapMaybe Reference.toId) (Set.mapMaybe Reference.toId) scope

  edges <-
    Cli.runTransaction (Operations.transitiveDependentsGraphWithinScope query scope)

  let graph =
        List.foldl
          ( \acc (dependency, dependent) ->
              case (refIdToDefn dependent, refToDefn dependency) of
                (Just source, Just target) -> Graph.overlay acc (Graph.edge source target)
                _ -> acc
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

  liftIO (putStrLn (Pretty.toANSI 80 (Pretty.syntaxToColor output)))
