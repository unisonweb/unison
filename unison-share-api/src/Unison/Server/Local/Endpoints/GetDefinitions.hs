{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Local.Endpoints.GetDefinitions where

import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Set qualified as Set
import Servant
  ( QueryParam,
    QueryParams,
    ServerT,
    (:<|>) (..),
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (..),
    ToParam (..),
    ToSample (..),
    noSamples,
  )
import U.Codebase.Causal qualified as Causal
import U.Codebase.Reference (TermReferenceId)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NamesWithHistory (SearchType (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime (Runtime)
import Unison.Server.Backend qualified as Backend
import Unison.Server.Local.Definitions qualified as Local
import Unison.Server.NameSearch.FromNames (makeNameSearch)
import Unison.Server.QueryResult (QueryResult (..))
import Unison.Server.SearchResult (SearchResult (..), TermResult (..), TypeResult (..))
import Unison.Server.Types
  ( APIGet,
    APIHeaders,
    DefinitionDisplayResults,
    DefinitionSearchResult (..),
    DefinitionSearchResults (..),
    RequiredQueryParam,
    Suffixify (..),
    TermOrTypeSummary (..),
    defaultWidth,
    setCacheControl,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Defns (Defns (..))
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty (Width)

type DefinitionsAPI =
  ("getDefinition" :> GetDefinitionEndpoint)
    :<|> ("getDefinitionDependents" :> GetDefinitionDependentsEndpoint)

-- More endpoints could go here in the future

type GetDefinitionEndpoint =
  QueryParam "relativeTo" Path.Path
    :> QueryParams "names" (HQ.HashQualified Name)
    :> QueryParam "renderWidth" Width
    :> QueryParam "suffixifyBindings" Suffixify
    :> APIGet DefinitionDisplayResults

type GetDefinitionDependentsEndpoint =
  QueryParam "relativeTo" Path.Path
    :> RequiredQueryParam "name" (HQ.HashQualified Name)
    :> QueryParam "renderWidth" Width
    :> APIGet DefinitionSearchResults

instance ToParam (QueryParam "renderWidth" Width) where
  toParam _ =
    DocQueryParam
      "renderWidth"
      ["80", "100", "120"]
      ( "The preferred maximum line width (in characters) of the source code of "
          <> "definitions to be rendered. "
          <> "If left absent, the render width is assumed to be "
          <> show defaultWidth
          <> "."
      )
      Normal

instance ToParam (QueryParam "suffixifyBindings" Suffixify) where
  toParam _ =
    DocQueryParam
      "suffixifyBindings"
      ["True", "False"]
      ( "If True or absent, renders definitions using the shortest unambiguous "
          <> "suffix. If False, uses the fully qualified name. "
      )
      Normal

instance ToParam (QueryParam "relativeTo" Path.Path) where
  toParam _ =
    DocQueryParam
      "relativeTo"
      []
      ( "The namespace relative to which names will be resolved and displayed. "
          <> "If left absent, the root namespace will be used."
          <> "E.g. base.List"
      )
      Normal

instance ToParam (QueryParam "namespace" Path.Path) where
  toParam _ =
    DocQueryParam
      "namespace"
      []
      ( "The namespace required by the endpoint."
          <> "If left absent, the relativeTo namespace will be used."
          <> "E.g. base.List"
      )
      Normal

instance ToParam (QueryParams "names" (HQ.HashQualified Name)) where
  toParam _ =
    DocQueryParam
      "names"
      [".base.List", "foo.bar", "@abc123"]
      ("A fully qualified name, hash-qualified name, " <> "or hash.")
      List

instance ToSample DefinitionDisplayResults where
  toSamples _ = noSamples

getDefinitionDependentsEndpoint ::
  Runtime Symbol ->
  Codebase IO Symbol Ann ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Maybe Path.Path ->
  HQ.HashQualified Name ->
  Maybe Width ->
  Backend.Backend IO (APIHeaders DefinitionSearchResults)
getDefinitionDependentsEndpoint _rt codebase projectAndBranch _relativePath hqn mayWidth = do
  hqLength <- liftIO $ Codebase.runTransaction codebase $ Codebase.hashLength
  rootCausal <- Backend.resolveProjectRoot codebase projectAndBranch
  (dependents, names) <- Backend.hoistBackend (Codebase.runTransaction codebase) $ do
    names <- lift $ Codebase.namesAtPath (Causal.valueHash rootCausal) (Path.fromList [])
    branch0 <- Branch.head <$> lift (Codebase.expectBranchForHashTx codebase (Causal.causalHash rootCausal))
    let nameSearch = makeNameSearch hqLength names
    QueryResult {hits} <- lift $ Backend.hqNameQuery codebase nameSearch ExactName [hqn]
    let defs =
          hits & foldMap \case
            Tp TypeResult {reference} -> Defns {terms = Set.empty, types = (Set.singleton reference)}
            Tm TermResult {referent} -> Defns {terms = (Set.singleton referent), types = Set.empty}

    dependents <- lift $ Codebase.dependentsWithinBranchScope branch0 defs
    pure (dependents, names)
  let pped = PPED.makePPED (PPE.hqNamer 10 names) PPE.dontSuffixify
  definitionSearchResults <-
    dependents
      & bitraverse (wither (doTerm pped) . Set.toList) (wither (doType pped) . Set.toList)
  definitionSearchResults
    & bifold
    & DefinitionSearchResults
    & setCacheControl
    & pure
  where
    project = projectAndBranch.project
    branchRef = projectAndBranch.branch
    doTerm :: PPED.PrettyPrintEnvDecl -> TermReferenceId -> Backend.Backend IO (Maybe DefinitionSearchResult)
    doTerm pped refId = runMaybeT do
      let referent = Referent.fromTermReferenceId refId
      fqn <- hoistMaybe $ HQ.toName $ PPE.termName (PPED.unsuffixifiedPPE pped) referent
      summary <- lift $ Backend.termSummaryForReferent codebase referent Nothing (\_ -> pure pped) mayWidth
      pure $
        DefinitionSearchResult
          { fqn,
            summary = ToTTermSummary summary,
            project,
            branchRef
          }

    doType pped refId = do
      runMaybeT do
        let reference = Reference.fromId refId
        fqn <- hoistMaybe $ HQ.toName $ PPE.typeName (PPED.unsuffixifiedPPE pped) reference
        summary <- lift $ Backend.typeSummaryForReference codebase reference Nothing (\_ -> pure pped) mayWidth
        pure $
          DefinitionSearchResult
            { fqn,
              summary = ToTTypeSummary summary,
              project,
              branchRef
            }

getDefinitionsEndpoint ::
  Runtime Symbol ->
  Codebase IO Symbol Ann ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  Maybe Path.Path ->
  [HQ.HashQualified Name] ->
  Maybe Width ->
  Maybe Suffixify ->
  Backend.Backend IO (APIHeaders DefinitionDisplayResults)
getDefinitionsEndpoint rt codebase projectAndBranchName relativePath hqns width suff = do
  root <- Backend.resolveProjectRoot codebase projectAndBranchName
  r <-
    foldMapM
      ( Local.prettyDefinitionsForHQName
          (maybe Path.Root Path.Absolute relativePath)
          root
          width
          (fromMaybe (Suffixify True) suff)
          rt
          codebase
      )
      hqns
  pure $ setCacheControl r

serveDefinitionsServer ::
  Runtime Symbol ->
  Codebase IO Symbol Ann ->
  ProjectAndBranch ProjectName ProjectBranchName ->
  ServerT DefinitionsAPI (Backend.Backend IO)
serveDefinitionsServer rt codebase projectAndBranch = do
  getDefinitionsEndpoint rt codebase projectAndBranch
    :<|> getDefinitionDependentsEndpoint rt codebase projectAndBranch
