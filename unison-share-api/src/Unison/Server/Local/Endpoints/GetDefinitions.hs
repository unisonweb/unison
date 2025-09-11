{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Local.Endpoints.GetDefinitions where

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
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project
import Unison.Runtime (Runtime)
import Unison.Server.Backend qualified as Backend
import Unison.Server.Local.Definitions qualified as Local
import Unison.Server.Types
  ( APIGet,
    APIHeaders,
    DefinitionDisplayResults,
    DefinitionSearchResults,
    RequiredQueryParam,
    Suffixify (..),
    defaultWidth,
    setCacheControl,
  )
import Unison.Symbol (Symbol)
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
getDefinitionDependentsEndpoint _rt codebase projectAndBranch _relativePath _hqn _width = do
  rootCausal <- Backend.resolveProjectRoot codebase projectAndBranch
  _names <- Backend.hoistBackend (Codebase.runTransaction codebase) $ do
    _names <- lift $ Codebase.namesAtPath (Causal.valueHash rootCausal) (Path.fromList [])
    pure ()
  pure $ setCacheControl undefined

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
