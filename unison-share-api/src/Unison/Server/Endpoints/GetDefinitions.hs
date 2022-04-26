{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.GetDefinitions where

import Control.Monad.Except
import qualified Data.Text as Text
import Servant
  ( QueryParam,
    QueryParams,
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (..),
    ToParam (..),
    ToSample (..),
    noSamples,
  )
import Unison.Codebase (Codebase)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import qualified Unison.Codebase.Runtime as Rt
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash,
  )
import qualified Unison.HashQualified as HQ
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import Unison.Server.Types
  ( APIGet,
    DefinitionDisplayResults,
    HashQualifiedName,
    NamespaceFQN,
    Suffixify (..),
    defaultWidth,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type DefinitionsAPI =
  "getDefinition" :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" NamespaceFQN
    :> QueryParams "names" HashQualifiedName
    :> QueryParam "renderWidth" Width
    :> QueryParam "suffixifyBindings" Suffixify
    :> APIGet DefinitionDisplayResults

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

instance ToParam (QueryParam "relativeTo" NamespaceFQN) where
  toParam _ =
    DocQueryParam
      "relativeTo"
      [".", ".base", "foo.bar"]
      ( "The namespace relative to which names will be resolved and displayed. "
          <> "If left absent, the root namespace will be used."
      )
      Normal

instance ToParam (QueryParam "rootBranch" ShortBranchHash) where
  toParam _ =
    DocQueryParam
      "rootBranch"
      ["#abc123"]
      ( "The hash or hash prefix of the namespace root. "
          <> "If left absent, the most recent root will be used."
      )
      Normal

instance ToParam (QueryParams "names" Text) where
  toParam _ =
    DocQueryParam
      "names"
      [".base.List", "foo.bar", "#abc123"]
      ("A fully qualified name, hash-qualified name, " <> "or hash.")
      List

instance ToSample DefinitionDisplayResults where
  toSamples _ = noSamples

serveDefinitions ::
  MonadIO m =>
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Maybe ShortBranchHash ->
  Maybe NamespaceFQN ->
  [HashQualifiedName] ->
  Maybe Width ->
  Maybe Suffixify ->
  Backend.Backend m DefinitionDisplayResults
serveDefinitions rt codebase mayRoot relativePath rawHqns width suff =
  do
    rel <-
      fmap Path.fromPath' <$> traverse (parsePath . Text.unpack) relativePath
    ea <- liftIO . runExceptT $ do
      root <- traverse (Backend.expandShortBranchHash codebase) mayRoot
      let hqns = HQ.unsafeFromText <$> rawHqns
          scope = case hqns of
            -- TODO: Change this API to support being queried by just 1 name/hash
            HQ.HashOnly _ : _ -> Backend.AllNames
            _ -> Backend.Scoped

      Backend.prettyDefinitionsBySuffixes
        (fromMaybe Path.empty rel)
        scope
        root
        width
        (fromMaybe (Suffixify True) suff)
        rt
        codebase
        hqns
    liftEither ea
  where
    parsePath p = errFromEither (`Backend.BadNamespace` p) $ Path.parsePath' p
    errFromEither f = either (throwError . f) pure
