{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.GetDefinitions where

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
import U.Util.Timing
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash,
  )
import qualified Unison.HashQualified as HQ
import Unison.Prelude
import Unison.Server.AppState (AppM, doBackend, tryAuth)
import qualified Unison.Server.Backend as Backend
import Unison.Server.Errors
  ( badNamespace,
    errFromEither,
  )
import Unison.Server.Types
  ( APIGet,
    APIHeaders,
    DefinitionDisplayResults,
    HashQualifiedName,
    Suffixify (..),
    addHeaders,
    defaultWidth,
  )
import Unison.Util.Pretty (Width)
import Unison.Var (Var)

type DefinitionsAPI =
  "getDefinition" :> QueryParam "rootBranch" ShortBranchHash
                  :> QueryParam "relativeTo" HashQualifiedName
                  :> QueryParams "names" HashQualifiedName
                  :> QueryParam "renderWidth" Width
                  :> QueryParam "suffixifyBindings" Suffixify
  :> APIGet DefinitionDisplayResults

instance ToParam (QueryParam "renderWidth" Width) where
  toParam _ = DocQueryParam
    "renderWidth"
    ["80", "100", "120"]
    (  "The preferred maximum line width (in characters) of the source code of "
    <> "definitions to be rendered. "
    <> "If left absent, the render width is assumed to be "
    <> show defaultWidth
    <> "."
    )
    Normal

instance ToParam (QueryParam "suffixifyBindings" Suffixify) where
  toParam _ = DocQueryParam
    "suffixifyBindings"
    ["True", "False"]
    (  "If True or absent, renders definitions using the shortest unambiguous "
    <> "suffix. If False, uses the fully qualified name. "
    )
    Normal


instance ToParam (QueryParam "relativeTo" HashQualifiedName) where
  toParam _ = DocQueryParam
    "relativeTo"
    [".", ".base", "foo.bar"]
    ("The namespace relative to which names will be resolved and displayed. "
    <> "If left absent, the root namespace will be used."
    )
    Normal

instance ToParam (QueryParam "rootBranch" ShortBranchHash) where
  toParam _ = DocQueryParam
    "rootBranch"
    ["#abc123"]
    (  "The hash or hash prefix of the namespace root. "
    <> "If left absent, the most recent root will be used."
    )
    Normal

instance ToParam (QueryParams "names" Text) where
  toParam _ = DocQueryParam
    "names"
    [".base.List", "foo.bar", "#abc123"]
    ("A fully qualified name, hash-qualified name, " <> "or hash.")
    List

instance ToSample DefinitionDisplayResults where
  toSamples _ = noSamples

serveDefinitions
  :: Var v
  => Maybe ShortBranchHash
  -> Maybe HashQualifiedName
  -> [HashQualifiedName]
  -> Maybe Width
  -> Maybe Suffixify
  -> AppM v (APIHeaders DefinitionDisplayResults)
serveDefinitions mayRoot relativePath hqns width suff =
  time "serveDefinitions" $ addHeaders <$> do
    time "authenticating" tryAuth
    rel <-
      fmap Path.fromPath' <$> traverse (parsePath . Text.unpack) relativePath
    doBackend $ do
      root <- traverse
        (time "expandShortBranchHash" . Backend.expandShortBranchHash)
        mayRoot
      time "prettyDefinitions"
        $   Backend.prettyDefinitionsBySuffixes
              rel
              root
              width
              (fromMaybe (Suffixify True) suff)
        $   HQ.unsafeFromText
        <$> hqns
  where parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
