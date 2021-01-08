{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.GetDefinitions where

import           Control.Error                  ( runExceptT )
import qualified Data.Text                     as Text
import           Servant                        ( Get
                                                , JSON
                                                , QueryParam
                                                , QueryParams
                                                , throwError
                                                , (:>)
                                                )
import           Servant.Docs                   ( DocQueryParam(..)
                                                , ParamKind(..)
                                                , ToParam(..)
                                                , ToSample(..)
                                                , noSamples
                                                )
import           Servant.Server                 ( Handler )
import qualified Unison.Codebase.Path          as Path
import qualified Unison.HashQualified          as HQ
import           Unison.Parser                  ( Ann )
import qualified Unison.Server.Backend         as Backend
import           Unison.Server.Types            ( HashQualifiedName
                                                , DefinitionDisplayResults
                                                , defaultWidth
                                                )
import           Unison.Server.Errors           ( backendError
                                                , badNamespace
                                                )
import           Unison.Util.Pretty             ( Width )
import           Unison.Var                     ( Var )
import           Unison.Codebase                ( Codebase )
import           Unison.Codebase.ShortBranchHash
                                                ( ShortBranchHash )
import           Unison.Prelude

type DefinitionsAPI =
  "getDefinition" :> QueryParam "rootBranch" ShortBranchHash
                  :> QueryParam "relativeTo" HashQualifiedName
                  :> QueryParams "names" HashQualifiedName
                  :> QueryParam "renderWidth" Width
  :> Get '[JSON] DefinitionDisplayResults

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

instance ToParam (QueryParam "relativeTo" HashQualifiedName) where
  toParam _ = DocQueryParam
    "relativeTo"
    [".", ".base", "foo.bar"]
    ("The namespace relative to which the `names` parameter is to be resolved. "
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
  => Codebase IO v Ann
  -> Maybe ShortBranchHash
  -> Maybe HashQualifiedName
  -> [HashQualifiedName]
  -> Maybe Width
  -> Handler DefinitionDisplayResults
serveDefinitions codebase mayRoot relativePath hqns width = do
  rel <- fmap Path.fromPath' <$> traverse (parsePath . Text.unpack) relativePath
  ea  <- liftIO . runExceptT $ do
    root <- traverse (Backend.expandShortBranchHash codebase) mayRoot
    Backend.prettyDefinitionsBySuffixes rel root width codebase
      $   HQ.unsafeFromText
      <$> hqns
  errFromEither backendError ea
 where
  parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
  errFromEither f = either (throwError . f) pure
