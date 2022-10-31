{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.NamespaceDetails where

import Control.Monad.Except
import Data.Aeson
import Data.OpenApi (ToSchema)
import Servant (Capture, QueryParam, (:>))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..))
import Servant.OpenApi ()
import qualified U.Codebase.Causal as V2Causal
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Rt
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend
import qualified Unison.Server.Backend as Backend
import Unison.Server.Doc (Doc)
import Unison.Server.Types
  ( APIGet,
    UnisonHash,
    mayDefaultWidth,
    v2CausalBranchToUnisonHash,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type NamespaceDetailsAPI =
  "namespaces" :> Capture "namespace" Path.Path
    :> QueryParam "rootBranch" ShortCausalHash
    :> QueryParam "renderWidth" Width
    :> APIGet NamespaceDetails

instance ToCapture (Capture "namespace" Text) where
  toCapture _ =
    DocCapture
      "namespace"
      "The fully qualified name of a namespace. The leading `.` is optional."

instance ToSample NamespaceDetails where
  toSamples _ =
    [ ( "When no value is provided for `namespace`, the root namespace `.` is "
          <> "listed by default",
        NamespaceDetails
          Path.empty
          "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
          Nothing
      )
    ]

data NamespaceDetails = NamespaceDetails
  { fqn :: Path.Path,
    hash :: UnisonHash,
    readme :: Maybe Doc
  }
  deriving (Generic, Show)

instance ToJSON NamespaceDetails where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamespaceDetails

namespaceDetails ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Path.Path ->
  Maybe ShortCausalHash ->
  Maybe Width ->
  Backend IO NamespaceDetails
namespaceDetails runtime codebase namespacePath maySBH mayWidth =
  let width = mayDefaultWidth mayWidth
   in do
        rootCausal <- Backend.resolveRootBranchHashV2 codebase maySBH
        namespaceCausal <- lift $ Codebase.getShallowCausalAtPath codebase namespacePath (Just rootCausal)
        shallowBranch <- lift $ V2Causal.value namespaceCausal
        namespaceDetails <- do
          (_localNamesOnly, ppe) <- Backend.scopedNamesForBranchHash codebase (Just rootCausal) namespacePath
          readme <-
            Backend.findShallowReadmeInBranchAndRender
              width
              runtime
              codebase
              ppe
              shallowBranch
          let causalHash = v2CausalBranchToUnisonHash namespaceCausal
          pure $ NamespaceDetails namespacePath causalHash readme

        pure $ namespaceDetails
