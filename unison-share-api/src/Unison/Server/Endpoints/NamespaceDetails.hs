{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.NamespaceDetails where

import Control.Error (runExceptT)
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import Servant (Capture, QueryParam, throwError, (:>))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..))
import Servant.OpenApi ()
import Servant.Server (Handler)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Path.Parse (parsePath')
import qualified Unison.Codebase.Runtime as Rt
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import Unison.Server.Doc (Doc)
import Unison.Server.Errors (backendError, badNamespace)
import Unison.Server.Types
  ( APIGet,
    APIHeaders,
    NamespaceFQN,
    UnisonHash,
    UnisonName,
    addHeaders,
    branchToUnisonHash,
    mayDefaultWidth,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type NamespaceDetailsAPI =
  "namespaces" :> Capture "namespace" NamespaceFQN
    :> QueryParam "rootBranch" ShortBranchHash
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
          "."
          "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
          Nothing
      )
    ]

data NamespaceDetails = NamespaceDetails
  { fqn :: UnisonName,
    hash :: UnisonHash,
    readme :: Maybe Doc
  }
  deriving (Generic, Show)

instance ToJSON NamespaceDetails where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamespaceDetails

serve ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  NamespaceFQN ->
  Maybe ShortBranchHash ->
  Maybe Width ->
  Handler (APIHeaders NamespaceDetails)
serve runtime codebase namespaceName mayRoot mayWidth =
  let doBackend a = do
        ea <- liftIO $ runExceptT a
        errFromEither backendError ea

      errFromEither f = either (throwError . f) pure

      fqnToPath fqn = do
        let fqnS = Text.unpack fqn
        path' <- errFromEither (`badNamespace` fqnS) $ parsePath' fqnS
        pure (Path.fromPath' path')

      width = mayDefaultWidth mayWidth
   in do
        namespacePath <- fqnToPath namespaceName

        namespaceDetails <- doBackend $ do
          root <- Backend.resolveRootBranchHash mayRoot codebase

          let namespaceBranch = Branch.getAt' namespacePath root

          -- Names used in the README should not be confined to the namespace
          -- of the README (since it could be referencing definitions from all
          -- over the codebase)
          let printNames = Backend.getCurrentPrettyNames (Backend.AllNames namespacePath) root

          readme <-
            Backend.findShallowReadmeInBranchAndRender
              width
              runtime
              codebase
              printNames
              namespaceBranch

          pure $ NamespaceDetails namespaceName (branchToUnisonHash namespaceBranch) readme

        pure $ addHeaders namespaceDetails
