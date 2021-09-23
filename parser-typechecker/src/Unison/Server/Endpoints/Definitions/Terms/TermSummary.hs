{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.Definitions.Terms.TermSummary where

import Control.Error (runExceptT)
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Servant (Capture, QueryParam, throwError, (:>))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..), noSamples)
import Servant.OpenApi ()
import Servant.Server (Handler)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Path.Parse (parsePath')
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Name as Name
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Server.Backend as Backend
import Unison.Server.Errors (backendError)
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    APIHeaders,
    NamespaceFQN,
    TermTag (..),
    UnisonHash,
    UnisonName,
    addHeaders,
    mayDefaultWidth,
  )
import Unison.Util.Pretty (Width)
import qualified Unison.Util.Relation as Relation
import Unison.Var (Var)

type TermSummaryAPI =
  "definitions" :> "terms" :> "by_name" :> Capture "fqn" UnisonName :> "summary"
    :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" NamespaceFQN
    :> QueryParam "renderWidth" Width
    :> APIGet TermSummary

instance ToCapture (Capture "fqn" Text) where
  toCapture _ =
    DocCapture
      "fqn"
      "The fully qualified name of a definition. The leading `.` is optional."

instance ToSample TermSummary where
  toSamples _ = noSamples

data TermSummary = TermSummary
  { fqn :: UnisonName,
    hash :: UnisonHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TermTag
  }
  deriving (Generic, Show)

instance ToJSON TermSummary where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TermSummary

-- TODO: Should this be using Backend.TermEntry as a way to get to TermSummary?
serve ::
  Var v =>
  Handler () ->
  Codebase IO v Ann ->
  UnisonName ->
  Maybe ShortBranchHash ->
  Maybe NamespaceFQN ->
  Maybe Width ->
  Handler (APIHeaders TermSummary)
serve tryAuth codebase rawTermName mayRoot namespaceName mayWidth =
  let inBackend a = do
        ea <- liftIO $ runExceptT a
        errFromEither backendError ea

      errFromEither f = either (throwError . f) pure

      fqnToNamespacePath fqn = do
        let fqnS = Text.unpack fqn
        path' <- errFromEither (`Backend.BadNamespacePath` fqnS) $ parsePath' fqnS
        pure (Path.fromPath' path')

      width = mayDefaultWidth mayWidth

      mkSummary reference termSig =
        if Reference.isBuiltin reference
          then BuiltinObject termSig
          else UserObject termSig
   in do
        -- TODO: Should servant do this Name.unsafeFromString?
        let termName = Name.unsafeFromText rawTermName

        termSummary <- inBackend $ do
          branch <- case namespaceName of
            Nothing ->
              Backend.resolveRootBranchHash mayRoot codebase
            Just n -> do
              namespacePath <- fqnToNamespacePath n
              root <- Backend.resolveRootBranchHash mayRoot codebase
              pure $ Branch.getAt' namespacePath root

          let branch0 = Branch.head branch
          let rel = Branch.deepTerms branch0
          -- TODO: Map exception of missing element in set to 404 - noSuchDefinition
          let termReferent = Set.elemAt 0 (Relation.lookupRan termName rel)
          let termReference = Referent.toReference termReferent
          let hash = Referent.toText termReferent
          sig <- lift (Codebase.getTypeOfTerm codebase termReference)
          case sig of
            Nothing ->
              throwError (Backend.MissingSignatureForTerm termReference)
            Just typeSig -> do
              hashLength <- liftIO $ Codebase.hashLength codebase
              let ppe = Backend.basicSuffixifiedNames hashLength branch Path.empty
              let formattedTermSig = Backend.formatSuffixedType (PrettyPrintEnvDecl ppe ppe) width typeSig
              let summary = mkSummary termReference formattedTermSig
              tag <- Backend.getTermTag branch0 termReferent sig

              pure $ TermSummary rawTermName hash summary tag

        addHeaders <$> (tryAuth $> termSummary)
