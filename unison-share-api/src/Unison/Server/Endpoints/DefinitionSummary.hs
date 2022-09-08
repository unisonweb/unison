{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Endpoints.DefinitionSummary where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Servant (Capture, QueryParam, throwError, (:>), type (:<|>))
import Servant.Docs (ToSample (..), noSamples)
import Servant.OpenApi ()
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
import Unison.Server.Backend (Backend)
import qualified Unison.Server.Backend as Backend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    NamespaceFQN,
    TermTag (..),
    TypeTag,
    UnisonHash,
    UnisonName,
    mayDefaultWidth,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)
import qualified Unison.Util.Relation as Relation

type DefinitionSummaryAPI = TermSummaryAPI :<|> TypeSummaryAPI

type TermSummaryAPI =
  "definitions" :> "terms" :> "by_name" :> Capture "fqn" UnisonName :> "summary"
    :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" NamespaceFQN
    :> QueryParam "renderWidth" Width
    :> APIGet TermSummary

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

serveTermSummary ::
  Codebase IO Symbol Ann ->
  Name.Name ->
  Maybe ShortBranchHash ->
  Maybe NamespaceFQN ->
  Maybe Width ->
  Backend IO TermSummary
serveTermSummary codebase termName mayRoot namespaceName mayWidth = do
  root <- Backend.resolveRootBranchHashV2 codebase mayRoot
  -- branch <- case namespaceName of
  --   Nothing ->
  --     Backend.resolveRootBranchHash mayRoot codebase
  --   Just n -> do
  --     namespacePath <- fqnToNamespacePath n
  --     root <- Backend.resolveRootBranchHash mayRoot codebase
  --     pure $ Branch.getAt' namespacePath root

  -- let rel = Branch.deepTerms branch0
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
      let ppe = Backend.basicSuffixifiedNames hashLength branch (Backend.AllNames Path.empty)
      let formattedTermSig = Backend.formatSuffixedType (PrettyPrintEnvDecl ppe ppe) width typeSig
      let summary = mkSummary termReference formattedTermSig
      termBranch <- Codebase.getShallowCausalAtPath
      let tag = Backend.getTermTag branch0 termReferent sig

      pure $ TermSummary (Name.toText termName) hash summary tag
  where
    errFromEither f = either (throwError . f) pure
    fqnToNamespacePath fqn = do
      let fqnS = Text.unpack fqn
      path' <- errFromEither (`Backend.BadNamespace` fqnS) $ parsePath' fqnS
      pure (Path.fromPath' path')

    width = mayDefaultWidth mayWidth

    mkSummary reference termSig =
      if Reference.isBuiltin reference
        then BuiltinObject termSig
        else UserObject termSig

type TypeSummaryAPI =
  "definitions" :> "types" :> "by_name" :> Capture "fqn" UnisonName :> "summary"
    :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" NamespaceFQN
    :> QueryParam "renderWidth" Width
    :> APIGet TypeSummary

data TypeSummary = TypeSummary
  { fqn :: UnisonName,
    hash :: UnisonHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TypeTag
  }
  deriving (Generic, Show)

instance ToJSON TypeSummary where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TypeSummary
