{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Endpoints.DefinitionSummary
  ( TermSummaryAPI,
    serveTermSummary,
    TypeSummaryAPI,
    serveTypeSummary,
  )
where

import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.OpenApi (ToSchema)
import qualified Data.Set.NonEmpty as NESet
import Servant (Capture, QueryParam, throwError, (:>))
import Servant.Docs (ToSample (..), noSamples)
import Servant.OpenApi ()
import qualified U.Codebase.Causal as V2Causal
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Server.Backend (Backend)
import qualified Unison.Server.Backend as Backend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    ExactName (..),
    TermTag (..),
    TypeTag,
    exactToHQ,
    mayDefaultWidth,
  )
import qualified Unison.ShortHash as SH
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type TermSummaryAPI =
  "definitions" :> "terms" :> "qualified" :> Capture "fqn" (ExactName Name SH.ShortHash) :> "summary"
    :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TermSummary

instance ToSample TermSummary where
  toSamples _ = noSamples

data TermSummary = TermSummary
  { fqn :: Name,
    hash :: SH.ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TermTag
  }
  deriving (Generic, Show)

instance ToJSON TermSummary where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TermSummary

serveTermSummary ::
  Codebase IO Symbol Ann ->
  ExactName Name SH.ShortHash ->
  Maybe ShortBranchHash ->
  Maybe Path.Path ->
  Maybe Width ->
  Backend IO TermSummary
serveTermSummary codebase exactNameSH@(ExactName {name = termName, ref = shortHash}) mayRoot relativeTo mayWidth = do
  exactNameRef@ExactName {ref = termReferent} <-
    exactNameSH & bitraverse pure \shortHash -> do
      matchingReferents <- lift $ Backend.termReferentsByShortHash codebase shortHash
      case NESet.nonEmptySet matchingReferents of
        Just neSet
          | NESet.size neSet == 1 -> pure $ NESet.findMin neSet
          | otherwise -> throwError $ Backend.AmbiguousHashForDefinition shortHash
        Nothing -> throwError $ Backend.NoSuchDefinition (exactToHQ exactNameSH)
  let relativeToPath = fromMaybe Path.empty relativeTo
  let termReference = Referent.toReference termReferent
  let v2ExactName = bimap id Cv.referent1to2 exactNameRef
  root <- Backend.resolveRootBranchHashV2 codebase mayRoot
  relativeToCausal <- lift $ Codebase.getShallowCausalAtPath codebase relativeToPath (Just root)
  relativeToBranch <- lift $ V2Causal.value relativeToCausal
  sig <- lift $ Backend.loadReferentType codebase termReferent
  case sig of
    Nothing ->
      throwError (Backend.MissingSignatureForTerm termReference)
    Just typeSig -> do
      (_localNames, ppe) <- Backend.scopedNamesForBranchHash codebase (Just root) relativeToPath
      let formattedTermSig = Backend.formatSuffixedType ppe width typeSig
      let summary = mkSummary termReference formattedTermSig
      tag <- lift $ Backend.getTermTag codebase relativeToBranch v2ExactName sig
      pure $ TermSummary termName shortHash summary tag
  where
    width = mayDefaultWidth mayWidth

    mkSummary reference termSig =
      if Reference.isBuiltin reference
        then BuiltinObject termSig
        else UserObject termSig

type TypeSummaryAPI =
  "definitions" :> "types" :> "qualified" :> Capture "fqn" (ExactName Name SH.ShortHash) :> "summary"
    :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TypeSummary

instance ToSample TypeSummary where
  toSamples _ = noSamples

data TypeSummary = TypeSummary
  { fqn :: Name,
    hash :: SH.ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TypeTag
  }
  deriving (Generic, Show)

instance ToJSON TypeSummary where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TypeSummary

serveTypeSummary ::
  Codebase IO Symbol Ann ->
  ExactName Name SH.ShortHash ->
  Maybe ShortBranchHash ->
  Maybe Path.Path ->
  Maybe Width ->
  Backend IO TypeSummary
serveTypeSummary codebase exactNameSH@(ExactName {name, ref = shortHash}) _mayRoot _relativeTo mayWidth = do
  let hqName = exactToHQ exactNameSH
  typeReference <- do
    matchingReferences <- lift $ Backend.typeReferencesByShortHash codebase shortHash
    case NESet.nonEmptySet matchingReferences of
      Just neSet
        | NESet.size neSet == 1 -> pure $ NESet.findMin neSet
        | otherwise -> throwError $ Backend.AmbiguousHashForDefinition shortHash
      Nothing -> throwError $ Backend.NoSuchDefinition (exactToHQ exactNameSH)
  tag <- lift $ Backend.getTypeTag codebase typeReference
  displayDecl <- lift $ Backend.displayType codebase typeReference
  let syntaxHeader = Backend.typeToSyntaxHeader width hqName displayDecl
  pure $
    TypeSummary
      { fqn = name,
        hash = shortHash,
        summary = bimap Backend.mungeSyntaxText Backend.mungeSyntaxText syntaxHeader,
        tag = tag
      }
  where
    width = mayDefaultWidth mayWidth
