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
    TermSummary (..),
    TypeSummaryAPI,
    serveTypeSummary,
    TypeSummary (..),
  )
where

import Data.Aeson
import Data.Bifunctor (bimap)
import Data.OpenApi (ToSchema)
import Servant (Capture, QueryParam, throwError, (:>))
import Servant.Docs (ToSample (..), noSamples)
import Servant.OpenApi ()
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Server.Backend (Backend)
import qualified Unison.Server.Backend as Backend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    TermTag (..),
    TypeTag,
    mayDefaultWidth,
  )
import qualified Unison.ShortHash as SH
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type TermSummaryAPI =
  "definitions" :> "terms" :> "by-hash" :> Capture "hash" Referent :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TermSummary

instance ToSample TermSummary where
  toSamples _ = noSamples

data TermSummary = TermSummary
  { displayName :: HQ.HashQualified Name,
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
  Referent ->
  Maybe Name ->
  Maybe ShortBranchHash ->
  Maybe Path.Path ->
  Maybe Width ->
  Backend IO TermSummary
serveTermSummary codebase referent mayName mayRoot relativeTo mayWidth = do
  let shortHash = Referent.toShortHash referent
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  let relativeToPath = fromMaybe Path.empty relativeTo
  let termReference = Referent.toReference referent
  let v2Referent = Cv.referent1to2 referent
  root <- Backend.resolveRootBranchHashV2 codebase mayRoot
  sig <- lift $ Backend.loadReferentType codebase referent
  case sig of
    Nothing ->
      throwError (Backend.MissingSignatureForTerm termReference)
    Just typeSig -> do
      (_localNames, ppe) <- Backend.scopedNamesForBranchHash codebase (Just root) relativeToPath
      let formattedTermSig = Backend.formatSuffixedType ppe width typeSig
      let summary = mkSummary termReference formattedTermSig
      tag <- lift $ Backend.getTermTag codebase v2Referent sig
      pure $ TermSummary displayName shortHash summary tag
  where
    width = mayDefaultWidth mayWidth

    mkSummary reference termSig =
      if Reference.isBuiltin reference
        then BuiltinObject termSig
        else UserObject termSig

type TypeSummaryAPI =
  "definitions" :> "types" :> "by-hash" :> Capture "hash" Reference :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "rootBranch" ShortBranchHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TypeSummary

instance ToSample TypeSummary where
  toSamples _ = noSamples

data TypeSummary = TypeSummary
  { displayName :: HQ.HashQualified Name,
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
  Reference ->
  Maybe Name ->
  Maybe ShortBranchHash ->
  Maybe Path.Path ->
  Maybe Width ->
  Backend IO TypeSummary
serveTypeSummary codebase reference mayName _mayRoot _relativeTo mayWidth = do
  let shortHash = Reference.toShortHash reference
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  tag <- lift $ Backend.getTypeTag codebase reference
  displayDecl <- lift $ Backend.displayType codebase reference
  let syntaxHeader = Backend.typeToSyntaxHeader width displayName displayDecl
  pure $
    TypeSummary
      { displayName = displayName,
        hash = shortHash,
        summary = bimap Backend.mungeSyntaxText Backend.mungeSyntaxText syntaxHeader,
        tag = tag
      }
  where
    width = mayDefaultWidth mayWidth
