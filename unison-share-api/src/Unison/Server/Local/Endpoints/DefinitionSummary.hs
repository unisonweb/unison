{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Local.Endpoints.DefinitionSummary
  ( TermSummaryAPI,
    serveTermSummary,
    TermSummary (..),
    TypeSummaryAPI,
    serveTypeSummary,
    TypeSummary (..),
  )
where

import Control.Monad.Reader
import Servant (Capture, QueryParam, throwError, (:>))
import Servant.Docs (ToSample (..), noSamples)
import Servant.OpenApi ()
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Backend (Backend)
import Unison.Server.Backend qualified as Backend
import Unison.Server.Types
  ( APIGet,
    TermSummary(..),
    TypeSummary(..),
    mayDefaultWidth,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type TermSummaryAPI =
  "definitions"
    :> "terms"
    :> "by-hash"
    :> Capture "hash" Referent
    :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TermSummary

serveTermSummary ::
  Codebase IO Symbol Ann ->
  Referent ->
  Maybe Name ->
  Either ShortCausalHash CausalHash ->
  Maybe Path.Path ->
  Maybe Width ->
  Backend IO TermSummary
serveTermSummary codebase referent mayName root relativeTo mayWidth = do
  let shortHash = Referent.toShortHash referent
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  let relativeToPath = fromMaybe mempty relativeTo
  let termReference = Referent.toReference referent
  let v2Referent = Cv.referent1to2 referent

  (root, sig) <-
    Backend.hoistBackend (Codebase.runTransaction codebase) do
      root <- Backend.normaliseRootCausalHash root
      sig <- lift (Backend.loadReferentType codebase referent)
      pure (root, sig)
  case sig of
    Nothing ->
      throwError (Backend.MissingSignatureForTerm termReference)
    Just typeSig -> do
      (_localNames, ppe) <- Backend.namesAtPathFromRootBranchHash codebase root relativeToPath
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
  "definitions"
    :> "types"
    :> "by-hash"
    :> Capture "hash" Reference
    :> "summary"
    -- Optional name to include in summary.
    -- It's propagated through to the response as-is.
    -- If missing, the short hash will be used instead.
    :> QueryParam "name" Name
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet TypeSummary

serveTypeSummary ::
  Codebase IO Symbol Ann ->
  Reference ->
  Maybe Name ->
  Either ShortCausalHash CausalHash ->
  Maybe Path.Path ->
  Maybe Width ->
  Backend IO TypeSummary
serveTypeSummary codebase reference mayName _mayRoot _relativeTo mayWidth = do
  let shortHash = Reference.toShortHash reference
  let displayName = maybe (HQ.HashOnly shortHash) HQ.NameOnly mayName
  (tag, displayDecl) <-
    lift do
      Codebase.runTransaction codebase do
        tag <- Backend.getTypeTag codebase reference
        displayDecl <- Backend.displayType codebase reference
        pure (tag, displayDecl)
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
