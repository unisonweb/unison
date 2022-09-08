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
  )
where

import Data.Aeson
import Data.Bitraversable (bitraverse)
import Data.OpenApi (ToSchema)
import Servant (Capture, QueryParam, throwError, (:>))
import Servant.Docs (ToSample (..), noSamples)
import Servant.OpenApi ()
import qualified U.Codebase.Causal as V2Causal
import qualified U.Codebase.Reference as V2Reference
import qualified U.Codebase.Referent as V2Referent
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend (Backend)
import qualified Unison.Server.Backend as Backend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    ExactName (..),
    TermTag (..),
    mayDefaultWidth,
  )
import qualified Unison.ShortHash as SH
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type TermSummaryAPI =
  "definitions" :> "terms" :> "by_name" :> Capture "fqn" (ExactName Name SH.ShortHash) :> "summary"
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
  exactNameRef@ExactName {ref = termReferent} <- bitraverse pure Cv.shorthash1toreferent2 exactNameSH `whenNothing` throwError (Backend.NoSuchDefinition (HQ.HashQualified termName shortHash))
  let relativeToPath = fromMaybe Path.empty relativeTo
  let termReference = V2Referent.toReference termReferent
  let v1Reference = Cv.reference2to1 termReference
  root <- Backend.resolveRootBranchHashV2 codebase mayRoot
  relativeToCausal <- lift $ Codebase.getShallowCausalAtPath codebase relativeToPath (Just root)
  relativeToBranch <- lift $ V2Causal.value relativeToCausal
  sig <- lift (Codebase.getTypeOfTerm codebase v1Reference)
  case sig of
    Nothing ->
      throwError (Backend.MissingSignatureForTerm v1Reference)
    Just typeSig -> do
      (_localNames, ppe) <- Backend.scopedNamesForBranchHash codebase (Just root) relativeToPath
      let formattedTermSig = Backend.formatSuffixedType ppe width typeSig
      let summary = mkSummary termReference formattedTermSig
      tag <- lift $ Backend.getTermTag codebase relativeToBranch exactNameRef sig
      pure $ TermSummary termName shortHash summary tag
  where
    width = mayDefaultWidth mayWidth

    mkSummary reference termSig =
      if V2Reference.isBuiltin reference
        then BuiltinObject termSig
        else UserObject termSig
