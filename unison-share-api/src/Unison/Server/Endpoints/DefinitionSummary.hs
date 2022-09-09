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
    exactToHQ,
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
  exactNameRef@ExactName {ref} <-
    exactNameSH & bitraverse pure \shortHash -> do
      matchingReferents <- lift $ Backend.termReferentsByShortHash codebase shortHash
      case NESet.nonEmptySet matchingReferents of
        Just neSet
          | NESet.size neSet == 1 -> pure $ NESet.findMin neSet
          | otherwise -> throwError $ Backend.AmbiguousHashForDefinition shortHash
        Nothing -> throwError $ Backend.NoSuchDefinition (exactToHQ exactNameSH)
  let relativeToPath = fromMaybe Path.empty relativeTo
  let termReference = Referent.toReference ref
  let v2ExactName = bimap id Cv.referent1to2 exactNameRef
  root <- Backend.resolveRootBranchHashV2 codebase mayRoot
  relativeToCausal <- lift $ Codebase.getShallowCausalAtPath codebase relativeToPath (Just root)
  relativeToBranch <- lift $ V2Causal.value relativeToCausal
  sig <- lift (Codebase.getTypeOfTerm codebase termReference)
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
