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
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bitraverse)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Servant (Capture, QueryParam, throwError, (:>), type (:<|>))
import Servant.Docs (ToSample (..), noSamples)
import Servant.OpenApi ()
import qualified U.Codebase.Causal as V2Causal
import qualified U.Codebase.Decl as Decl
import qualified U.Codebase.Reference as V2Reference
import qualified U.Codebase.Referent as V2Referent
import qualified U.Util.Hash as Hash
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Path.Parse (parsePath')
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
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
    ExactName (..),
    NamespaceFQN,
    TermTag (..),
    TypeTag,
    UnisonHash,
    UnisonName,
    mayDefaultWidth,
  )
import qualified Unison.ShortHash as SH
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)
import qualified Unison.Util.Relation as Relation

type TermSummaryAPI =
  "definitions" :> "terms" :> "by_name" :> Capture "fqn" Name.Name :> "summary"
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
  exactNameRef@ExactName {ref = termReferent} <- bitraverse pure referentFromShortHash exactNameSH `whenNothing` throwError (Backend.NoSuchDefinition (HQ.HashQualified termName shortHash))
  let relativeToPath = fromMaybe Path.empty relativeTo
  let termReference = V2Referent.toReference termReferent
  root <- Backend.resolveRootBranchHashV2 codebase mayRoot
  let (namePath, segment) = Path.splitFromName termName
  -- The full path, relative to the root.
  let path = Path.resolve relativeTo namePath
  relativeCausal <- lift $ Codebase.getShallowCausalAtPath codebase path (Just root)
  relativeBranch <- lift $ V2Causal.value relativeCausal
  sig <- lift (Codebase.getTypeOfTerm codebase (Cv.reference2to1 termReference))
  case sig of
    Nothing ->
      throwError (Backend.MissingSignatureForTerm $ Cv.reference2to1 termReference)
    Just typeSig -> do
      (_localNames, ppe) <- Backend.scopedNamesForBranchHash codebase (Just root) relativeToPath
      let formattedTermSig = Backend.formatSuffixedType ppe width typeSig
      let summary = mkSummary termReference formattedTermSig
      tag <- lift $ Backend.getTermTag codebase relativeBranch exactNameRef sig
      pure $ TermSummary (Name.toText termName) (SH.toText shortHash) summary tag
  where
    referentFromShortHash :: SH.ShortHash -> Maybe V2Referent.Referent
    referentFromShortHash = \case
      SH.Builtin b -> Just $ V2Referent.Ref (Reference.Builtin b)
      SH.ShortHash prefix cycle mayConId -> do
        h <- Hash.fromBase32HexText prefix
        ref <- case cycle of
          Nothing -> Just (V2Reference.Derived h 0)
          Just i -> V2Reference.Derived h <$> readMay (Text.unpack i)
        case mayConId >>= readMaybe @Decl.ConstructorId . Text.unpack of
          Nothing -> Just $ V2Referent.Ref ref
          Just conId -> _
    errFromEither f = either (throwError . f) pure
    fqnToNamespacePath fqn = do
      let fqnS = Text.unpack fqn
      path' <- errFromEither (`Backend.BadNamespace` fqnS) $ parsePath' fqnS
      pure (Path.fromPath' path')

    width = mayDefaultWidth mayWidth

    mkSummary reference termSig =
      if V2Reference.isBuiltin reference
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
