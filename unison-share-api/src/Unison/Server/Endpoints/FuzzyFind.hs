{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.FuzzyFind where

import Control.Monad.Except
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.OpenApi (ToSchema)
import Servant
  ( QueryParam,
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (Normal),
    ToParam (..),
    ToSample (..),
    noSamples,
  )
import Servant.OpenApi ()
import qualified Text.FuzzyFind as FZF
import qualified U.Codebase.Causal as V2Causal
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.ShortCausalHash as SCH
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.Server.Backend as Backend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    ExactName (..),
    HashQualifiedName,
    NamedTerm,
    NamedType,
    UnisonName,
    mayDefaultWidth,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type FuzzyFindAPI =
  "find" :> QueryParam "rootBranch" SCH.ShortCausalHash
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "limit" Int
    :> QueryParam "renderWidth" Width
    :> QueryParam "query" String
    :> APIGet [(FZF.Alignment, FoundResult)]

instance ToSample FZF.Alignment where
  toSamples _ = noSamples

instance ToParam (QueryParam "limit" Int) where
  toParam _ =
    DocQueryParam
      "limit"
      ["1", "10", "20"]
      "The maximum number of results to return. Defaults to 10."
      Normal

instance ToParam (QueryParam "query" String) where
  toParam _ =
    DocQueryParam
      "query"
      ["foo", "ff", "td nr"]
      "Space-separated subsequences to find in the name of a type or term."
      Normal

instance ToJSON FZF.Alignment where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON FZF.Result where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON FZF.ResultSegment where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema FZF.Alignment

deriving anyclass instance ToSchema FZF.Result

deriving instance ToSchema FZF.ResultSegment

data FoundTerm = FoundTerm
  { bestFoundTermName :: HashQualifiedName,
    namedTerm :: NamedTerm
  }
  deriving (Generic, Show)

data FoundType = FoundType
  { bestFoundTypeName :: HashQualifiedName,
    typeDef :: DisplayObject SyntaxText SyntaxText,
    namedType :: NamedType
  }
  deriving (Generic, Show)

instance ToJSON FoundType

deriving instance ToSchema FoundType

instance ToJSON FoundTerm

deriving instance ToSchema FoundTerm

data FoundResult
  = FoundTermResult FoundTerm
  | FoundTypeResult FoundType
  deriving (Generic, Show)

instance ToJSON FoundResult

deriving instance ToSchema FoundResult

instance ToSample FoundResult where
  toSamples _ = noSamples

serveFuzzyFind ::
  forall m.
  MonadIO m =>
  Codebase m Symbol Ann ->
  Maybe SCH.ShortCausalHash ->
  Maybe Path.Path ->
  Maybe Int ->
  Maybe Width ->
  Maybe String ->
  Backend.Backend m [(FZF.Alignment, FoundResult)]
serveFuzzyFind codebase mayRoot relativeTo limit typeWidth query = do
  let path = fromMaybe Path.empty relativeTo
  rootHash <- traverse (Backend.expandShortCausalHash codebase) mayRoot
  rootCausal <- Backend.resolveCausalHashV2 codebase (Cv.causalHash1to2 <$> rootHash)
  (localNamesOnly, ppe) <- Backend.scopedNamesForBranchHash codebase (Just rootCausal) path
  relativeToCausal <- lift $ Codebase.getShallowCausalAtPath codebase path (Just rootCausal)
  relativeToBranch <- lift $ V2Causal.value relativeToCausal
  let alignments ::
        ( [ ( FZF.Alignment,
              UnisonName,
              [Backend.FoundRef]
            )
          ]
        )
      alignments =
        take (fromMaybe 10 limit) $ Backend.fuzzyFind localNamesOnly (fromMaybe "" query)
  lift (join <$> traverse (loadEntry relativeToBranch (PPE.suffixifiedPPE ppe)) alignments)
  where
    loadEntry relativeToBranch ppe (a, n, refs) = do
      for refs $
        \case
          Backend.FoundTermRef r ->
            ( \te ->
                ( a,
                  FoundTermResult
                    . FoundTerm
                      (Backend.bestNameForTerm @Symbol ppe (mayDefaultWidth typeWidth) r)
                    $ Backend.termEntryToNamedTerm ppe typeWidth te
                )
            )
              <$> Backend.termListEntry codebase relativeToBranch (ExactName (NameSegment n) (Cv.referent1to2 r))
          Backend.FoundTypeRef r ->
            Codebase.runTransaction codebase do
              te <- Backend.typeListEntry codebase relativeToBranch (ExactName (NameSegment n) r)
              let namedType = Backend.typeEntryToNamedType te
              let typeName = Backend.bestNameForType @Symbol ppe (mayDefaultWidth typeWidth) r
              typeHeader <- Backend.typeDeclHeader codebase ppe r
              let ft = FoundType typeName typeHeader namedType
              pure (a, FoundTypeResult ft)
