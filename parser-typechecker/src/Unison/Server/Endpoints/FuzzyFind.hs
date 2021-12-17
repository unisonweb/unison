{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.FuzzyFind where

import Control.Error (runExceptT)
import Data.Aeson ( defaultOptions, genericToEncoding, ToJSON(toEncoding) )
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import Servant
  ( QueryParam,
    throwError,
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
import Servant.Server (Handler)
import qualified Text.FuzzyFind as FZF
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.HashQualified' as HQ'
import Unison.NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import Unison.Server.Errors
  ( backendError,
    badNamespace,
  )
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    APIHeaders,
    HashQualifiedName,
    NamedTerm,
    NamedType,
    addHeaders,
    mayDefaultWidth,
  )
import Unison.Util.Pretty (Width)
import Unison.Var (Var)

type FuzzyFindAPI =
  "find" :> QueryParam "rootBranch" SBH.ShortBranchHash
         :> QueryParam "relativeTo" HashQualifiedName
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
  { bestFoundTermName :: HashQualifiedName
  , namedTerm :: NamedTerm
  } deriving (Generic, Show)

data FoundType = FoundType
  { bestFoundTypeName :: HashQualifiedName
  , typeDef :: DisplayObject SyntaxText SyntaxText
  , namedType :: NamedType
  } deriving (Generic, Show)

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

serveFuzzyFind
  :: forall v
   . Var v
  => Handler ()
  -> Codebase IO v Ann
  -> Maybe SBH.ShortBranchHash
  -> Maybe HashQualifiedName
  -> Maybe Int
  -> Maybe Width
  -> Maybe String
  -> Handler (APIHeaders [(FZF.Alignment, FoundResult)])
serveFuzzyFind h codebase mayRoot relativePath limit typeWidth query =
  addHeaders <$> do
    h
    rel <-
      maybe mempty Path.fromPath'
      <$> traverse (parsePath . Text.unpack) relativePath
    hashLength <- liftIO $ Codebase.hashLength codebase
    ea         <- liftIO . runExceptT $ do
      root   <- traverse (Backend.expandShortBranchHash codebase) mayRoot
      branch <- Backend.resolveBranchHash root codebase
      let b0 = Branch.head branch
          alignments =
            take (fromMaybe 10 limit) $ Backend.fuzzyFind rel branch (fromMaybe "" query)
          -- Use AllNames to render source
          ppe = Backend.basicSuffixifiedNames hashLength branch (Backend.AllNames rel)
      liftIO (join <$> traverse (loadEntry root (Just rel) ppe b0) alignments)
    errFromEither backendError ea
 where
  loadEntry _root _rel ppe b0 (a, HQ'.NameOnly . NameSegment -> n, refs) =
    for refs $
      \case
        Backend.FoundTermRef r ->
          (\te ->
              ( a
              , FoundTermResult
                . FoundTerm
                    (Backend.bestNameForTerm @v ppe (mayDefaultWidth typeWidth) r)
                $ Backend.termEntryToNamedTerm ppe typeWidth te
              )
            )
            <$> Backend.termListEntry codebase b0 r n
        Backend.FoundTypeRef r -> do
          te <- Backend.typeListEntry codebase r n
          let namedType = Backend.typeEntryToNamedType te
          let typeName = Backend.bestNameForType @v ppe (mayDefaultWidth typeWidth) r
          typeHeader <- Backend.typeDeclHeader codebase ppe r
          let ft = FoundType typeName typeHeader namedType
          pure (a, FoundTypeResult ft)

  parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
  errFromEither f = either (throwError . f) pure
