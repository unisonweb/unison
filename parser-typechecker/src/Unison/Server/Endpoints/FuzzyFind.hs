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

import           Control.Lens                   ( view, _1 )
import           Control.Error                  ( runExceptT )
import           Data.Function                  ( on )
import           Data.Aeson
import           Data.List                      ( sortBy )
import           Data.Ord                       ( Down(..) )
import           Data.OpenApi                   ( ToSchema )
import           Servant                        ( Get
                                                , JSON
                                                , QueryParam
                                                , throwError
                                                , (:>)
                                                )
import           Servant.Docs                   ( DocQueryParam(..)
                                                , ParamKind(Normal)
                                                , ToParam(..)
                                                , ToSample(..)
                                                , noSamples
                                                )
import           Servant.OpenApi                ( )
import           Servant.Server                 ( Handler )
import           Unison.Prelude
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Path          as Path
import qualified Unison.HashQualified'         as HQ'
import           Unison.Parser                  ( Ann )
import qualified Unison.Server.Backend         as Backend
import           Unison.Server.Errors           ( backendError
                                                , badNamespace
                                                )
import           Unison.Server.Types            ( HashQualifiedName
                                                , NamedTerm
                                                , NamedType
                                                )
import           Unison.Util.Pretty             ( Width )
import           Unison.Var                     ( Var )
import qualified Unison.Codebase.ShortBranchHash
                                               as SBH
import qualified Data.Text                     as Text
import qualified Text.FuzzyFind as FZF
import qualified Unison.Codebase.Branch as Branch
import Unison.NameSegment

type FuzzyFindAPI =
  "find" :> QueryParam "rootBranch" SBH.ShortBranchHash
         :> QueryParam "relativeTo" HashQualifiedName
         :> QueryParam "renderWidth" Width
         :> QueryParam "limit" Int
         :> QueryParam "query" String
         :> Get '[JSON] [(FZF.Alignment, FoundResult)]

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
deriving instance ToSchema FZF.Result
deriving instance ToSchema FZF.ResultSegment

data FoundResult
  = FoundTerm NamedTerm
  | FoundType NamedType
  deriving (Generic, Show)

instance ToJSON FoundResult

deriving instance ToSchema FoundResult

instance ToSample FoundResult where
  toSamples _ = noSamples

serveFuzzyFind
  :: Var v
  => Codebase IO v Ann
  -> Maybe SBH.ShortBranchHash
  -> Maybe HashQualifiedName
  -> Maybe Int
  -> Maybe Width
  -> Maybe String
  -> Handler [(FZF.Alignment, FoundResult)]
serveFuzzyFind codebase mayRoot relativePath limit typeWidth query = do
  rel <-
    fromMaybe mempty
    .   fmap Path.fromPath'
    <$> traverse (parsePath . Text.unpack) relativePath
  hashLength <- liftIO $ Codebase.hashLength codebase
  ea         <- liftIO . runExceptT $ do
    root   <- traverse (Backend.expandShortBranchHash codebase) mayRoot
    branch <- Backend.resolveBranchHash root codebase
 --HQ.unsafeFromText . Backend.unisonRefToText <
    let b0 = Branch.head branch
        alignments =
          take (fromMaybe 10 limit)
            . sortBy (compare `on` (Down . FZF.score . (view _1)))
            $ Backend.fuzzyFind rel branch (fromMaybe "" query)
        ppe = Backend.basicSuffixifiedNames hashLength branch rel
    join <$> traverse (loadEntry ppe b0) alignments
  errFromEither backendError ea
 where
  loadEntry ppe b0 (a, (HQ'.NameOnly . NameSegment) -> n, refs) = traverse
    (\case
      Backend.FoundTermRef r ->
        (\te -> (a, FoundTerm $ Backend.termEntryToNamedTerm ppe typeWidth te))
          <$> Backend.termListEntry codebase b0 r n
      Backend.FoundTypeRef r ->
        (\te -> (a, FoundType $ Backend.typeEntryToNamedType te))
          <$> Backend.typeListEntry codebase r n
    )
    refs
  parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
  errFromEither f = either (throwError . f) pure
