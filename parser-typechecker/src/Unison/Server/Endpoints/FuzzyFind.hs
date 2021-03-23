{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.FuzzyFind where

import           Control.Error                  ( runExceptT )
import           Data.Function                  ( on )
import           Data.Aeson                     ( ToJSON )
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
import qualified Unison.Codebase.Path          as Path
import qualified Unison.HashQualified          as HQ
import           Unison.Parser                  ( Ann )
import qualified Unison.Server.Backend         as Backend
import           Unison.Server.Errors           ( backendError
                                                , badNamespace
                                                )
import           Unison.Server.Types            ( HashQualifiedName
                                                , DefinitionDisplayResults
                                                )
import           Unison.Util.Pretty             ( Width )
import           Unison.Var                     ( Var )
import qualified Unison.Codebase.ShortBranchHash
                                               as SBH
import qualified Data.Text                     as Text
import qualified Text.FuzzyFind as FZF

type FuzzyFindAPI =
  "find" :> QueryParam "rootBranch" SBH.ShortBranchHash
         :> QueryParam "relativeTo" HashQualifiedName
         :> QueryParam "renderWidth" Width
         :> QueryParam "limit" Int
         :> QueryParam "query" String
         :> Get '[JSON] [(FZF.Alignment, DefinitionDisplayResults)]

instance ToSample FZF.Alignment where
  toSamples _ = noSamples

instance ToParam (QueryParam "limit" Int) where
  toParam _ =
    DocQueryParam
      "query"
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

instance ToJSON FZF.Alignment
instance ToJSON FZF.Result
instance ToJSON FZF.ResultSegment

deriving instance ToSchema FZF.Alignment
deriving instance ToSchema FZF.Result
deriving instance ToSchema FZF.ResultSegment

serveFuzzyFind
  :: Var v
  => Codebase IO v Ann
  -> Maybe SBH.ShortBranchHash
  -> Maybe HashQualifiedName
  -> Maybe Width
  -> Maybe Int
  -> Maybe String
  -> Handler [(FZF.Alignment, DefinitionDisplayResults)]
serveFuzzyFind codebase mayRoot relativePath width limit query = do
  rel <- fmap Path.fromPath' <$> traverse (parsePath . Text.unpack) relativePath
  ea  <- liftIO . runExceptT $ do
    root   <- traverse (Backend.expandShortBranchHash codebase) mayRoot
    branch <- Backend.resolveBranchHash root codebase
    let
      alignments =
        take (fromMaybe 10 limit)
          . sortBy (compare `on` (Down . FZF.score . fst))
          . fmap
              (\(a, _, c) ->
                (a, HQ.unsafeFromText . Backend.unisonRefToText <$> c)
              )
          $ Backend.fuzzyFind (fromMaybe mempty rel) branch (fromMaybe "" query)
    traverse
      (traverse $ Backend.prettyDefinitionsBySuffixes rel root width codebase)
      alignments
  errFromEither backendError ea
 where
  parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
  errFromEither f = either (throwError . f) pure
