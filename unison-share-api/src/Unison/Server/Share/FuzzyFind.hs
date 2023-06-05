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
import Data.Aeson
import Data.Char qualified as Char
import Data.List qualified as List
import Data.OpenApi (ToSchema)
import Data.Text qualified as Text
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
import Text.FuzzyFind qualified as FZF
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (BranchHash, CausalHash)
import U.Codebase.Reference qualified as U
import U.Codebase.Referent qualified as U
import U.Codebase.Sqlite.NamedRef qualified as S
import U.Codebase.Sqlite.Operations qualified as SqliteOps
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Sqlite qualified as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Server.Backend (termEntryLabeledDependencies, typeEntryLabeledDependencies)
import Unison.Server.Backend qualified as Backend
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
  "find"
    :> QueryParam "rootBranch" SCH.ShortCausalHash
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
  toJSON (FZF.Alignment {score, result}) =
    object ["score" .= score, "result" .= result]

instance ToJSON FZF.Result where
  toJSON (FZF.Result {segments}) = object ["segments" .= toJSON segments]

instance ToJSON FZF.ResultSegment where
  toJSON = \case
    FZF.Gap s -> object ["tag" .= String "Gap", "contents" .= s]
    FZF.Match s -> object ["tag" .= String "Match", "contents" .= s]

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

instance ToJSON FoundType where
  toJSON (FoundType {bestFoundTypeName, typeDef, namedType}) =
    object
      [ "bestFoundTypeName" .= bestFoundTypeName,
        "typeDef" .= typeDef,
        "namedType" .= namedType
      ]

deriving instance ToSchema FoundType

instance ToJSON FoundTerm where
  toJSON (FoundTerm {bestFoundTermName, namedTerm}) =
    object
      [ "bestFoundTermName" .= bestFoundTermName,
        "namedTerm" .= namedTerm
      ]

deriving instance ToSchema FoundTerm

data FoundResult
  = FoundTermResult FoundTerm
  | FoundTypeResult FoundType
  deriving (Generic, Show)

instance ToJSON FoundResult where
  toJSON = \case
    FoundTermResult ft -> object ["tag" .= String "FoundTermResult", "contents" .= ft]
    FoundTypeResult ft -> object ["tag" .= String "FoundTypeResult", "contents" .= ft]

deriving instance ToSchema FoundResult

instance ToSample FoundResult where
  toSamples _ = noSamples

serveFuzzyFind ::
  Codebase IO Symbol Ann ->
  CausalHash ->
  Path.Path ->
  Maybe Int ->
  Maybe Width ->
  String ->
  Backend.Backend IO [(FZF.Alignment, FoundResult)]
serveFuzzyFind codebase rootCausal perspective mayLimit typeWidth query = do
  (bh, dbTermMatches, dbTypeMatches) <- liftIO . Codebase.runTransaction codebase $ do
    shallowRoot <- Backend.resolveCausalHashV2 (Just rootCausal)
    let bh = V2Causal.valueHash shallowRoot
    (terms, types) <- SqliteOps.fuzzySearchDefinitions bh limit perspectiveText preparedQuery
    pure (bh, terms, types)
  let termMatches = fmap namedRefToNamePairTerm dbTermMatches
  let typeMatches = fmap namedRefToNamePairType dbTypeMatches
  let matchNames = Names.fromTermsAndTypes termMatches typeMatches
  let alignments ::
        ( [ ( FZF.Alignment,
              UnisonName,
              [Backend.FoundRef]
            )
          ]
        )
      alignments = Backend.fuzzyFind matchNames fzfQuery
  lift (join <$> traverse (loadEntry bh) alignments)
  where
    preparedQuery = prepareQuery query
    fzfQuery = Text.unpack $ Text.intercalate " " preparedQuery
    namedRefToNamePairTerm ::
      S.NamedRef
        ( U.Referent,
          Maybe U.ConstructorType
        ) ->
      (Name, Referent)
    namedRefToNamePairTerm (S.NamedRef {reversedSegments, ref = (ref, mayCt)}) =
      let ct = fromMaybe (error "serveFuzzyFind: Required constructor type for constructor but it was null") mayCt
       in (Name.fromReverseSegments (coerce reversedSegments), Cv.referent2to1UsingCT ct ref)
    namedRefToNamePairType :: S.NamedRef U.Reference -> (Name, Reference)
    namedRefToNamePairType (S.NamedRef {reversedSegments, ref}) =
      (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
    perspectiveText :: Text
    perspectiveText = Path.toText perspective
    limit = fromMaybe 10 mayLimit
    loadEntry :: BranchHash -> (FZF.Alignment, Text, [Backend.FoundRef]) -> IO [(FZF.Alignment, FoundResult)]
    loadEntry bh (a, n, refs) = do
      let relativeToBranch = Nothing
      entries <- for refs $
        \case
          Backend.FoundTermRef r -> do
            Left . (r,) <$> Backend.termListEntry codebase relativeToBranch (ExactName (NameSegment n) (Cv.referent1to2 r))
          Backend.FoundTypeRef r ->
            Codebase.runTransaction codebase do
              Right . (r,) <$> Backend.typeListEntry codebase relativeToBranch (ExactName (NameSegment n) r)
      let allLabeledDependencies = foldMap (either (termEntryLabeledDependencies . snd) (typeEntryLabeledDependencies . snd)) entries
      pped <- liftIO . Codebase.runTransaction codebase $ PPED.ppedForReferences bh perspective allLabeledDependencies
      let ppe = PPED.suffixifiedPPE pped
      Codebase.runTransaction codebase do
        for entries \case
          Left (r, termEntry) ->
            pure
              ( a,
                FoundTermResult
                  . FoundTerm
                    (Backend.bestNameForTerm @Symbol ppe (mayDefaultWidth typeWidth) r)
                  $ Backend.termEntryToNamedTerm ppe typeWidth termEntry
              )
          Right (r, typeEntry) -> do
            let namedType = Backend.typeEntryToNamedType typeEntry
            let typeName = Backend.bestNameForType @Symbol ppe (mayDefaultWidth typeWidth) r
            typeHeader <- Backend.typeDeclHeader codebase ppe r
            let ft = FoundType typeName typeHeader namedType
            pure (a, FoundTypeResult ft)

-- | Splits a query into segments, where each segment must appear in order in any matching
-- names.
--
-- >>> prepareQuery "foo bar baz"
-- ["foo","bar","baz"]
--
-- Split camel-case style words into segments.
-- >>> prepareQuery "fMap"
-- ["f","Map"]
--
-- Collapse multiple spaces
-- >>> prepareQuery "foo barBaz    boom"
-- ["foo","bar","Baz","boom"]
--
-- Split namespaces into segments
-- >>> prepareQuery "List.map"
-- ["List","map"]
--
-- Shouldn't get multiple splits for capitalized letters
-- >>> prepareQuery "List.Map"
-- ["List","Map"]
prepareQuery :: String -> [Text]
prepareQuery query = do
  word <- words query
  xs <-
    word
      & map (\c -> if c == '.' then ' ' else c)
      & List.foldl'
        ( \acc next -> case next of
            c
              | Char.isUpper c -> [c] : acc
              | Char.isSpace c -> [] : acc
              | otherwise -> case acc of
                  [] -> [[c]]
                  (last : rest) -> (last ++ [c]) : rest
        )
        []
      & reverse
      & filter (not . null)
  pure $ Text.pack xs
