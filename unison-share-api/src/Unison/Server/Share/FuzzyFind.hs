{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Share.FuzzyFind where

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
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (BranchHash, CausalHash)
import U.Codebase.Sqlite.NameLookups (PathSegments (..))
import U.Codebase.Sqlite.NameLookups qualified as NameLookups
import U.Codebase.Sqlite.NamedRef qualified as S
import U.Codebase.Sqlite.Operations qualified as SqliteOps
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Debug qualified as Debug
import Unison.NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Sqlite qualified as PPED
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
    :> APIGet [(Alignment, FoundResult)]

instance ToSample Alignment where
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
  -- | Whether the root is a scratch root
  Bool ->
  Codebase IO Symbol Ann ->
  CausalHash ->
  Path.Path ->
  Maybe Int ->
  Maybe Width ->
  Text ->
  Backend.Backend IO [(Alignment, FoundResult)]
serveFuzzyFind isInScratch codebase rootCausal perspective mayLimit typeWidth query = do
  (scratchRootSearch, bh, namesPerspective, dbTermMatches, dbTypeMatches) <- liftIO . Codebase.runTransaction codebase $ do
    shallowRoot <- Backend.resolveCausalHashV2 (Just rootCausal)
    let bh = V2Causal.valueHash shallowRoot
    namesPerspective@SqliteOps.NamesPerspective {pathToMountedNameLookup = PathSegments pathToPerspective} <- SqliteOps.namesPerspectiveForRootAndPath bh (coerce $ Path.toList perspective)
    -- If were browsing at a scratch root we need to include one level of dependencies'
    -- since the projects are "dependencies" of the scratch root.
    let scratchRootSearch = isInScratch && null pathToPerspective
    (terms, types) <- SqliteOps.fuzzySearchDefinitions scratchRootSearch namesPerspective limit preparedQuery
    pure (scratchRootSearch, bh, namesPerspective, terms, types)
  let prepareMatch :: S.NamedRef Backend.FoundRef -> (PathSegments, Alignment, UnisonName, [Backend.FoundRef])
      prepareMatch name@(S.NamedRef {S.reversedSegments}) =
        let renderedName = NameLookups.reversedNameToNamespaceText reversedSegments
            segments = computeMatchSegments preparedQuery name
            alignment = Alignment {score = scoreMatch name, result = MatchResult {segments}}
         in (NameLookups.reversedNameToPathSegments reversedSegments, alignment, renderedName, [S.ref name])
  let preparedTerms :: [(PathSegments, Alignment, UnisonName, [Backend.FoundRef])]
      preparedTerms =
        dbTermMatches
          <&> \match ->
            match
              & fmap (\(ref, ct) -> Backend.FoundTermRef $ Cv.referent2to1UsingCT (fromMaybe (error "serveFuzzyFind: CT required but not found") ct) ref)
              & prepareMatch
  let preparedTypes :: [(PathSegments, Alignment, UnisonName, [Backend.FoundRef])]
      preparedTypes = prepareMatch . fmap (Backend.FoundTypeRef . Cv.reference2to1) <$> dbTypeMatches
  let alignments ::
        ( [ ( PathSegments,
              Alignment,
              UnisonName,
              [Backend.FoundRef]
            )
          ]
        )
      alignments =
        (preparedTerms <> preparedTypes)
          & List.sortOn (\(_, Alignment {score}, _, _) -> score)
  lift (join <$> traverse (loadEntry scratchRootSearch bh namesPerspective) alignments)
  where
    preparedQuery = prepareQuery (Text.unpack query)
    limit = fromMaybe 10 mayLimit
    loadEntry :: Bool -> BranchHash -> SqliteOps.NamesPerspective -> (PathSegments, Alignment, Text, [Backend.FoundRef]) -> IO [(Alignment, FoundResult)]
    loadEntry scratchRootSearch bh searchPerspective (pathToMatch, a, n, refs) = do
      Debug.debugM Debug.Server "Search match" pathToMatch
      namesPerspective <-
        -- If we're searching from a scratch root, render each match within its project ppe.
        if scratchRootSearch
          then Codebase.runTransaction codebase $ SqliteOps.namesPerspectiveForRootAndPath bh (coerce (Path.toList perspective) <> pathToMatch)
          else pure searchPerspective
      let relativeToBranch = Nothing
      entries <- for refs $
        \case
          Backend.FoundTermRef r -> do
            Left . (r,) <$> Backend.termListEntry codebase relativeToBranch (ExactName (NameSegment n) (Cv.referent1to2 r))
          Backend.FoundTypeRef r ->
            Codebase.runTransaction codebase do
              Right . (r,) <$> Backend.typeListEntry codebase relativeToBranch (ExactName (NameSegment n) r)
      let allLabeledDependencies = foldMap (either (termEntryLabeledDependencies . snd) (typeEntryLabeledDependencies . snd)) entries
      pped <- liftIO . Codebase.runTransaction codebase $ PPED.ppedForReferences namesPerspective allLabeledDependencies
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

-- Scores a matched name by the number of segments.
-- Lower is better.
scoreMatch :: S.NamedRef r -> Int
scoreMatch S.NamedRef {S.reversedSegments = NameLookups.ReversedName segments} = length segments

data Alignment = Alignment
  { score :: Int,
    result :: MatchResult
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data MatchResult = MatchResult
  { segments :: [MatchSegment]
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data MatchSegment
  = Gap Text
  | Match Text
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance ToJSON Alignment where
  toJSON (Alignment {score, result}) =
    object ["score" .= score, "result" .= result]

instance ToJSON MatchResult where
  toJSON (MatchResult {segments}) = object ["segments" .= toJSON segments]

instance ToJSON MatchSegment where
  toJSON = \case
    Gap s -> object ["tag" .= String "Gap", "contents" .= s]
    Match s -> object ["tag" .= String "Match", "contents" .= s]

-- After finding a search results with fuzzy find we do some post processing to
-- refine the result:
--  * Sort:
--      we sort both on the FZF score and the number of segments in the FQN
--      preferring shorter FQNs over longer. This helps with things like forks
--      of base.
--  * Dedupe:
--      we dedupe on the found refs to avoid having several rows of a
--      definition with different names in the result set.
--
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> computeMatchSegments [] (S.NamedRef (NameLookups.ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- >>> computeMatchSegments ["foo", "baz"] (S.NamedRef (NameLookups.ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- >>> computeMatchSegments ["Li", "Ma"] (S.NamedRef (NameLookups.ReversedName ("foldMap" NonEmpty.:| ["List", "data"])) ())
computeMatchSegments ::
  [Text] ->
  (S.NamedRef r) ->
  [MatchSegment]
computeMatchSegments query (S.NamedRef {reversedSegments}) =
  let nameText = NameLookups.reversedNameToNamespaceText reversedSegments
      -- This will be a list of _lower-cased_ match segments, but we need to reclaim the
      -- casing from the actual name.
      matchSegmentShape = List.unfoldr go (filter (not . Text.null) . map Text.toLower $ query, Text.toLower nameText)
   in List.unfoldr go2 (matchSegmentShape, nameText)
  where
    go2 :: ([MatchSegment], Text) -> Maybe (MatchSegment, ([MatchSegment], Text))
    go2 = \case
      ([], _) -> Nothing
      (Gap gap : restShape, name) ->
        let (actualGap, restName) = Text.splitAt (Text.length gap) name
         in Just (Gap actualGap, (restShape, restName))
      (Match match : restShape, name) ->
        let (actualMatch, restName) = Text.splitAt (Text.length match) name
         in Just (Match actualMatch, (restShape, restName))
    go :: ([Text], Text) -> Maybe (MatchSegment, ([Text], Text))
    go = \case
      (_, "") -> Nothing
      ([], rest) -> Just (Gap rest, ([], ""))
      (q : qs, name) ->
        Text.breakOn q name
          & \case
            ("", rest) ->
              case Text.stripPrefix q rest of
                Nothing -> Nothing
                Just remainder ->
                  Just (Match q, (qs, remainder))
            (gap, rest) ->
              Just (Gap gap, (q : qs, rest))

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
-- Split namespaces into segments with a required dot in between.
-- >>> prepareQuery "List.map"
-- ["List",".","map"]
--
-- Shouldn't get multiple splits for capitalized letters
-- >>> prepareQuery "List.Map"
-- ["List",".","Map"]
prepareQuery :: String -> [Text]
prepareQuery query = do
  word <- words query
  xs <-
    word
      & List.foldl'
        ( \acc next -> case next of
            c
              | Char.isUpper c -> [c] : acc
              | Char.isSpace c -> "" : acc
              | c == '.' -> "" : "." : acc
              | otherwise -> case acc of
                  [] -> [[c]]
                  (last : rest) -> (last ++ [c]) : rest
        )
        []
      & reverse
      & filter (not . null)
  pure $ Text.pack xs
