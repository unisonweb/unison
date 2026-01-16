{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Server.Types where

-- Types common to endpoints --

import Codec.Serialise
import Control.Lens hiding (from, (.=))
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.ByteString.Lazy qualified as LZ
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.OpenApi
  ( OpenApiType (..),
    ToParamSchema (..),
    ToSchema (..),
  )
import Data.OpenApi.Lens qualified as OpenApi
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text
import Servant qualified
import Servant.API
  ( Capture,
    FromHttpApiData (..),
    Get,
    Header,
    Headers,
    JSON,
    QueryParam,
    ToHttpApiData (..),
    addHeader,
  )
import Servant.Docs (DocCapture (..), DocQueryParam (..), ParamKind (..), ToParam)
import Servant.Docs qualified as Docs
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectBranchName)
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude
import Unison.Project (ProjectAndBranch, ProjectName)
import Unison.Server.Doc (Doc)
import Unison.Server.Orphans ()
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Syntax qualified as Syntax
import Unison.ShortHash (ShortHash)
import Unison.Syntax.HashQualified qualified as HQ (parseText)
import Unison.Syntax.Name qualified as Name
import Unison.Util.AnnotatedText (Segment)
import Unison.Util.Pretty (Width (..))

type APIHeaders x =
  Headers
    '[ Header "Cache-Control" String
     ]
    x

type APIGet c = Get '[JSON] (APIHeaders c)

type HashQualifiedName = Text

type NamespaceFQN = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

data NamespaceDetails = NamespaceDetails
  { fqn :: Path.Path,
    hash :: UnisonHash,
    readme :: Maybe Doc
  }
  deriving (Generic, Show)

instance Docs.ToSample NamespaceDetails where
  toSamples _ =
    [ ( "When no value is provided for `namespace`, the root namespace `.` is "
          <> "listed by default",
        NamespaceDetails
          mempty
          "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
          Nothing
      )
    ]

instance ToJSON NamespaceDetails where
  toJSON NamespaceDetails {..} =
    object
      [ "fqn" .= fqn,
        "hash" .= hash,
        "readme" .= readme
      ]

instance FromJSON NamespaceDetails where
  parseJSON = Aeson.withObject "NamespaceDetails" \obj -> do
    fqn <- obj .: "fqn"
    hash <- obj .: "hash"
    readme <- obj .: "readme"
    pure $ NamespaceDetails {..}

deriving instance ToSchema NamespaceDetails

-- | A hash qualified name, unlike HashQualified, the hash is required
data ExactName name ref = ExactName
  { name :: name,
    ref :: ref
  }
  deriving stock (Show, Eq, Functor, Ord)

instance ToParamSchema (ExactName Name ShortHash) where
  toParamSchema _ =
    mempty
      & OpenApi.type_ ?~ OpenApiString
      & OpenApi.example ?~ Aeson.String "base.List"

instance ToParam (QueryParam "exact-name" (ExactName Name ShortHash)) where
  toParam _ =
    DocQueryParam
      "exact-name"
      []
      "The fully qualified name of a namespace with a hash, denoted by a '@'. E.g. base.List.map@abc"
      Normal

instance Docs.ToCapture (Capture "fqn" (ExactName Name ShortHash)) where
  toCapture _ =
    DocCapture
      "fqn"
      "The fully qualified name of a namespace with a hash, denoted by a '@'. E.g. base.List.map@abc"

exactToHQ :: ExactName name ShortHash -> HQ.HashQualified name
exactToHQ (ExactName {name, ref}) = HQ.HashQualified name ref

exactToHQ' :: ExactName name ShortHash -> HQ'.HashQualified name
exactToHQ' (ExactName {name, ref}) = HQ'.HashQualified name ref

instance Bifunctor ExactName where
  bimap l r (ExactName a b) = ExactName (l a) (r b)

instance Bifoldable ExactName where
  bifoldMap l r (ExactName a b) = l a <> r b

instance Bitraversable ExactName where
  bitraverse l r (ExactName a b) = ExactName <$> (l a) <*> (r b)

instance FromHttpApiData (ExactName Name ShortHash) where
  parseQueryParam txt =
    -- # is special in URLs, so we use @ for hash qualification instead;
    -- e.g. ".base.List.map@abc"
    -- e.g. ".base.Nat@@Nat"
    case HQ.parseText (Text.replace "@" "#" txt) of
      Nothing -> Left "Invalid absolute name with Hash"
      Just hq' -> case hq' of
        HQ.NameOnly _ -> Left "A name and hash are required, but only a name was provided"
        HQ.HashOnly _ -> Left "A name and hash are required, but only a hash was provided"
        HQ.HashQualified name ref -> Right $ ExactName {name, ref}

deriving via Bool instance FromHttpApiData Suffixify

deriving anyclass instance ToParamSchema Suffixify

instance ToJSON TypeDefinition where
  toJSON TypeDefinition {..} =
    object
      [ "typeNames" .= typeNames,
        "bestTypeName" .= bestTypeName,
        "defnTypeTag" .= defnTypeTag,
        "typeDefinition" .= typeDefinition,
        "typeDocs" .= typeDocs
      ]

instance FromJSON TypeDefinition where
  parseJSON = Aeson.withObject "TypeDefinition" \obj -> do
    typeNames <- obj .: "typeNames"
    bestTypeName <- obj .: "bestTypeName"
    defnTypeTag <- obj .: "defnTypeTag"
    typeDefinition <- obj .: "typeDefinition"
    typeDocs <- obj .: "typeDocs"
    pure $ TypeDefinition {..}

deriving instance ToSchema TypeDefinition

instance ToJSON TermDefinition where
  toJSON TermDefinition {..} =
    object
      [ "termNames" .= termNames,
        "bestTermName" .= bestTermName,
        "defnTermTag" .= defnTermTag,
        "termDefinition" .= termDefinition,
        "signature" .= signature,
        "termDocs" .= termDocs
      ]

instance FromJSON TermDefinition where
  parseJSON = Aeson.withObject "TermDefinition" \obj -> do
    termNames <- obj .: "termNames"
    bestTermName <- obj .: "bestTermName"
    defnTermTag <- obj .: "defnTermTag"
    termDefinition <- obj .: "termDefinition"
    signature <- obj .: "signature"
    termDocs <- obj .: "termDocs"
    pure $ TermDefinition {..}

deriving instance ToSchema TermDefinition

instance ToJSON DefinitionDisplayResults where
  toJSON DefinitionDisplayResults {..} =
    object
      [ "termDefinitions" .= termDefinitions,
        "typeDefinitions" .= typeDefinitions,
        "missingDefinitions" .= missingDefinitions
      ]

instance FromJSON DefinitionDisplayResults where
  parseJSON = Aeson.withObject "DefinitionDisplayResults" \obj -> do
    termDefinitions <- obj .: "termDefinitions"
    typeDefinitions <- obj .: "typeDefinitions"
    missingDefinitions <- obj .: "missingDefinitions"
    pure $ DefinitionDisplayResults {..}

deriving instance ToSchema DefinitionDisplayResults

data TermDefinitionDiff = TermDefinitionDiff
  { left :: TermDefinition,
    right :: TermDefinition,
    diff :: DisplayObjectDiff
  }
  deriving (Eq, Ord, Show, Generic)

data TypeDefinitionDiff = TypeDefinitionDiff
  { left :: TypeDefinition,
    right :: TypeDefinition,
    diff :: DisplayObjectDiff
  }
  deriving (Eq, Ord, Show, Generic)

newtype Suffixify = Suffixify {suffixified :: Bool}
  deriving (Eq, Ord, Show, Generic)

data TermDefinition = TermDefinition
  { termNames :: [HashQualifiedName],
    bestTermName :: HashQualifiedName,
    defnTermTag :: TermTag,
    termDefinition :: DisplayObject Syntax.SyntaxText Syntax.SyntaxText,
    signature :: Syntax.SyntaxText,
    termDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Ord, Generic)

data TypeDefinition = TypeDefinition
  { typeNames :: [HashQualifiedName],
    bestTypeName :: HashQualifiedName,
    defnTypeTag :: TypeTag,
    typeDefinition :: DisplayObject Syntax.SyntaxText Syntax.SyntaxText,
    typeDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Ord, Generic)

data DefinitionDisplayResults = DefinitionDisplayResults
  { termDefinitions :: Map UnisonHash TermDefinition,
    typeDefinitions :: Map UnisonHash TypeDefinition,
    missingDefinitions :: [HashQualifiedName]
  }
  deriving (Eq, Show, Ord, Generic)

instance Semigroup DefinitionDisplayResults where
  DefinitionDisplayResults terms1 types1 missing1 <> DefinitionDisplayResults terms2 types2 missing2 =
    DefinitionDisplayResults (terms1 `Map.union` terms2) (types1 `Map.union` types2) (missing1 ++ missing2)

instance Monoid DefinitionDisplayResults where
  mempty = DefinitionDisplayResults mempty mempty mempty

data TermTag = Doc | Test | Plain | Constructor TypeTag
  deriving (Eq, Ord, Show, Generic)

data TypeTag = Ability | Data
  deriving (Eq, Ord, Show, Generic)

-- | A type for semantic diffing of definitions.
-- Includes special-cases for when the name in a definition has changed but the hash hasn't
-- (rename/alias), and when the hash has changed but the name hasn't (update propagation).
data SemanticSyntaxDiff a
  = OnlyThisSide (NonEmpty (Segment a))
  | Both (NonEmpty (Segment a))
  | --  (fromSegment, toSegment) (shared annotation)
    SegmentChange (Text, Text) (Maybe a)
  | -- (shared segment) (fromAnnotation, toAnnotation)
    AnnotationChange Text (Maybe a, Maybe a)
  deriving (Eq, Show, Ord, Generic)

deriving instance (ToSchema a) => ToSchema (SemanticSyntaxDiff a)

instance (ToJSON a) => ToJSON (SemanticSyntaxDiff a) where
  toJSON = \case
    OnlyThisSide segments ->
      object
        [ "diffTag" .= ("oneSided" :: Text),
          "elements" .= segments
        ]
    Both segments ->
      object
        [ "diffTag" .= ("both" :: Text),
          "elements" .= segments
        ]
    SegmentChange (fromSegment, toSegment) annotation ->
      object
        [ "diffTag" .= ("segmentChange" :: Text),
          "fromSegment" .= fromSegment,
          "toSegment" .= toSegment,
          "annotation" .= annotation
        ]
    AnnotationChange segment (fromAnnotation, toAnnotation) ->
      object
        [ "diffTag" .= ("annotationChange" :: Text),
          "segment" .= segment,
          "fromAnnotation" .= fromAnnotation,
          "toAnnotation" .= toAnnotation
        ]

instance (FromJSON a) => FromJSON (SemanticSyntaxDiff a) where
  parseJSON = Aeson.withObject "SemanticSyntaxDiff" \obj -> do
    diffTag :: Text <- obj .: "diffTag"
    case diffTag of
      "one-sided" -> OnlyThisSide <$> obj .: "elements"
      "both" -> Both <$> obj .: "elements"
      "segmentChange" -> do
        fromSegment <- obj .: "fromSegment"
        toSegment <- obj .: "toSegment"
        annotation <- obj .: "annotation"
        pure $ SegmentChange (fromSegment, toSegment) annotation
      "annotationChange" -> do
        segment <- obj .: "segment"
        fromAnnotation <- obj .: "fromAnnotation"
        toAnnotation <- obj .: "toAnnotation"
        pure $ AnnotationChange segment (fromAnnotation, toAnnotation)
      _ -> fail "Invalid diffTag"

data Changed a
  = Changed a
  | Unchanged a
  | Spacer
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance (ToJSON a) => ToJSON (Changed a) where
  toJSON = \case
    Changed a ->
      object
        [ "kind" .= ("changed" :: Text),
          "value" .= a
        ]
    Unchanged a ->
      object
        [ "kind" .= ("unchanged" :: Text),
          "value" .= a
        ]
    Spacer ->
      object
        [ "kind" .= ("spacer" :: Text)
        ]

instance (FromJSON a) => FromJSON (Changed a) where
  parseJSON = Aeson.withObject "Changed" \obj -> do
    kind :: Text <- obj .: "kind"
    case kind of
      "changed" -> Changed <$> obj .: "value"
      "unchanged" -> Unchanged <$> obj .: "value"
      "spacer" -> pure Spacer
      _ -> fail "Invalid kind"

deriving instance (ToSchema a) => ToSchema (Changed a)

data LinewiseDiff a = LinewiseDiff
  { lhsLines :: [Changed [a]],
    rhsLines :: [Changed [a]]
  }
  deriving stock (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

deriving instance (ToSchema a) => ToSchema (LinewiseDiff a)

instance (ToJSON a) => ToJSON (LinewiseDiff a) where
  toJSON LinewiseDiff {..} =
    object
      [ "left" .= lhsLines,
        "right" .= rhsLines
      ]

instance (FromJSON a) => FromJSON (LinewiseDiff a) where
  parseJSON = Aeson.withObject "LinewiseDiff" \obj -> do
    lhsLines <- obj .: "left"
    rhsLines <- obj .: "right"
    pure $ LinewiseDiff {..}

-- Diff data can be one-sided or have a counter-part on the other side of the diff.
-- We can use this to represent things like name-changes for the same hash, or hash-changes for the same name.
data Paired a
  = OneSided a
  | Paired a a
  deriving (Eq, Ord, Show)

swapPair :: Paired a -> Paired a
swapPair (OneSided a) = OneSided a
swapPair (Paired a b) = Paired b a

-- | A diff of the syntax of a term or type
--
-- It doesn't make sense to diff builtins with ABTs, so in that case we just provide the
-- undiffed syntax.
data DisplayObjectDiff
  = DisplayObjectDiff (DisplayObject (LinewiseDiff (SemanticSyntaxDiff Syntax.Element)) (LinewiseDiff (SemanticSyntaxDiff Syntax.Element)))
  | MismatchedDisplayObjects (DisplayObject SyntaxText SyntaxText) (DisplayObject SyntaxText SyntaxText)
  deriving stock (Show, Eq, Ord, Generic)

deriving instance ToSchema DisplayObjectDiff

data UnisonRef
  = TypeRef UnisonHash
  | TermRef UnisonHash
  deriving (Eq, Ord, Show, Generic)

unisonRefToText :: UnisonRef -> Text
unisonRefToText = \case
  TypeRef r -> r
  TermRef r -> r

data NamedTerm = NamedTerm
  { -- The name of the term, should be hash qualified if conflicted, otherwise name only.
    termName :: HQ'.HashQualified Name,
    termHash :: ShortHash,
    termType :: Maybe Syntax.SyntaxText,
    termTag :: TermTag
  }
  deriving (Eq, Ord, Generic, Show)

instance ToJSON NamedTerm where
  toJSON (NamedTerm n h typ tag) =
    Aeson.object
      [ "termName" .= HQ'.toTextWith Name.toText n,
        "termHash" .= h,
        "termType" .= typ,
        "termTag" .= tag
      ]

instance FromJSON NamedTerm where
  parseJSON = Aeson.withObject "NamedTerm" \obj -> do
    termName <- obj .: "termName"
    termHash <- obj .: "termHash"
    termType <- obj .: "termType"
    termTag <- obj .: "termTag"
    pure $ NamedTerm {..}

deriving instance ToSchema NamedTerm

data NamedType = NamedType
  { typeName :: HQ'.HashQualified Name,
    typeHash :: ShortHash,
    typeTag :: TypeTag
  }
  deriving (Eq, Ord, Generic, Show)

instance ToJSON NamedType where
  toJSON (NamedType n h tag) =
    Aeson.object
      [ "typeName" .= HQ'.toTextWith Name.toText n,
        "typeHash" .= h,
        "typeTag" .= tag
      ]

instance FromJSON NamedType where
  parseJSON = Aeson.withObject "NamedType" \obj -> do
    typeName <- obj .: "typeName"
    typeHash <- obj .: "typeHash"
    typeTag <- obj .: "typeTag"
    pure $ NamedType {..}

deriving instance ToSchema NamedType

instance ToJSON TermTag where
  toJSON = \case
    Doc -> "Doc"
    Test -> "Test"
    Plain -> "Plain"
    Constructor tt -> case tt of
      Ability -> "AbilityConstructor"
      Data -> "DataConstructor"

instance FromJSON TermTag where
  parseJSON Null = pure Plain
  parseJSON v =
    v
      & Aeson.withText "TermTag" \case
        "Doc" -> pure Doc
        "Test" -> pure Test
        "Plain" -> pure Plain
        "AbilityConstructor" -> pure $ Constructor Ability
        "DataConstructor" -> pure $ Constructor Data
        txt -> fail $ "Invalid TermTag" <> Text.unpack txt

deriving instance ToSchema TermTag

instance ToJSON TypeTag where
  toJSON = \case
    Ability -> "Ability"
    Data -> "Data"

instance FromJSON TypeTag where
  parseJSON = Aeson.withText "TypeTag" \case
    "Ability" -> pure Ability
    "Data" -> pure Data
    txt -> fail $ "Invalid TypeTag" <> Text.unpack txt

deriving instance ToSchema TypeTag

-- Helpers

munge :: Text -> LZ.ByteString
munge = Text.encodeUtf8 . Text.Lazy.fromStrict

mungeShow :: (Show s) => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = Text.encodeUtf8 . Text.Lazy.pack

defaultWidth :: Width
defaultWidth = 80

discard :: (Applicative m) => a -> m ()
discard = const $ pure ()

mayDefaultWidth :: Maybe Width -> Width
mayDefaultWidth = fromMaybe defaultWidth

setCacheControl :: v -> APIHeaders v
setCacheControl = addHeader @"Cache-Control" "public"

branchToUnisonHash :: Branch.Branch m -> UnisonHash
branchToUnisonHash b =
  ("#" <>) . Hash.toBase32HexText . unCausalHash $ Branch.headHash b

v2CausalBranchToUnisonHash :: V2Branch.CausalBranch m -> UnisonHash
v2CausalBranchToUnisonHash b =
  ("#" <>) . Hash.toBase32HexText . unCausalHash $ V2Causal.causalHash b

newtype ProjectBranchNameParam = ProjectBranchNameParam {unProjectBranchNameParam :: ProjectAndBranch ProjectName ProjectBranchName}
  deriving (Eq, Show, Generic)

instance ToParamSchema ProjectBranchNameParam where
  toParamSchema _ =
    mempty
      & OpenApi.type_ ?~ OpenApiString
      & OpenApi.example ?~ Aeson.String "@unison%2Fbase%2Fmain"

-- | Parses URL escaped project and branch names, e.g. `@unison%2Fbase%2Fmain` or `@unison%2Fbase%2F@runarorama%2Fmain`
instance FromHttpApiData ProjectBranchNameParam where
  parseUrlPiece t =
    case tryInto @(ProjectAndBranch ProjectName ProjectBranchName) t of
      Left _ -> Left "Invalid project and branch name"
      Right pab -> Right . ProjectBranchNameParam $ pab

instance ToParam (QueryParam "project-and-branch" (ProjectBranchNameParam)) where
  toParam _ =
    DocQueryParam
      "project_and_branch"
      []
      "The name of a project and branch e.g. `@unison%2Fbase%2Fmain` or `@unison%2Fbase%2F@runarorama%2Fmain`"
      Normal

instance Docs.ToCapture (Capture "project-and-branch" ProjectBranchNameParam) where
  toCapture _ =
    DocCapture
      "project-and-branch"
      "The name of a project and branch e.g. `@unison%2Fbase%2Fmain` or `@unison%2Fbase%2F@runarorama%2Fmain`"

data TermDiffResponse = TermDiffResponse
  { project :: ProjectName,
    oldBranch :: ProjectBranchName,
    newBranch :: ProjectBranchName,
    oldTerm :: TermDefinition,
    newTerm :: TermDefinition,
    diff :: DisplayObjectDiff
  }
  deriving (Eq, Ord, Show, Generic)

deriving instance ToSchema TermDiffResponse

instance Docs.ToSample TermDiffResponse where
  toSamples _ = []

instance ToJSON TermDiffResponse where
  toJSON (TermDiffResponse {diff, project, oldBranch, newBranch, oldTerm, newTerm}) =
    case diff of
      DisplayObjectDiff dispDiff ->
        object
          [ "diff" .= dispDiff,
            "diffKind" .= ("diff" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldTerm" .= oldTerm,
            "newTerm" .= newTerm
          ]
      MismatchedDisplayObjects {} ->
        object
          [ "diffKind" .= ("mismatched" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldTerm" .= oldTerm,
            "newTerm" .= newTerm
          ]

instance FromJSON TermDiffResponse where
  parseJSON = Aeson.withObject "TermDiffResponse" \obj -> do
    diff <- DisplayObjectDiff <$> obj .: "diff"
    diffKind :: Text <- obj .: "diffKind"
    project <- obj .: "project"
    oldBranch <- obj .: "oldBranchRef"
    newBranch <- obj .: "newBranchRef"
    oldTerm <- obj .: "oldTerm"
    newTerm <- obj .: "newTerm"
    case diffKind of
      "diff" -> pure $ TermDiffResponse {..}
      "mismatched" -> pure $ TermDiffResponse {..}
      _ -> fail "Invalid diffKind"

data TypeDiffResponse = TypeDiffResponse
  { project :: ProjectName,
    oldBranch :: ProjectBranchName,
    newBranch :: ProjectBranchName,
    oldType :: TypeDefinition,
    newType :: TypeDefinition,
    diff :: DisplayObjectDiff
  }
  deriving (Eq, Ord, Show, Generic)

deriving instance ToSchema TypeDiffResponse

instance Docs.ToSample TypeDiffResponse where
  toSamples _ = []

instance ToJSON TypeDiffResponse where
  toJSON (TypeDiffResponse {diff, project, oldBranch, newBranch, oldType, newType}) =
    case diff of
      DisplayObjectDiff dispDiff ->
        object
          [ "diff" .= dispDiff,
            "diffKind" .= ("diff" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldType" .= oldType,
            "newType" .= newType
          ]
      MismatchedDisplayObjects {} ->
        object
          [ "diffKind" .= ("mismatched" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldType" .= oldType,
            "newType" .= newType
          ]

instance FromJSON TypeDiffResponse where
  parseJSON = Aeson.withObject "TypeDiffResponse" \obj -> do
    diff <- DisplayObjectDiff <$> obj .: "diff"
    diffKind :: Text <- obj .: "diffKind"
    project <- obj .: "project"
    oldBranch <- obj .: "oldBranchRef"
    newBranch <- obj .: "newBranchRef"
    oldType <- obj .: "oldType"
    newType <- obj .: "newType"
    case diffKind of
      "diff" -> pure $ TypeDiffResponse {..}
      "mismatched" -> pure $ TypeDiffResponse {..}
      _ -> fail "Invalid diffKind"

-- | Servant utility for a query param that's required, providing a useful error message if it's missing.
type RequiredQueryParam = Servant.QueryParam' '[Servant.Required, Servant.Strict]

data DefinitionNameSearchResult = DefinitionNameSearchResult
  { token :: Name,
    tag :: TermOrTypeTag
  }

instance ToJSON DefinitionNameSearchResult where
  toJSON DefinitionNameSearchResult {..} =
    Aeson.object
      [ "token" .= token,
        "tag" .= tag
      ]

instance FromJSON DefinitionNameSearchResult where
  parseJSON = Aeson.withObject "DefinitionNameSearchResult" $ \o -> do
    token <- o Aeson..: "token"
    tag <- o Aeson..: "tag"
    pure DefinitionNameSearchResult {token, tag}

newtype DefinitionSearchResults = DefinitionSearchResults
  { results :: [DefinitionSearchResult]
  }
  deriving (Show, Eq, Generic)

instance ToJSON DefinitionSearchResults where
  toJSON DefinitionSearchResults {..} =
    Aeson.object
      [ "results" .= results
      ]

instance FromJSON DefinitionSearchResults where
  parseJSON = Aeson.withObject "DefinitionSearchResults" $ \o -> do
    results <- o Aeson..: "results"
    pure DefinitionSearchResults {results}

instance Docs.ToSample DefinitionSearchResults where
  toSamples _ = Docs.noSamples

deriving anyclass instance ToSchema DefinitionSearchResults

data DefinitionSearchResult = DefinitionSearchResult
  { fqn :: Name,
    summary :: TermOrTypeSummary,
    project :: ProjectName,
    branchRef :: ProjectBranchName
  }
  deriving (Show, Eq, Generic)

deriving instance ToSchema DefinitionSearchResult

instance ToJSON DefinitionSearchResult where
  toJSON DefinitionSearchResult {..} =
    Aeson.object
      [ "fqn" Aeson..= fqn,
        "projectRef" Aeson..= project,
        "branchRef" Aeson..= branchRef,
        "kind" Aeson..= kind,
        "definition" Aeson..= definition
      ]
    where
      (kind, definition) = case summary of
        ToTTermSummary TermSummary {displayName, hash, summary, tag} ->
          ( Aeson.String "term",
            Aeson.object
              [ "displayName" Aeson..= displayName,
                "hash" Aeson..= hash,
                "summary" Aeson..= summary,
                "tag" Aeson..= tag
              ]
          )
        ToTTypeSummary TypeSummary {displayName, hash, summary, tag} ->
          ( Aeson.String "type",
            Aeson.object
              [ "displayName" Aeson..= displayName,
                "hash" Aeson..= hash,
                "summary" Aeson..= summary,
                "tag" Aeson..= tag
              ]
          )

instance FromJSON DefinitionSearchResult where
  parseJSON = Aeson.withObject "DefinitionSearchResult" $ \o -> do
    fqn <- o Aeson..: "fqn"
    project <- o Aeson..: "projectRef"
    branchRef <- o Aeson..: "branchRef"
    kind <- o Aeson..: "kind"
    definition <- o Aeson..: "definition"
    summary <- case kind of
      Aeson.String "term" -> do
        definitionObj <- case definition of
          Aeson.Object obj -> pure obj
          _ -> fail "Expected object for term definition"
        displayName <- definitionObj Aeson..: "displayName"
        hash <- definitionObj Aeson..: "hash"
        summaryText <- definitionObj Aeson..: "summary"
        tag <- definitionObj Aeson..: "tag"
        pure $ ToTTermSummary $ TermSummary {displayName, hash, summary = summaryText, tag}
      Aeson.String "type" -> do
        definitionObj <- case definition of
          Aeson.Object obj -> pure obj
          _ -> fail "Expected object for type definition"
        displayName <- definitionObj Aeson..: "displayName"
        hash <- definitionObj Aeson..: "hash"
        summaryText <- definitionObj Aeson..: "summary"
        tag <- definitionObj Aeson..: "tag"
        pure $ ToTTypeSummary $ TypeSummary {displayName, hash, summary = summaryText, tag}
      _ -> fail "Invalid definition kind"
    pure DefinitionSearchResult {fqn, summary, project, branchRef}

instance Docs.ToSample TermSummary where
  toSamples _ = Docs.noSamples

data TermSummary = TermSummary
  { displayName :: HQ.HashQualified Name,
    hash :: ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TermTag
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON TermSummary where
  toJSON (TermSummary {..}) =
    object
      [ "displayName" .= displayName,
        "hash" .= hash,
        "summary" .= summary,
        "tag" .= tag
      ]

deriving instance ToSchema TermSummary

instance Docs.ToSample TypeSummary where
  toSamples _ = Docs.noSamples

data TypeSummary = TypeSummary
  { displayName :: HQ.HashQualified Name,
    hash :: ShortHash,
    summary :: DisplayObject SyntaxText SyntaxText,
    tag :: TypeTag
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON TypeSummary where
  toJSON (TypeSummary {..}) =
    object
      [ "displayName" .= displayName,
        "hash" .= hash,
        "summary" .= summary,
        "tag" .= tag
      ]

deriving instance ToSchema TypeSummary

data TermOrTypeSummary = ToTTermSummary TermSummary | ToTTypeSummary TypeSummary
  deriving (Show, Eq, Ord, Generic)

deriving instance ToSchema TermOrTypeSummary

instance ToJSON TermOrTypeSummary where
  toJSON (ToTTermSummary ts) = object ["kind" .= ("term" :: Text), "payload" .= ts]
  toJSON (ToTTypeSummary ts) = object ["kind" .= ("type" :: Text), "payload" .= ts]

instance FromJSON TermOrTypeSummary where
  parseJSON = withObject "TermOrTypeSummary" $ \o -> do
    kind :: Text <- o .: "kind"
    case kind of
      "term" -> do
        ts <- o .: "payload"
        ts & withObject "TermSummary" \o -> do
          displayName <- o .: "displayName"
          hash <- o .: "hash"
          summary <- o .: "summary"
          tag <- o .: "tag"
          pure $ ToTTermSummary $ TermSummary {..}
      "type" -> do
        ts <- o .: "payload"
        ts & withObject "TypeSummary" \o -> do
          displayName <- o .: "displayName"
          hash <- o .: "hash"
          summary <- o .: "summary"
          tag <- o .: "tag"
          pure $ ToTTypeSummary $ TypeSummary {..}
      _ -> fail $ "Invalid kind: " <> Text.unpack kind

data TermOrTypeTag = ToTTermTag TermTag | ToTTypeTag TypeTag
  deriving stock (Show, Eq, Ord)

instance FromHttpApiData TermOrTypeTag where
  parseQueryParam = \case
    "doc" -> Right $ ToTTermTag Doc
    "test" -> Right $ ToTTermTag Test
    "plain" -> Right $ ToTTermTag Plain
    "data-constructor" -> Right $ ToTTermTag $ Constructor Data
    "ability-constructor" -> Right $ ToTTermTag $ Constructor Ability
    "data" -> Right $ ToTTypeTag Data
    "ability" -> Right $ ToTTypeTag Ability
    _ -> Left "Invalid TermOrTypeTag"

instance ToHttpApiData TermOrTypeTag where
  toQueryParam = \case
    ToTTermTag Doc -> "doc"
    ToTTermTag Test -> "test"
    ToTTermTag Plain -> "plain"
    ToTTermTag (Constructor Data) -> "data-constructor"
    ToTTermTag (Constructor Ability) -> "ability-constructor"
    ToTTypeTag Data -> "data"
    ToTTypeTag Ability -> "ability"

instance ToJSON TermOrTypeTag where
  toJSON = String . toQueryParam

instance FromJSON TermOrTypeTag where
  parseJSON = withText "TermOrTypeTag" $ \txt ->
    case parseQueryParam txt of
      Left err -> fail $ Text.unpack err
      Right tag -> pure tag

newtype BranchRef = BranchRef {unBranchRef :: Text}
  deriving (Serialise, Eq, Show, Ord, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData) via Text

instance From (ProjectAndBranch ProjectName ProjectBranchName) BranchRef where
  from pab = BranchRef $ from pab
