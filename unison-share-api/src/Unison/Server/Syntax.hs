{-# OPTIONS_GHC -Wno-orphans #-}

-- | Duplicate of the Unison.Util.SyntaxText module, but we expect these to
-- evolve separately. This is the version which is outward facing
-- to the server frontend.
module Unison.Server.Syntax where

import Data.Aeson
import Data.List qualified as List
import Data.List.Extra
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.OpenApi (ToSchema (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Lucid
import Lucid qualified as L
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Pattern (SeqOp (..))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualified qualified as HashQualified (toText)
import Unison.Syntax.Name qualified as Name (unsafeParseText)
import Unison.Syntax.NameSegment qualified as NameSegment (toEscapedText)
import Unison.Util.AnnotatedText
  ( AnnotatedText (..),
    Segment (..),
    annotate,
    segment,
  )
import Unison.Util.SyntaxText qualified as SyntaxText

type SyntaxText = AnnotatedText Element

type SyntaxSegment = Segment Element

instance (ToJSON a) => ToJSON (Segment a) where
  toJSON (Segment {segment, annotation}) = object ["segment" .= segment, "annotation" .= annotation]

instance (FromJSON a) => FromJSON (Segment a) where
  parseJSON = withObject "Segment" $ \o ->
    Segment <$> o .: "segment" <*> o .: "annotation"

deriving instance (ToSchema a) => ToSchema (Segment a)

instance ToJSON SeqOp where
  toJSON = \case
    Cons -> String "Cons"
    Snoc -> String "Snoc"
    Concat -> String "Concat"

instance FromJSON SeqOp where
  parseJSON = withText "SeqOp" $ \case
    "Cons" -> pure Cons
    "Snoc" -> pure Snoc
    "Concat" -> pure Concat
    _ -> fail "Expected one of Cons, Snoc, Concat"

deriving instance ToSchema SeqOp

deriving newtype instance ToJSON (AnnotatedText Element)

deriving newtype instance FromJSON (AnnotatedText Element)

deriving anyclass instance ToSchema SyntaxText

instance (ToSchema r) => ToSchema (Seq r) where
  declareNamedSchema _ = declareNamedSchema (Proxy @[r])

convertElement :: SyntaxText.Element Reference -> Element
convertElement = \case
  SyntaxText.NumericLiteral -> NumericLiteral
  SyntaxText.TextLiteral -> TextLiteral
  SyntaxText.BytesLiteral -> BytesLiteral
  SyntaxText.CharLiteral -> CharLiteral
  SyntaxText.BooleanLiteral -> BooleanLiteral
  SyntaxText.Blank -> Blank
  SyntaxText.Var -> Var
  SyntaxText.TermReference r -> TermReference $ Referent.toText r
  SyntaxText.TypeReference r -> TypeReference $ Reference.toText r
  SyntaxText.Op s -> Op s
  SyntaxText.AbilityBraces -> AbilityBraces
  SyntaxText.ControlKeyword -> ControlKeyword
  SyntaxText.TypeOperator -> TypeOperator
  SyntaxText.BindingEquals -> BindingEquals
  SyntaxText.TypeAscriptionColon -> TypeAscriptionColon
  SyntaxText.DataTypeKeyword -> DataTypeKeyword
  SyntaxText.DataTypeParams -> DataTypeParams
  SyntaxText.Unit -> Unit
  SyntaxText.DataTypeModifier -> DataTypeModifier
  SyntaxText.UseKeyword -> UseKeyword
  SyntaxText.UsePrefix -> UsePrefix
  SyntaxText.UseSuffix -> UseSuffix
  SyntaxText.HashQualifier n -> HashQualifier (HashQualified.toText n)
  SyntaxText.DelayForceChar -> DelayForceChar
  SyntaxText.DelimiterChar -> DelimiterChar
  SyntaxText.Parenthesis -> Parenthesis
  SyntaxText.LinkKeyword -> LinkKeyword
  SyntaxText.DocDelimiter -> DocDelimiter
  SyntaxText.DocKeyword -> DocKeyword

type UnisonHash = Text

type HashQualifiedName = Text

-- | The elements of the Unison grammar, for syntax highlighting purposes
data Element
  = NumericLiteral
  | TextLiteral
  | BytesLiteral
  | CharLiteral
  | BooleanLiteral
  | Blank
  | Var
  | TypeReference UnisonHash
  | DataConstructorReference UnisonHash
  | AbilityConstructorReference UnisonHash
  | TermReference UnisonHash
  | Op SeqOp
  | -- | Constructor Are these even used?
    -- | Request
    AbilityBraces
  | -- let|handle|in|where|match|with|cases|->|if|then|else|and|or
    ControlKeyword
  | -- forall|->
    TypeOperator
  | BindingEquals
  | TypeAscriptionColon
  | -- type|ability
    DataTypeKeyword
  | DataTypeParams
  | Unit
  | -- unique
    DataTypeModifier
  | -- `use Foo bar` is keyword, prefix, suffix
    UseKeyword
  | UsePrefix
  | UseSuffix
  | HashQualifier HashQualifiedName
  | DelayForceChar
  | -- ? , ` [ ] @ |
    -- Currently not all commas in the pretty-print output are marked up as DelimiterChar - we miss
    -- out characters emitted by Pretty.hs helpers like Pretty.commas.
    DelimiterChar
  | -- ! '
    Parenthesis
  | LinkKeyword -- `typeLink` and `termLink`
  -- [: :] @[]
  | DocDelimiter
  | -- the 'include' in @[include], etc
    DocKeyword
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Element where
  toJSON = \case
    NumericLiteral -> object ["tag" .= String "NumericLiteral"]
    TextLiteral -> object ["tag" .= String "TextLiteral"]
    BytesLiteral -> object ["tag" .= String "BytesLiteral"]
    CharLiteral -> object ["tag" .= String "CharLiteral"]
    BooleanLiteral -> object ["tag" .= String "BooleanLiteral"]
    Blank -> object ["tag" .= String "Blank"]
    Var -> object ["tag" .= String "Var"]
    TypeReference r -> object ["tag" .= String "TypeReference", "contents" .= r]
    DataConstructorReference r ->
      object ["tag" .= String "DataConstructorReference", "contents" .= r]
    AbilityConstructorReference r -> object ["tag" .= String "AbilityConstructorReference", "contents" .= r]
    TermReference r -> object ["tag" .= String "TermReference", "contents" .= r]
    Op s -> object ["tag" .= String "Op", "contents" .= s]
    AbilityBraces -> object ["tag" .= String "AbilityBraces"]
    ControlKeyword -> object ["tag" .= String "ControlKeyword"]
    TypeOperator -> object ["tag" .= String "TypeOperator"]
    BindingEquals -> object ["tag" .= String "BindingEquals"]
    TypeAscriptionColon -> object ["tag" .= String "TypeAscriptionColon"]
    DataTypeKeyword -> object ["tag" .= String "DataTypeKeyword"]
    DataTypeParams -> object ["tag" .= String "DataTypeParams"]
    Unit -> object ["tag" .= String "Unit"]
    DataTypeModifier -> object ["tag" .= String "DataTypeModifier"]
    UseKeyword -> object ["tag" .= String "UseKeyword"]
    UsePrefix -> object ["tag" .= String "UsePrefix"]
    UseSuffix -> object ["tag" .= String "UseSuffix"]
    HashQualifier n -> object ["tag" .= String "HashQualifier", "contents" .= n]
    DelayForceChar -> object ["tag" .= String "DelayForceChar"]
    DelimiterChar -> object ["tag" .= String "DelimiterChar"]
    Parenthesis -> object ["tag" .= String "Parenthesis"]
    LinkKeyword -> object ["tag" .= String "LinkKeyword"]
    DocDelimiter -> object ["tag" .= String "DocDelimiter"]
    DocKeyword -> object ["tag" .= String "DocKeyword"]

instance FromJSON Element where
  parseJSON = withObject "Element" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      "NumericLiteral" -> pure NumericLiteral
      "TextLiteral" -> pure TextLiteral
      "BytesLiteral" -> pure BytesLiteral
      "CharLiteral" -> pure CharLiteral
      "BooleanLiteral" -> pure BooleanLiteral
      "Blank" -> pure Blank
      "Var" -> pure Var
      "TypeReference" -> TypeReference <$> obj .: "contents"
      "DataConstructorReference" -> DataConstructorReference <$> obj .: "contents"
      "AbilityConstructorReference" -> AbilityConstructorReference <$> obj .: "contents"
      "TermReference" -> TermReference <$> obj .: "contents"
      "Op" -> Op <$> obj .: "contents"
      "AbilityBraces" -> pure AbilityBraces
      "ControlKeyword" -> pure ControlKeyword
      "TypeOperator" -> pure TypeOperator
      "BindingEquals" -> pure BindingEquals
      "TypeAscriptionColon" -> pure TypeAscriptionColon
      "DataTypeKeyword" -> pure DataTypeKeyword
      "DataTypeParams" -> pure DataTypeParams
      "Unit" -> pure Unit
      "DataTypeModifier" -> pure DataTypeModifier
      "UseKeyword" -> pure UseKeyword
      "UsePrefix" -> pure UsePrefix
      "UseSuffix" -> pure UseSuffix
      "HashQualifier" -> HashQualifier <$> obj .: "contents"
      "DelayForceChar" -> pure DelayForceChar
      "DelimiterChar" -> pure DelimiterChar
      "Parenthesis" -> pure Parenthesis
      "LinkKeyword" -> pure LinkKeyword
      "DocDelimiter" -> pure DocDelimiter
      "DocKeyword" -> pure DocKeyword
      _ -> fail $ "Unknown tag: " <> tag

deriving instance ToSchema Element

syntax :: Element -> SyntaxText -> SyntaxText
syntax = annotate

firstReference :: SyntaxText -> Maybe UnisonHash
firstReference (AnnotatedText segments) =
  firstJust reference (toList segments)

reference :: SyntaxSegment -> Maybe UnisonHash
reference (Segment _ el) =
  let reference' el' =
        case el' of
          TermReference r -> Just r
          TypeReference r -> Just r
          HashQualifier r -> Just r
          _ -> Nothing
   in el >>= reference'

-- | Convert a `SyntaxText` to a `String`, ignoring syntax markup
toPlain :: SyntaxText -> String
toPlain (AnnotatedText at) = join (toList $ segment <$> at)

toPlainText :: SyntaxText -> Text
toPlainText = Text.pack . toPlain

-- HTML -----------------------------------------------------------------------

toHtml :: SyntaxText -> Html ()
toHtml (AnnotatedText segments) =
  let renderedSegments =
        fmap segmentToHtml segments
   in span_ [class_ "syntax"] $ sequence_ (toList renderedSegments)

nameToHtml :: Name -> Html ()
nameToHtml name =
  span_ [class_ "fqn"] $ sequence_ parts
  where
    segments =
      map (segment . L.toHtml . NameSegment.toEscapedText) $ List.NonEmpty.toList $ Name.segments name

    segment =
      span_ [class_ "segment"]

    sep =
      span_ [class_ "sep "] "."

    parts =
      List.intersperse sep segments

segmentToHtml :: SyntaxSegment -> Html ()
segmentToHtml (Segment segmentText element) =
  let sText = Text.pack segmentText

      el = fromMaybe Blank element

      ref =
        case el of
          TypeReference h ->
            Just (h, "type")
          TermReference h ->
            Just (h, "term")
          AbilityConstructorReference h ->
            Just (h, "ability-constructor")
          DataConstructorReference h ->
            Just (h, "data-constructor")
          _ ->
            Nothing

      isFQN =
        let isFQN_ =
              Text.isInfixOf "." sText && not (Text.isInfixOf "#" sText)
         in case el of
              TypeReference {} ->
                isFQN_
              TermReference {} ->
                isFQN_
              HashQualifier {} ->
                isFQN_
              DataConstructorReference {} ->
                isFQN_
              AbilityConstructorReference {} ->
                isFQN_
              _ ->
                False

      className =
        elementToClassName el

      content
        | Text.isInfixOf "->" sText = span_ [class_ "arrow"] $ L.toHtml sText
        | isFQN = nameToHtml (Name.unsafeParseText sText)
        | otherwise = L.toHtml sText
   in case ref of
        Just (r, refType) ->
          span_ [class_ className, data_ "ref" r, data_ "ref-type" refType] content
        _ ->
          span_ [class_ className] content

elementToClassName :: Element -> Text
elementToClassName el =
  case el of
    NumericLiteral ->
      "numeric-literal"
    TextLiteral ->
      "text-literal"
    BytesLiteral ->
      "bytes-literal"
    CharLiteral ->
      "char-literal"
    BooleanLiteral ->
      "boolean-literal"
    Blank ->
      "blank"
    Var ->
      "var"
    TypeReference {} ->
      "type-reference"
    TermReference {} ->
      "term-reference"
    DataConstructorReference {} ->
      "data-constructor-reference"
    AbilityConstructorReference {} ->
      "ability-constructor-reference"
    Op seqOp ->
      case seqOp of
        Cons ->
          "op cons"
        Snoc ->
          "op snoc"
        Concat ->
          "op concat"
    AbilityBraces ->
      "ability-braces"
    ControlKeyword ->
      "control-keyword"
    TypeOperator ->
      "type-operator"
    BindingEquals ->
      "binding-equals"
    TypeAscriptionColon ->
      "type-ascription-colon"
    DataTypeKeyword -> "data-type-keyword"
    DataTypeParams ->
      "data-type-params"
    Unit ->
      "unit"
    DataTypeModifier ->
      "data-type-modifier"
    UseKeyword ->
      "use-keyword"
    UsePrefix ->
      "use-prefix"
    UseSuffix ->
      "use-suffix"
    HashQualifier {} ->
      "hash-qualifier"
    DelayForceChar ->
      "delay-force-char"
    DelimiterChar ->
      "delimeter-char"
    Parenthesis ->
      "parenthesis"
    LinkKeyword ->
      "link-keyword"
    DocDelimiter ->
      "doc-delimeter"
    DocKeyword ->
      "doc-keyword"
