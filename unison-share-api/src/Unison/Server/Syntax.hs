{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Duplicate of the Unison.Util.SyntaxText module, but we expect these to
-- evolve separately. This is the version which is outward facing
-- to the server frontend.
module Unison.Server.Syntax where

import Data.Aeson (FromJSON, ToJSON (..))
import qualified Data.List as List
import Data.List.Extra
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.OpenApi (ToSchema (..))
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Lucid
import qualified Lucid as L
import qualified Unison.HashQualified as HashQualified
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.NameSegment as NameSegment
import Unison.Pattern (SeqOp (..))
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Util.AnnotatedText
  ( AnnotatedText (..),
    Segment (..),
    annotate,
    segment,
  )
import qualified Unison.Util.SyntaxText as SyntaxText

type SyntaxText = AnnotatedText Element

type SyntaxSegment = Segment Element

instance ToJSON Element

instance FromJSON Element

deriving instance ToSchema Element

instance ToJSON a => ToJSON (Segment a)

instance FromJSON a => FromJSON (Segment a)

deriving instance ToSchema a => ToSchema (Segment a)

instance ToJSON SeqOp

instance FromJSON SeqOp

deriving instance ToSchema SeqOp

instance ToJSON SyntaxText

instance FromJSON SyntaxText

deriving anyclass instance ToSchema SyntaxText

instance ToSchema r => ToSchema (Seq r) where
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
      map (segment . L.toHtml . NameSegment.toText) $ List.NonEmpty.toList $ Name.segments name

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
        | isFQN = nameToHtml (Name.unsafeFromText sText)
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
