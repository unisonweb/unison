{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Duplicate of the Unison.Util.SyntaxText module, but we expect these to
-- evolve separately. This is the version which is outward facing
-- to the server frontend.
module Unison.Server.Syntax where

import Data.Aeson (ToJSON)
import qualified Data.List as List
import Data.OpenApi (ToSchema (..))
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Unison.HashQualified as HashQualified
import Unison.Name (Name)
import qualified Data.Foldable as Foldable
import qualified Unison.Name as Name
import qualified Unison.NameSegment as NameSegment
import Unison.Pattern (SeqOp (..))
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Server.Html (Html, span_, text)
import Unison.Server.Html.Attribute (class_, data_)
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

deriving instance ToSchema Element

instance ToJSON a => ToJSON (Segment a)

deriving instance ToSchema a => ToSchema (Segment a)

instance ToJSON SeqOp

deriving instance ToSchema SeqOp

instance ToJSON SyntaxText

deriving instance ToSchema SyntaxText

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
  SyntaxText.Referent r -> TermReference $ Referent.toText r
  SyntaxText.Reference r -> TypeReference $ Reference.toText r
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

-- The elements of the Unison grammar, for syntax highlighting purposes
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

foldl :: (b -> SyntaxSegment -> b) -> b -> SyntaxText -> b
foldl f init (AnnotatedText segments) =
  Foldable.foldl' f init segments

reference :: SyntaxSegment -> Maybe UnisonHash
reference (Segment _ el) =
  let reference' el' =
        case el' of
          TermReference r -> Just r
          TypeReference r -> Just r
          _ -> Nothing
   in el >>= reference'

-- Convert a `SyntaxText` to a `String`, ignoring syntax markup
toPlain :: SyntaxText -> String
toPlain (AnnotatedText at) = join (toList $ segment <$> at)

-- HTML -----------------------------------------------------------------------

toHtml :: SyntaxText -> Html
toHtml (AnnotatedText segments) =
  let renderedSegments =
        fmap segmentToHtml segments
   in span_ [class_ "syntax"] (toList renderedSegments)

nameToHtml :: Name -> Html
nameToHtml =
  span_ [class_ "fqn"] . List.intersperse (span_ [class_ "sep"] [text "."])
    . map ((\s -> span_ [class_ "segment"] [text s]) . NameSegment.toText)
    . Name.segments

segmentToHtml :: SyntaxSegment -> Html
segmentToHtml (Segment segmentText element) =
  let sText = Text.pack segmentText

      el = fromMaybe Blank element

      ref =
        case el of
          TypeReference h ->
            Just h
          TermReference h ->
            Just h
          AbilityConstructorReference h ->
            Just h
          DataConstructorReference h ->
            Just h
          _ ->
            Nothing

      isFQN =
        let isFQN_ =
              Text.isInfixOf "." sText
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
        | Text.isInfixOf "->" sText = span_ [class_ "arrow"] [text sText]
        | isFQN = nameToHtml (Name.unsafeFromText sText)
        | otherwise = text sText
   in case ref of
        Just r ->
          span_ [class_ className, data_ "ref" r] [content]
        _ ->
          span_ [class_ className] [content]

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
    DataTypeKeyword ->
      "data-type-keyword"
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
