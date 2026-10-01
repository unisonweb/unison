module Unison.Util.SyntaxText where

import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Pattern (SeqOp)
import Unison.Prelude
import Unison.ReferentPrime (Referent')
import Unison.Util.AnnotatedText (AnnotatedText (..), annotate, segment)

type SyntaxText' r = AnnotatedText (Element r)

-- The elements of the Unison grammar, for syntax highlighting purposes
data Element r
  = NumericLiteral
  | TextLiteral
  | BytesLiteral
  | CharLiteral
  | BooleanLiteral
  | Blank
  | Var
  | TypeReference (Maybe Name {- fqn, if it has one -}) r
  | TermReference (Maybe Name {- fqn, if it has one -}) (Referent' r)
  | Op SeqOp
  | AbilityBraces
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
  | HashQualifier (HashQualified Name)
  | DelayForceChar
  | -- ? , ` [ ] @ |
    -- Currently not all commas in the pretty-print output are marked up as DelimiterChar - we miss
    -- out characters emitted by Pretty.hs helpers like Pretty.commas.
    DelimiterChar
  | -- ! '
    Parenthesis
  | LinkKeyword -- `typeLink` and `termLink`
  -- { } @
  | DocDelimiter
  | -- the 'source' in @source{…}, etc
    DocKeyword
  | RecordFieldName Text
  | RecordFieldValueColon
  | RecordExtraFields
  deriving (Eq, Ord, Show, Functor)

syntax :: Element r -> SyntaxText' r -> SyntaxText' r
syntax = annotate

-- Convert a `SyntaxText` to a `Text`, ignoring syntax markup
toPlain :: SyntaxText' r -> Text
toPlain (AnnotatedText at) = foldMap segment at
