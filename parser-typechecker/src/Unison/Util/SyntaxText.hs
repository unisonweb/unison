module Unison.Util.SyntaxText where

import Unison.Prelude

import Unison.Util.AnnotatedText      ( AnnotatedText(..), annotate )

type SyntaxText = AnnotatedText Element

-- The elements of the Unison grammar, for syntax highlighting purposes
data Element = NumericLiteral
             | TextLiteral
             | CharLiteral
             | BooleanLiteral
             | Blank
             | Var
             | Reference
             | Constructor
             | Request
             | AbilityBraces
             -- let|handle|in|where|match|with|cases|->|if|then|else|and|or
             | ControlKeyword
             -- forall|->
             | TypeOperator
             | BindingEquals
             | TypeAscriptionColon
             -- type|ability
             | DataTypeKeyword
             | DataTypeParams
             | DataType
             -- unique
             | DataTypeModifier
             -- `use Foo bar` is keyword, prefix, suffix
             | UseKeyword
             | UsePrefix
             | UseSuffix
             | HashQualifier
             | DelayForceChar
             -- ? , ` [ ] @ |
             -- Currently not all commas in the pretty-print output are marked up as DelimiterChar - we miss
             -- out characters emitted by Pretty.hs helpers like Pretty.commas.
             | DelimiterChar
             -- ! '
             | Parenthesis
             | LinkKeyword -- `typeLink` and `termLink`
             -- [: :] @[]
             | DocDelimiter
             -- the 'include' in @[include], etc
             | DocKeyword
             deriving (Eq, Ord, Bounded, Enum, Show, Read)

syntax :: Element -> SyntaxText -> SyntaxText
syntax = annotate

-- Convert a `SyntaxText` to a `String`, ignoring syntax markup
toPlain :: SyntaxText -> String
toPlain (AnnotatedText at) = join (toList $ fst <$> at)
