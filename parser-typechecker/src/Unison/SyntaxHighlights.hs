module Unison.SyntaxHighlights where

import Unison.Util.ColorText
import Unison.Util.Pretty             ( Pretty )

-- The elements of the Unison grammar, for syntax highlighting purposes
data Element = NumericLiteral
             | TextLiteral
             | BooleanLiteral
             | Blank
             | Var
             | Reference
             | Constructor
             | Request
             | AbilityBraces
             -- let|handle|in|where|case|of|->|if|then|else|and|or
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

defaultColors :: Element -> ColorText -> ColorText
defaultColors = \case
  NumericLiteral      -> id
  TextLiteral         -> id
  BooleanLiteral      -> id
  Blank               -> id
  Var                 -> id
  Reference           -> id
  Constructor         -> id
  Request             -> id
  AbilityBraces       -> hiBlack
  ControlKeyword      -> bold
  TypeOperator        -> hiBlack
  BindingEquals       -> id
  TypeAscriptionColon -> blue
  DataTypeKeyword     -> id
  DataType            -> id
  DataTypeParams      -> id
  DataTypeModifier    -> id
  UseKeyword          -> hiBlack
  UsePrefix           -> hiBlack
  UseSuffix           -> hiBlack
  HashQualifier       -> hiBlack
  DelayForceChar      -> yellow
  DelimiterChar       -> id
  Parenthesis         -> id

fmt :: Element -> Pretty ColorText -> Pretty ColorText
fmt e = fmap $ defaultColors e
