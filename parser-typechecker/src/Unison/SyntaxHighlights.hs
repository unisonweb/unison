module Unison.SyntaxHighlights where

import Unison.Util.ColorText

-- The elements of the Unison grammar, for syntax highlighting purposes
data Element = NumericLiteral
             | TextLiteral
             | BooleanLiteral
             | Blank
             | Var
             | Reference
             | Constructor
             | Request
             -- let|alias|handle|in|namespace|type|ability|where|case|of|->|if|then|else|and|or
             | ControlKeyword 
             | TypeAscriptionColon
             -- forall|∀|->
             | TypeOperator
             -- `use Foo bar` is keyword, prefix, suffix
             | UseKeyword
             | UsePrefix
             | UseSuffix
             -- ? ! , ` [ ] { } @ | =
             | DelimiterChar
             | Parenthesis

defaultColors :: Element -> ColorText -> ColorText
defaultColors = \case 
  NumericLiteral      -> purple
  TextLiteral         -> yellow
  BooleanLiteral      -> cyan
  Blank               -> hiCyan
  Var                 -> white
  Reference           -> green
  Constructor         -> green
  Request             -> green
  ControlKeyword      -> red
  TypeAscriptionColon -> red
  TypeOperator        -> red
  UseKeyword          -> white
  UsePrefix           -> white
  UseSuffix           -> white
  DelimiterChar       -> white
  Parenthesis         -> white
