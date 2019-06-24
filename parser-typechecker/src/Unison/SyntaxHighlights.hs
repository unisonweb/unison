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
             | UseStatement
             -- , ` [ ] { } @ | =
             | DelimiterChar
             | Parenthesis

-- TODO this is currently just inspired by the sublime haskell colors - what would people rather?
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
  UseStatement        -> white
  DelimiterChar       -> white
  Parenthesis         -> white
