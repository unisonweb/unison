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
             -- let|alias|handle|in|namespace|where|case|of|->|if|then|else|and|or
             | ControlKeyword 
             | BindingEquals
             -- forall|∀|->
             | TypeOperator
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
             -- ? ! , ' ` [ ] { } @ | = _
             -- Currently not all commas in the pretty-print output are marked up as DelimiterChar - we miss
             -- out characters emitted by Pretty.hs helpers like Pretty.commas.
             | DelayForceChar
             | DelimiterChar
             -- ! '
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
  Request             -> hiGreen
  AbilityBraces       -> hiGreen
  ControlKeyword      -> red
  BindingEquals       -> red
  TypeOperator        -> red
  TypeAscriptionColon -> hiBlack
  DataTypeKeyword     -> hiBlue
  DataType            -> blue
  DataTypeParams      -> white
  DataTypeModifier    -> hiBlack
  UseKeyword          -> hiBlack
  UsePrefix           -> hiBlack
  UseSuffix           -> hiBlack
  DelayForceChar      -> yellow
  DelimiterChar       -> white
  Parenthesis         -> white

fmt :: Element -> Pretty ColorText -> Pretty ColorText
fmt e = fmap $ defaultColors e
