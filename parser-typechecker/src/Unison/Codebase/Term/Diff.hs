module Unison.Codebase.Term.Diff (diffTerms) where

import Data.Text (Text)
import Unison.Syntax.DisplayObject (DisplayObject)
import Unison.Parser.Ann (Ann)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)

data DiffTag = Both | LeftOnly | RightOnly

-- | Pretty print each term using their own pretty-printers, then do a simple text diff between them.
diffTerms :: (PPED.PrettyPrintEnvDecl, Symbol, DisplayObject (Type Symbol a) (Term Symbol a)) -> (PPED.PrettyPrintEnvDecl, Symbol, DisplayObject (Type Symbol a) (Term Symbol a)) -> [(DiffTag, Text)]
diffTerms (beforePPED, beforeTerm) (afterPPED, afterTerm) =
  P.prettyTerm
