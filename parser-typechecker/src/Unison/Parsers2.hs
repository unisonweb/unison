-- These are parsing helper functions that assume the "Builtins",
-- whereas Unison.Parsers doesn't depend on Builtin (because Builtins
-- depends on Parsers).

module Unison.Parsers2 where

import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Parsers as Parsers
import qualified Unison.Builtin as B
import Unison.UnisonFile (UnisonFile(..))
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Parser (PEnv)
import qualified Unison.Parser as Parser

-- parse a type, hard-coding the builtins
t :: String -> Type Symbol
t s = B.resolveBuiltins B.builtinTypes Type.builtin $
        Parsers.unsafeParseType s Parser.penv0

-- parse a term, hard-coding the builtins
tm :: String -> Term Symbol
tm s = B.resolveBuiltins B.builtinTerms Term.builtin $
        Parsers.unsafeParseTerm s Parser.penv0

parseFile :: FilePath -> String -> PEnv -> Either String (UnisonFile Symbol)
parseFile filename s =
  Parsers.parseFile filename B.builtinEnv s

unsafeParseFile :: String -> PEnv -> UnisonFile Symbol
unsafeParseFile = Parsers.unsafeParseFile B.builtinEnv

unsafeParseFile' :: String -> UnisonFile Symbol
unsafeParseFile' = Parsers.unsafeParseFile' B.builtinEnv

unsafeReadAndParseFile' :: String -> IO (UnisonFile Symbol)
unsafeReadAndParseFile' = Parsers.unsafeReadAndParseFile' B.builtinEnv

unsafeReadAndParseFile :: PEnv -> String -> IO (UnisonFile Symbol)
unsafeReadAndParseFile = Parsers.unsafeReadAndParseFile B.builtinEnv
