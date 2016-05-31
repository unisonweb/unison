module Unison.TermParser where

import Prelude hiding (takeWhile)

import Control.Applicative (some, optional, (<|>))
import Data.Char (isLower, isDigit, isAlpha, isSymbol)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Text as Text

import Unison.Parser
import Unison.Reference (Reference)
import Unison.Symbol (Symbol, Symbol(..))
import Unison.Term (Term, Literal)
import Unison.Type (Type)
import Unison.TypeParser (RefLookup)
import Unison.Var (Var)
import Unison.View (DFO)
import qualified Unison.ABT as ABT
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Var as Var
import qualified Unison.View as View

type MakeParser v = RefLookup -> Parser (Term v)

term :: Var v => MakeParser v
-- term l = ABT.freeVars (term1 l)
term = term1

term1 :: Var v => MakeParser v
term1 l = possiblyAnnotated term2 l

term2 :: Var v => MakeParser v
term2 l = let_ term3 l <|> term3 l

term3 :: Var v => MakeParser v
term3 l = app term4 l <|> term4 l

term4 :: Var v => MakeParser v
term4 l = lam term l <|> termLeaf l

termLeaf :: Var v => MakeParser v
termLeaf l = asum [lit, parenthesized (term l), blank, vector term l, var]

-- app vs ann:  a b c ::   -> ann has lower priority

-- app vs lam:
--   a b c d -> e
--     as: a b c (d -> e)
--     or: (a b c d -> e)
-- lam has higher priorty than app

-- app vs let:
--   a b c let blah = blah in blah
--     as: a b c (let blah = blah in blah)
--   let a = b in c d e
--   as let a = b in (c d e)
--   vs (let a = b in c) d e
-- app has higher precedence than let because the app will be the body

-- lam vs let:
--  let a = b in a -> b
-- lam has higher precedence than let

-- let vs ann
-- let a = b in 1 + x :: ann
--   as let a = b in 1 + (x :: ann)
--   vs (let a = b in 1 + x) :: ann
-- let has higher precedence

--- high priority
-- lit, blank, vector, ref
-- lam
-- app
-- let
-- ann
--- low priority

-- lit' :: Parser Literal
text' :: Parser Literal
text' =
  token $ fmap (Term.Text . Text.pack) ps
  where ps = char '"' *> Unison.Parser.takeWhile (/= '"') <* char '"'

text :: Ord v => Parser (Term v)
text = Term.lit <$> text'

number' :: Parser Literal
number' = token (f <$> digits <*> optional ((:) <$> char '.' <*> digits))
  where
    digits = nonempty (takeWhile isDigit)
    f :: String -> Maybe String -> Literal
    f whole part =
      (Term.Number . read) $ maybe whole (whole++) part

number :: Ord v => Parser (Term v)
number = Term.lit <$> number'

lit' :: Parser Literal
lit' = text' <|> number'

lit :: Ord v => Parser (Term v)
lit = Term.lit <$> lit'

blank :: Ord v => Parser (Term v)
blank = token (char '_') $> Term.blank

vector :: Ord v => MakeParser v -> MakeParser v
vector rec l = Term.vector <$> (lbracket *> elements <* rbracket)
  where
    lbracket = token (char '[')
    elements = sepBy comma (rec l)
    comma = token (char ',')
    rbracket = lineErrorUnless "syntax error" $ token (char ']')

possiblyAnnotated :: Var v => MakeParser v -> MakeParser v
possiblyAnnotated rec l = f <$> rec l <*> optional(ann'' l)
  where
    f t (Just y) = Term.ann t y
    f t Nothing = t

ann'' :: Var v => RefLookup -> Parser (Type v)
ann'' l = token (char ':') *> TypeParser.type_ l

var :: Var v => Parser (Term v)
var = (Term.var' . Text.pack) <$> varName

--let server = _; blah = _ in _
let_ :: Var v => MakeParser v -> MakeParser v
let_ rec l = f <$> (let_ *> optional rec_) <*> bindings <* in_ <*> body
  where
    let_ = token (string "let")
    rec_ = token (string "rec") $> ()
    bindings = lineErrorUnless "error parsing let bindings" $ sepBy1 (token (char ';')) (letBinding rec l)
    in_ = lineErrorUnless "missing 'in' after bindings in let-expression'" $ token (string "in")
    body = lineErrorUnless "parse error in body of let-expression" $ rec l
    -- f = maybe Term.let1'
    f :: Var v => Maybe () -> [(Text.Text, Term v)] -> Term v -> Term v
    f Nothing bindings body = Term.let1' bindings body
    f (Just _) bindings body = Term.letRec' bindings body

-- var = body
letBinding :: Ord v => MakeParser v -> RefLookup -> Parser (Text.Text, Term v)
letBinding rec l = (,) <$> var <* eq <*> body
  where
    var = lineErrorUnless "invalid variable name in let-binding" $ fmap Text.pack varName
    eq = lineErrorUnless "missing '=' in let-binding" $ token (char '=')
    body = lineErrorUnless "parse error in body of let-binding" $ rec l

varName :: Parser String
varName =
  token $ constrainedIdentifier [ isLower . head
                                , all isAlpha
                                , (`notElem` keywords)
                                ]
keywords :: [String]
keywords = ["let", "rec", "in", "->", ":"]

lam :: Var v => MakeParser v -> MakeParser v
lam rec l = Term.lam' <$> vars <* arrow <*> body
  where
    vars = some (Text.pack <$> token varName)
    arrow = token (string "->")
    body = rec l

app :: Ord v => MakeParser v -> MakeParser v
app rec l = f <$> some (rec l)
  where
    f (func:args) = Term.apps func args
    f [] = error "'some' shouldn't produce an empty list"

-- isSymbols = all
-- a + b :: Int   parses as (a + b) :: Int
-- a `foo` b   parses as (foo a b)
operator :: Parser String
operator = token $ symbols <|> ((char '`') *> varName <* char '`')
  where symbols = constrainedIdentifier [ all isSymbol ]


----- temporary stuff
type V = Symbol DFO

type RefLookup' = Text.Text -> Maybe Reference

resolveAllAsBuiltin :: RefLookup
resolveAllAsBuiltin s = Just (Reference.Builtin $ Text.pack s)

resolveAllAsBuiltin' :: RefLookup'
resolveAllAsBuiltin' t = Just (Reference.Builtin t)

parseTermTest :: String -> Result (Term V)
parseTermTest s = run (term resolveAllAsBuiltin) s

dogCatMouse :: RefLookup
dogCatMouse s =
  if s `elem` ["dog", "cat", "mouse"] then
    Just (Reference.Builtin $ Text.pack s)
  else Nothing

substsTest :: Var v => RefLookup -> MakeParser v -> String -> Either [String] (Term v, Set v)
substsTest stringToRef p s = (,) <$> term' <*> freeVars'
  where
    -- term :: Either [String] (Unison.Term.Term v)
    term = toEither $ run (p stringToRef) s

    -- freeVars :: Either [String] (Set v)
    freeVars = ABT.freeVars <$> term

    -- term' :: Either [String] (Unison.ABT.Term (Unison.Term.F v) v ())
    term' = ABT.substs <$> varTermList <*> term

    -- freeVars' :: Either [String] (Set v)
    freeVars' = ABT.freeVars <$> term'

    -- varTermList :: Either [String] [(v, Unison.Term.Term v)]
    varTermList = foldMap collectRefs <$> freeVars

    varToString :: Var v => v -> String
    varToString v = Text.unpack (Var.name v)

    collectRefs :: Var v => v -> [(v, Term v)]
    collectRefs a = case stringToRef (varToString a) of
      Just r -> [(a, Term.ref r)]
      Nothing -> []

substsTestV :: RefLookup -> MakeParser V -> String -> Either [String] (Term V, Set V)
substsTestV = substsTest
