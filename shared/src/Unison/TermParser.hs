module Unison.TermParser where

import Prelude hiding (takeWhile)
import Unison.Var (Var)
import Unison.Parser
import Unison.TypeParser (RefLookup)
import qualified Unison.TypeParser as TypeParser
import Unison.Type (Type)
import Unison.Term (Term, Literal)
import qualified Unison.Term as Term
import qualified Data.Text as Text
import Control.Applicative (some, optional, (<|>))
import Data.Functor (($>))
import Data.Foldable (asum)
import Data.Char (isLower, isDigit, isAlpha, isSymbol)

import Unison.Symbol (Symbol)
import Unison.View (DFO)
import qualified Unison.Reference as Reference

type MakeParser v = RefLookup -> Parser (Term v)

term :: Var v => MakeParser v
term l = possiblyAnnotated term2 l

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
vector rec l = Term.vector <$> (lbracket *> sepBy1 comma (rec l) <* rbracket)
  where
    lbracket = token (char '[')
    comma = token (char ',')
    rbracket = token (char ']')

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
    bindings = sepBy1 (token (char ';')) (letBinding rec l)
    in_ = token (string "in")
    body = rec l
    -- f = maybe Term.let1'
    f :: Var v => Maybe () -> [(Text.Text, Term v)] -> Term v -> Term v
    f Nothing bindings body = Term.let1' bindings body
    f (Just _) bindings body = Term.letRec' bindings body

-- var = body
letBinding :: Ord v => MakeParser v -> RefLookup -> Parser (Text.Text, Term v)
letBinding rec l = (,) <$> fmap Text.pack varName <* token (char '=') <*> rec l

varName :: Parser String
varName =
  token $ constrainedIdentifier [ isLower . head
                        , all isAlpha
                        , not . flip elem keywords
                        ]
  where keywords = ["let", "rec", "in"]

lam :: Var v => MakeParser v -> MakeParser v
lam rec l = Term.lam' <$> vars <* arrow <*> body
  where
    vars = some (Text.pack <$> token varName)
    arrow = token (string "->")
    body = rec l

app :: Ord v => MakeParser v -> MakeParser v
app rec l = f <$> some(rec l)
  where
    f (func:args) = Term.apps func args
    f [] = error "'some' shouldn't produce an empty list"

-- isSymbols = all
-- a + b :: Int   parses as (a + b) :: Int
-- a `foo` b   parses as (foo a b)
operator :: Parser String
operator = token $ symbols <|> (char '`') *> varName <* char '`'
  where symbols = constrainedIdentifier [ all isSymbol ]

-- type V = Symbol DFO
-- foo :: String -> Result (Term V)
-- foo s = run (term l) s
--   where l s = Just (Reference.Builtin $ Text.pack s)
--
-- foo' :: (RefLookup -> Parser (Term V)) -> String -> Result (Term V)
-- foo' p s = run (p l) s
--   where l s = Just (Reference.Builtin $ Text.pack s)
