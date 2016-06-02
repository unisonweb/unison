module Unison.TermParser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char (isLower, isDigit, isAlpha, isSymbol)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (foldl1')
import Data.Set (Set)
import qualified Data.Text as Text
import Unison.Parser
import Unison.Reference (Reference)
import Unison.Symbol (Symbol, Symbol(..))
import Unison.Term (Term, Literal)
import Unison.Type (Type)
import Unison.TypeParser (RefLookup)
import Unison.View (DFO)
import qualified Unison.ABT as ABT
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Var as Var

-- todo:
-- remove use of RefLookup from all parsers
-- do the lookup at the end, following what's already done in substsTest
-- convert the TypeParsers to not use RefLookup
-- resolve :: Term v -> RefLookup -> Term v
--   which replaces free term AND type variables (in embedded Ann) in the input,
--   using the provided RefLookup
--   using the Term.typeMap function to perform substitution of free variables
--   and just using ABT.substs as before to perform substitution of the term's free variables
--   so two calls to subts, one for the term, and one in the function passed to typeMap to do
--   the same substitution over the type annotations

-- use the new parsers to clean up the tests
   -- add whatever convenience functions needed to make that easier

type MakeParser v = RefLookup -> Parser (Term v)

type V = Symbol DFO
type RefLookup' = Text.Text -> Maybe Reference
type RefLookup'' = V -> Maybe Reference

term :: MakeParser V
term = term1

term1 :: MakeParser V
term1 = possiblyAnnotated term2

term2 :: MakeParser V
term2 l = let_ term2_5 l <|> term2_5 l

term2_5 :: MakeParser V
term2_5 = plusparser

plusparser :: MakeParser V
plusparser l = case l "+" of
  Just r -> foldl1' app0 <$> operands
    where
      plus = Term.ref r
      operands = sepBy1 (token $ commit (char '+')) (term3 l)
      app0 t1 t2 = Term.apps plus [t1, t2]
  Nothing -> fail "could not find '+' in environment"

term3 :: MakeParser V
term3 = app term4

term4 :: MakeParser V
term4 l = lam term l <|> termLeaf l

termLeaf :: MakeParser V
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

-- let vs +
-- 1 + let ... -- not supposed to parse
-- 1 + (let ...) -- ok
-- let a = b in a + 1
-- operators have higher precedence

-- app vs +
-- sqrt 42 + 11
-- (sqrt 42) + 11
-- app has higher precedence

--- high priority
-- lit, blank, vector, ref
-- lam
-- app
-- operators
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

vector :: MakeParser V -> MakeParser V
vector rec l = Term.vector <$> (lbracket *> elements <* rbracket)
  where
    lbracket = token (char '[')
    elements = sepBy comma (rec l)
    comma = token (char ',')
    rbracket = lineErrorUnless "syntax error" $ token (char ']')

possiblyAnnotated :: MakeParser V -> MakeParser V
possiblyAnnotated rec l = f <$> rec l <*> optional(ann'' l)
  where
    f t (Just y) = Term.ann t y
    f t Nothing = t

ann'' :: RefLookup -> Parser (Type V)
ann'' l = token (char ':') *> TypeParser.type_ l

var :: Parser (Term V)
var = (Term.var' . Text.pack) <$> varName

--let server = _; blah = _ in _
let_ :: MakeParser V -> MakeParser V
let_ rec l = f <$> (let_ *> optional rec_) <*> bindings <* in_ <*> body
  where
    let_ = token (string "let")
    rec_ = token (string "rec") $> ()
    bindings = lineErrorUnless "error parsing let bindings" $ sepBy1 (token (char ';')) (binding rec l)
    in_ = lineErrorUnless "missing 'in' after bindings in let-expression'" $ token (string "in")
    body = lineErrorUnless "parse error in body of let-expression" $ rec l
    -- f = maybe Term.let1'
    f :: Maybe () -> [(V, Term V)] -> Term V -> Term V
    f Nothing bindings body = Term.let1 bindings body
    f (Just _) bindings body = Term.letRec bindings body

-- var = body
-- TODO:
-- foo x y = 23
-- let foo x y = x + y in ...
-- let
--   foo : Int -> String
--   foo n = ...
-- in ...
binding :: MakeParser V -> RefLookup -> Parser (V, Term V)
binding rec l = (,) <$> (Var.named <$> var) <* eq <*> body
  where
    var = lineErrorUnless "invalid variable name in let-binding" $ fmap Text.pack varName
    eq = lineErrorUnless "missing '=' in let-binding" $ token (char '=')
    body = lineErrorUnless "parse error in body of let-binding" $ rec l

-- todo: maybe split this into operator/nonoperator constructors?
--       e.g. for operator declarations in parens  (!@#$dog) a b = ...
varName :: Parser String
varName =
  token $ constrainedIdentifier [ isLower . head
                                , isAlpha . head
                                , (`notElem` keywords)
                                ]
keywords :: [String]
keywords = ["let", "rec", "in", "->", ":"]

lam :: MakeParser V -> MakeParser V
lam rec l = Term.lam' <$> vars <* arrow <*> body
  where
    vars = some (Text.pack <$> token varName)
    arrow = token (string "->")
    body = rec l

app :: MakeParser V -> MakeParser V
app rec l = f <$> some (rec l)
  where
    f (func:args) = Term.apps func args
    f [] = error "'some' shouldn't produce an empty list"

-- isSymbols = all
-- a + b :: Int   parses as (a + b) :: Int
-- a `foo` b   parses as (foo a b)
operator :: Parser String
operator = token $ symbols <|> (char '`' *> varName <* char '`')
  where symbols = constrainedIdentifier [ all isSymbol ]

bindings :: RefLookup -> Parser [(V, Term V)]
bindings l = many (binding term l)

----- temporary stuff
resolveAllAsBuiltin :: RefLookup
resolveAllAsBuiltin s = Just (Reference.Builtin $ Text.pack s)

resolveAllAsBuiltin' :: RefLookup'
resolveAllAsBuiltin' t = Just (Reference.Builtin t)

parseTermTest :: String -> Result (Term V)
parseTermTest = run (term resolveAllAsBuiltin)

dogCatMouse :: RefLookup
dogCatMouse s =
  if s `elem` ["dog", "cat", "mouse", "+"] then
    Just (Reference.Builtin $ Text.pack s)
  else Nothing

substsTest :: RefLookup -> MakeParser V -> String -> (Term V, Set V)
substsTest stringToRef p s = (term', freeVars')
  where
    term = unsafeRun (p stringToRef) s
    freeVars = ABT.freeVars term
    term' = ABT.substs varTermList term
    freeVars' = ABT.freeVars term'
    varTermList = foldMap collectRefs freeVars

    varToString :: V -> String
    varToString v = Text.unpack (Var.name v)

    collectRefs :: V -> [(V, Term V)]
    collectRefs a = case stringToRef (varToString a) of
      Just r -> [(a, Term.ref r)]
      Nothing -> []

substsTestV :: RefLookup -> MakeParser V -> String -> (Term V, Set V)
substsTestV = substsTest
