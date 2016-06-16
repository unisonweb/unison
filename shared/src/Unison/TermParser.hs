module Unison.TermParser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char (isDigit, isAlphaNum, isSpace, isSymbol, isPunctuation)
import Data.Foldable (asum)
import Data.Functor (($>), void)
import Data.List (foldl')
import Data.Set (Set)
import Unison.Parser
import Unison.Symbol (Symbol, Symbol(..))
import Unison.Term (Term, Literal)
import Unison.Type (Type)
import Unison.View (DFO)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Term as Term
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Var as Var

type V = Symbol DFO

term :: Parser (Term V)
term = possiblyAnnotated term2

term2 :: Parser (Term V)
term2 = let_ term3 <|> term3

term3 :: Parser (Term V)
term3 = infixApp term4 <|> term4

infixApp :: Parser (Term V) -> Parser (Term V)
infixApp p = f <$> arg <*> some ((,) <$> infixVar <*> arg)
  where
    arg = p
    f :: Term V -> [(V, Term V)] -> Term V
    f = foldl' g
    g :: Term V -> (V, Term V) -> Term V
    g lhs (op, rhs) = Term.apps (Term.var op) [lhs,rhs]

term4 :: Parser (Term V)
term4 = prefixApp term5

term5 :: Parser (Term V)
term5 = lam term <|> termLeaf

termLeaf :: Parser (Term V)
termLeaf = asum [prefixTerm, lit, parenthesized term, blank, vector term]

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

vector :: Parser (Term V) -> Parser (Term V)
vector p = Term.vector <$> (lbracket *> elements <* rbracket)
  where
    lbracket = token (char '[')
    elements = sepBy comma p
    comma = token (char ',')
    rbracket = lineErrorUnless "syntax error" $ token (char ']')

possiblyAnnotated :: Parser (Term V) -> Parser (Term V)
possiblyAnnotated p = f <$> p <*> optional ann''
  where
    f t (Just y) = Term.ann t y
    f t Nothing = t

ann'' :: Parser (Type V)
ann'' = token (char ':') *> TypeParser.type_

--let server = _; blah = _ in _
let_ :: Parser (Term V) -> Parser (Term V)
let_ p = f <$> (let_ *> optional rec_) <*> bindings' <* in_ <*> body
  where
    let_ = token (string "let")
    rec_ = token (string "rec") $> ()
    bindings' = lineErrorUnless "error parsing let bindings" (bindings p)
    in_ = lineErrorUnless "missing 'in' after bindings in let-expression'" $ token (string "in")
    body = lineErrorUnless "parse error in body of let-expression" p
    -- f = maybe Term.let1'
    f :: Maybe () -> [(V, Term V)] -> Term V -> Term V
    f Nothing bindings body = Term.let1 bindings body
    f (Just _) bindings body = Term.letRec bindings body


semicolon :: Parser ()
semicolon = void $ token (char ';')

infixBinding :: Parser (Term V) -> Parser (V, Term V)
infixBinding p = ((,,,,) <$> optional (typedecl <* semicolon) <*> prefixVar <*> infixVar <*> prefixVar <*> bindingEqBody p) >>= f
  where
    f :: (Maybe (V, Type V), V, V, V, Term V) -> Parser (V, Term V)
    f (Just (opName', _), _, opName, _, _) | opName /= opName' =
      failWith ("The type signature for ‘" ++ show opName' ++ "’ lacks an accompanying binding")
    f (Nothing, arg1, opName, arg2, body) = pure (mkBinding opName [arg1,arg2] body)
    f (Just (_, type'), arg1, opName, arg2, body) = pure $ (`Term.ann` type') <$> mkBinding opName [arg1,arg2] body

mkBinding :: V -> [V] -> Term V -> (V, Term V)
mkBinding f [] body = (f, body)
mkBinding f args body = (f, Term.lam'' args body)

typedecl :: Parser (V, Type V)
typedecl = (,) <$> prefixVar <*> ann''

prefixBinding :: Parser (Term V) -> Parser (V, Term V)
prefixBinding p = ((,,,) <$> optional (typedecl <* semicolon) <*> prefixVar <*> many prefixVar <*> bindingEqBody p) >>= f -- todo
  where
    f :: (Maybe (V, Type V), V, [V], Term V) -> Parser (V, Term V)
    f (Just (opName, _), opName', _, _) | opName /= opName' =
      failWith ("The type signature for ‘" ++ show opName' ++ "’ lacks an accompanying binding")
    f (Nothing, name, args, body) = pure $ mkBinding name args body
    f (Just (_, t), name, args, body) = pure $ (`Term.ann` t) <$> mkBinding name args body

bindingEqBody :: Parser (Term V) -> Parser (Term V)
bindingEqBody p = eq *> body
  where
    eq = token (char '=')
    body = lineErrorUnless "parse error in body of binding" p

-- a wordyId isn't all digits, and isn't all symbols
wordyId :: Parser String
wordyId = token $ f <$> id <*> optional ((:) <$> dot <*> id)
  where
    dot = char '.'
    id = identifier [any (not.isDigit), any isAlphaNum, (`notElem` keywords)]
    f id rest = maybe id (id++) rest

-- a symbolyId is all symbols
symbolyId :: Parser String
symbolyId = token $ identifier'
  [notReservedChar, not . isSpace, \c -> isSymbol c || isPunctuation c]
  [(`notElem` keywords)]

infixVar :: Parser V
infixVar = (Var.named . Text.pack) <$> (backticked <|> symbolyId)
  where
    backticked = char '`' *> wordyId <* token (char '`')


prefixVar :: Parser V
prefixVar = (Var.named . Text.pack) <$> prefixOp
  where
    prefixOp :: Parser String
    prefixOp = wordyId <|> (char '(' *> symbolyId <* token (char ')')) -- no whitespace w/in parens

prefixTerm :: Parser (Term V)
prefixTerm = Term.var <$> prefixVar

keywords :: Set String
keywords = Set.fromList ["let", "rec", "in", "->", ":", "=", "where"]

lam :: Parser (Term V) -> Parser (Term V)
lam p = Term.lam'' <$> vars <* arrow <*> body
  where
    vars = some prefixVar
    arrow = token (string "->")
    body = p

prefixApp :: Parser (Term V) -> Parser (Term V)
prefixApp p = f <$> some p
  where
    f (func:args) = Term.apps func args
    f [] = error "'some' shouldn't produce an empty list"

bindings :: Parser (Term V) -> Parser [(V, Term V)]
bindings p = --many (binding term)
  sepBy1 (token (char ';' <|> char '\n')) (prefixBinding p <|> infixBinding p)
