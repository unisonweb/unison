{-# Language OverloadedStrings #-}

module Unison.TermParser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char (isDigit, isAlphaNum, isSpace, isSymbol, isPunctuation)
import Data.Foldable (asum)
import Data.Functor
import Data.List (foldl')
import Data.Set (Set)
import Unison.Parser
import Unison.Term (Term, Literal)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Var as Var

{-
Precedence of language constructs is identical to Haskell, except that all
operators (like +, <*>, or any sequence of non-alphanumeric characters) are
left-associative and equal precedence, and operators must have surrounding
whitespace (a + b, not a+b) to distinguish from identifiers that may contain
operator characters (like empty? or fold-left).

Sections / partial application of infix operators is not implemented.
-}

term :: Var v => Parser (Term v)
term = possiblyAnnotated term2

term2 :: Var v => Parser (Term v)
term2 = let_ term3 <|> term3

term3 :: Var v => Parser (Term v)
term3 = infixApp term4 <|> term4

infixApp :: Var v => Parser (Term v) -> Parser (Term v)
infixApp p = f <$> arg <*> some ((,) <$> infixVar <*> arg)
  where
    arg = p
    f :: Ord v => Term v -> [(v, Term v)] -> Term v
    f = foldl' g
    g :: Ord v => Term v -> (v, Term v) -> Term v
    g lhs (op, rhs) = Term.apps (Term.var op) [lhs,rhs]

term4 :: Var v => Parser (Term v)
term4 = prefixApp term5

term5 :: Var v => Parser (Term v)
term5 = lam term <|> effectBlock <|> termLeaf

termLeaf :: Var v => Parser (Term v)
termLeaf = asum [hashLit, prefixTerm, lit, tupleOrParenthesized term, blank, vector term]

tupleOrParenthesized :: Var v => Parser (Term v) -> Parser (Term v)
tupleOrParenthesized rec =
  parenthesized $ go <$> sepBy1 (token $ string ",") rec where
    go [t] = t -- was just a parenthesized term
    go terms = foldr pair unit terms -- it's a tuple literal
    pair t1 t2 = Term.builtin "Pair" `Term.app` t1 `Term.app` t2
    unit = Term.builtin "()"

-- |
-- do Remote { x := pure 23; y := at node2 23; pure 19 }
-- do Remote { action1; action2; }
-- do Remote { action1; x = 1 + 1; action2; }
-- do Remote
--   x := pure 23
--   y = 11
--   pure (f x)
effectBlock :: Var v => Parser (Term v)
effectBlock = (token (string "do") *> withLayout wordyId) >>= go where
  go name = layout $ do
    bindings <- sepBy1 semicolon $ asum [Right <$> binding, Left <$> action]
    Just result <- pure $ foldr bind Nothing bindings
    pure result
    where
    qualifiedPure = ABT.var' (Text.pack name `mappend` Text.pack ".pure")
    qualifiedBind = ABT.var' (Text.pack name `mappend` Text.pack ".bind")
    bind = go where
      go (Right (lhs,rhs)) (Just acc) = Just $ qualifiedBind `Term.apps` [Term.lam lhs acc, rhs]
      go (Right (_,_)) Nothing = Nothing
      go (Left action) (Just acc) = Just $ qualifiedBind `Term.apps` [Term.lam (ABT.v' "_") acc, action]
      go (Left action) _ = Just action
    interpretPure = ABT.subst (ABT.v' "pure") qualifiedPure
    binding = scope "binding" $ do
      lhs <- ABT.v' . Text.pack <$> token wordyId
      eff <- token $ (True <$ string ":=") <|> (False <$ string "=")
      rhs <- term
      let rhs' = if eff then interpretPure rhs
                 else qualifiedPure `Term.app` rhs
      pure (lhs, rhs')
    action = attempt . scope "action" $ (interpretPure <$> term)

text' :: Parser Literal
text' =
  token $ fmap (Term.Text . Text.pack) ps
  where ps = char '"' *> Unison.Parser.takeWhile "text literal" (/= '"') <* char '"'

text :: Ord v => Parser (Term v)
text = Term.lit <$> text'

number' :: Parser Literal
number' = token (f <$> digits <*> optional ((:) <$> char '.' <*> digits))
  where
    digits = nonempty (takeWhile "number" isDigit)
    f :: String -> Maybe String -> Literal
    f whole part =
      (Term.Number . read) $ maybe whole (whole++) part

hashLit :: Ord v => Parser (Term v)
hashLit = token (f <$> (mark *> hash))
  where
    f = Term.derived' . Text.pack
    mark = char '#'
    hash = lineErrorUnless "error parsing base64url hash" base64urlstring

number :: Ord v => Parser (Term v)
number = Term.lit <$> number'

lit' :: Parser Literal
lit' = text' <|> number'

lit :: Ord v => Parser (Term v)
lit = Term.lit <$> lit'

blank :: Ord v => Parser (Term v)
blank = token (char '_') $> Term.blank

vector :: Ord v => Parser (Term v) -> Parser (Term v)
vector p = Term.vector <$> (lbracket *> elements <* rbracket)
  where
    lbracket = token (char '[')
    elements = sepBy comma p
    comma = token (char ',')
    rbracket = lineErrorUnless "syntax error" $ token (char ']')

possiblyAnnotated :: Var v => Parser (Term v) -> Parser (Term v)
possiblyAnnotated p = f <$> p <*> optional ann''
  where
    f t (Just y) = Term.ann t y
    f t Nothing = t

ann'' :: Var v => Parser (Type v)
ann'' = token (char ':') *> TypeParser.type_

--let server = _; blah = _ in _
let_ :: Var v => Parser (Term v) -> Parser (Term v)
let_ p = f <$> withLayout (let_ *> optional rec_) <*> (layout bindings' <|> bindings')
  where
    let_ = token (string "let")
    rec_ = token (string "rec") $> ()
    bindings' = withLayout $ do
      bs <- lineErrorUnless "error parsing let bindings" (bindings p)
      semicolon
      body <- lineErrorUnless "parse error in body of let-expression" p
      pure (bs, body)
    f :: Ord v => Maybe () -> ([(v, Term v)], Term v) -> Term v
    f Nothing (bindings, body) = Term.let1 bindings body
    f (Just _) (bindings, body) = Term.letRec bindings body

typedecl :: Var v => Parser (v, Type v)
typedecl = (,) <$> prefixVar <*> ann''

bindingEqBody :: Parser (Term v) -> Parser (Term v)
bindingEqBody p = eq *> (layout body <|> body)
  where
    eq = token (char '=')
    body = lineErrorUnless "parse error in body of binding" p

-- a wordyId isn't all digits, and isn't all symbols
wordyId :: Parser String
wordyId = token $ f <$> id <*> optional ((:) <$> dot <*> wordyId)
  where
    dot = char '.'
    id = identifier [any (not.isDigit), any isAlphaNum, (`notElem` keywords)]
    f id rest = maybe id (id++) rest

-- a symbolyId is all symbols
symbolyId :: Parser String
symbolyId = token $ identifier'
  [notReservedChar, not . isSpace, \c -> isSymbol c || isPunctuation c]
  [(`notElem` keywords)]

infixVar :: Var v => Parser v
infixVar = (Var.named . Text.pack) <$> (backticked <|> symbolyId)
  where
    backticked = char '`' *> wordyId <* token (char '`')

prefixVar :: Var v => Parser v
prefixVar = (Var.named . Text.pack) <$> prefixOp
  where
    prefixOp :: Parser String
    prefixOp = wordyId <|> (char '(' *> symbolyId <* token (char ')')) -- no whitespace w/in parens

prefixTerm :: Var v => Parser (Term v)
prefixTerm = Term.var <$> prefixVar

keywords :: Set String
keywords = Set.fromList ["do", "let", "rec", "in", "->", ":", "=", "where"]

lam :: Var v => Parser (Term v) -> Parser (Term v)
lam p = Term.lam'' <$> vars <* arrow <*> body
  where
    vars = some prefixVar
    arrow = token (string "->")
    body = p

prefixApp :: Ord v => Parser (Term v) -> Parser (Term v)
prefixApp p = f <$> some p
  where
    f (func:args) = Term.apps func args
    f [] = error "'some' shouldn't produce an empty list"

bindings :: Var v => Parser (Term v) -> Parser [(v, Term v)]
bindings p = withLayout (sepBy1 semicolon binding) where
  binding = do
    typ <- optional (typedecl <* semicolon)
    (name, args) <- ((\arg1 op arg2 -> (op,[arg1,arg2])) <$> prefixVar <*> infixVar <*> prefixVar)
                  <|> ((,) <$> prefixVar <*> many prefixVar)
    body <- bindingEqBody p
    case typ of
      Nothing -> pure $ mkBinding name args body
      Just (nameT, typ)
        | name == nameT -> case mkBinding name args body of (v,body) -> pure (v, Term.ann body typ)
        | otherwise -> failWith ("The type signature for ‘" ++ show (Var.name nameT) ++ "’ lacks an accompanying binding")

  mkBinding f [] body = (f, body)
  mkBinding f args body = (f, Term.lam'' args body)

moduleBindings :: Var v => Parser [(v, Term v)]
moduleBindings = root (bindings term3)
