{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language BangPatterns #-}

module Unison.TermParser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.Functor
import Unison.Literal (Literal)
import Unison.Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Literal as Literal
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Var as Var
import qualified Text.Parsec.Layout as L

--import Debug.Trace
--import Text.Parsec (anyChar)
--
--pTrace s = pt <|> return ()
--    where pt = attempt $
--               do
--                 x <- attempt $ many anyChar
--                 trace (s++": " ++x) $ attempt $ char 'z'
--                 fail x
--
---- traced s p = p
--traced s p = do
--  pTrace s
--  a <- p <|> trace (s ++ " backtracked") (fail s)
--  let !x = trace (s ++ " succeeded") ()
--  pure a

{-
Precedence of language constructs is identical to Haskell, except that all
operators (like +, <*>, or any sequence of non-alphanumeric characters) are
left-associative and equal precedence, and operators must have surrounding
whitespace (a + b, not a+b) to distinguish from identifiers that may contain
operator characters (like empty? or fold-left).

Sections / partial application of infix operators is not implemented.
-}

type S = TypeParser.S

term :: Var v => Parser (S v) (Term v)
term = term2

term2 :: Var v => Parser (S v) (Term v)
term2 = lam term2 <|> term3

term3 :: Var v => Parser (S v) (Term v)
term3 = do
  t <- let_ <|> ifthen <|> infixApp
  ot <- optional (token (char ':') *> TypeParser.type_)
  pure $ case ot of
    Nothing -> t
    Just y -> Term.ann t y

infixApp :: Var v => Parser (S v) (Term v)
infixApp = chainl1 term4 (f <$> infixVar)
  where
    f :: Ord v => v -> Term v -> Term v -> Term v
    f op lhs rhs = Term.apps (Term.var op) [lhs,rhs]

term4 :: Var v => Parser (S v) (Term v)
term4 = effectBlock <|> term5

term5 :: Var v => Parser (S v) (Term v)
term5 = f <$> some termLeaf
  where
    f (func:args) = Term.apps func args
    f [] = error "'some' shouldn't produce an empty list"

termLeaf :: Var v => Parser (S v) (Term v)
termLeaf =
  asum [hashLit, prefixTerm, lit, tupleOrParenthesized term, blank, vector term]

ifthen :: Var v => Parser (S v) (Term v)
ifthen = do
  _ <- token (string "if")
  cond <- L.withoutLayout "then" term
  _ <- token (string "then")
  iftrue <- L.withoutLayout "else" term
  _ <- token (string "else")
  iffalse <- L.block term
  pure (Term.apps (Term.lit Literal.If) [cond, iftrue, iffalse])

tupleOrParenthesized :: Var v => Parser (S v) (Term v) -> Parser (S v) (Term v)
tupleOrParenthesized rec =
  parenthesized $ go <$> sepBy1 (token $ string ",") rec where
    go [t] = t -- was just a parenthesized term
    go terms = foldr pair unit terms -- it's a tuple literal
    pair t1 t2 = Term.builtin "pair" `Term.app` t1 `Term.app` t2
    unit = Term.builtin "()"

-- |
-- do Remote x := pure 23; y := at node2 23; pure 19;;
-- do Remote action1; action2;;
-- do Remote action1; x = 1 + 1; action2;;
-- do Remote
--   x := pure 23
--   y = 11
--   pure (f x)
effectBlock :: forall v . Var v => Parser (S v) (Term v)
effectBlock = (token (string "do") *> wordyId keywords) >>= go where
  go name = do
    bindings <- L.laidout $ asum [Right <$> binding, Left <$> action]
    Just result <- pure $ foldr bind Nothing bindings
    pure result
    where
    qualifiedPure, qualifiedBind :: Term v
    qualifiedPure = ABT.var' (Text.pack name `mappend` Text.pack ".pure")
    qualifiedBind = ABT.var' (Text.pack name `mappend` Text.pack ".bind")
    bind :: (Either (Term v) (v, Term v)) -> Maybe (Term v) -> Maybe (Term v)
    bind = go where
      go (Right (lhs,rhs)) (Just acc) = Just $ qualifiedBind `Term.apps` [Term.lam lhs acc, rhs]
      go (Right (_,_)) Nothing = Nothing
      go (Left action) (Just acc) = Just $ qualifiedBind `Term.apps` [Term.lam (ABT.v' "_") acc, action]
      go (Left action) _ = Just action
    interpretPure :: Term v -> Term v
    interpretPure = ABT.subst (ABT.v' "pure") qualifiedPure
    binding :: Parser (S v) (v, Term v)
    binding = do
      (lhs, eff) <- attempt $ do
        lhs <- ABT.v' . Text.pack <$> token (wordyId keywords)
        eff <- token $ (True <$ string ":=") <|> (False <$ string "=")
        pure (lhs, eff)
      rhs <- L.block term
      let rhs' = if eff then interpretPure rhs
                 else qualifiedPure `Term.app` rhs
      pure (lhs, rhs')
    action :: Parser (S v) (Term v)
    action = interpretPure <$> L.block term

text' :: Parser s Literal
text' =
  token $ fmap (Literal.Text . Text.pack) ps
  where ps = char '"' *> Unison.Parser.takeWhile "text literal" (/= '"') <* char '"'

text :: Ord v => Parser s (Term v)
text = Term.lit <$> text'

number' :: Parser s Literal
number' = token (f <$> digits <*> optional ((:) <$> char '.' <*> digits))
  where
    digits = takeWhile1 "number" isDigit
    f :: String -> Maybe String -> Literal
    f whole part =
      (Literal.Number . read) $ maybe whole (whole++) part

hashLit :: Ord v => Parser s (Term v)
hashLit = token (f =<< (mark *> hash))
  where
    f h = case Term.derived' (Text.pack h) of
      Nothing -> fail "invalid base58 string"
      Just a -> pure a
    mark = char '#'
    hash = base64urlstring

number :: Ord v => Parser (S v) (Term v)
number = Term.lit <$> number'

lit' :: Parser s Literal
lit' = text' <|> number'

lit :: Ord v => Parser (S v) (Term v)
lit = Term.lit <$> lit'

blank :: Ord v => Parser (S v) (Term v)
blank = token (char '_') $> Term.blank

vector :: Ord v => Parser (S v) (Term v) -> Parser (S v) (Term v)
vector p = Term.app (Term.builtin "Vector.force") . Term.vector <$> (lbracket *> elements <* rbracket)
  where
    lbracket = token (char '[')
    elements = sepBy comma (L.withoutLayout "vector element" p)
    comma = token (char ',')
    rbracket = token (char ']')

let_ :: Var v => Parser (S v) (Term v)
let_ = join $ fixup <$> (let_ *> optional rec_) <*> L.laidout (many alias *> bindingOrTerm)
  where
    let_ = token (string "let")
    rec_ = token (string "rec") $> ()
    bindingOrTerm = (Left <$> binding) <|> (Right <$> term2)
    fixup r bs = case reverse bs of
      (Left b : _) -> fail "let block must end with an expression"
      (Right e : bs) -> pure (f r (reverse [ b | Left b <- bs ], e))
      [] -> fail "empty let block"
    f Nothing (bindings,body) = Term.let1 bindings body
    f (Just _) (bindings,body) = Term.letRec bindings body

binding :: Var v => Parser (S v) (v, Term v)
binding = label "binding" $ do
  typ <- optional typedecl <* optional semicolon
  let lhs = attempt ((\arg1 op arg2 -> (op,[arg1,arg2]))
                    <$> prefixVar <*> infixVar <*> prefixVar)
                <|> ((,) <$> prefixVar <*> many prefixVar)
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (name, args) <- attempt (lhs <* token (char '='))
      body <- L.block term
      pure $ mkBinding name args body
    Just (nameT, typ) -> do
      (name, args) <- lhs
      when (name /= nameT) $
        fail ("The type signature for ‘" ++ show (Var.name nameT) ++ "’ lacks an accompanying binding")
      body <- token (char '=') *> L.block term
      pure $ fmap (\e -> Term.ann e typ) (mkBinding name args body)
  where
  mkBinding f [] body = (f, body)
  mkBinding f args body = (f, Term.lam'' args body)

typedecl :: Var v => Parser (S v) (v, Type v)
typedecl = (,) <$> attempt (prefixVar <* token (char ':')) <*> L.block TypeParser.type_

bindingEqBody :: Parser (S v) (Term v) -> Parser (S v) (Term v)
bindingEqBody p = eq *> L.block p
  where eq = token (char '=')

infixVar :: Var v => Parser s v
infixVar = (Var.named . Text.pack) <$> (backticked <|> symbolyId keywords)
  where
    backticked = attempt (char '`') *> wordyId keywords <* token (char '`')

prefixVar :: Var v => Parser s v
prefixVar = (Var.named . Text.pack) <$> label "symbol" (token prefixOp)
  where
    prefixOp = wordyId keywords
           <|> (char '(' *> symbolyId keywords <* token (char ')')) -- no whitespace w/in parens

prefixTerm :: Var v => Parser (S v) (Term v)
prefixTerm = Term.var <$> prefixVar

keywords :: [String]
keywords = ["alias", "do", "let", "rec", "in", "->", ":", "=", "where", "else", "then"]

lam :: Var v => Parser (S v) (Term v) -> Parser (S v) (Term v)
lam p = attempt (Term.lam'' <$> vars <* arrow) <*> body
  where
    vars = some prefixVar
    arrow = token (string "->")
    body = p

alias :: Var v => Parser (S v) ()
alias = do
  _ <- token (string "alias")
  (fn:params) <- some (Var.named . Text.pack <$> wordyId keywords)
  _ <- token (char '=')
  body <- L.block TypeParser.type_
  TypeParser.Aliases s <- get
  let s' = (fn, apply)
      apply args | length args <= length params = ABT.substs (params `zip` args) body
      apply args = apply (take n args) `Type.apps` drop n args
      n = length params
  set (TypeParser.Aliases (s':s))

bindings :: Var v => Parser (S v) [(v, Term v)]
bindings = do s0 <- get; L.laidout (many alias *> binding) <* set s0 where

moduleBindings :: Var v => Parser (S v) [(v, Term v)]
moduleBindings = root bindings
