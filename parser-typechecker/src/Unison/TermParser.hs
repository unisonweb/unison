{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language BangPatterns #-}
{-# Language TupleSections #-}

module Unison.TermParser where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader (ask)
import           Data.Char (isDigit, isUpper)
import           Data.Foldable (asum,toList)
import           Data.Functor
import           Data.Int (Int64)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           Debug.Trace
import           Prelude hiding (and, or, takeWhile)
import qualified Text.Parsec.Layout as L
import qualified Unison.ABT as ABT
import           Unison.Parser
import           Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import qualified Unison.Reference as R
import           Unison.Term (Term)
import qualified Unison.Term as Term
import           Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import           Unison.Var (Var)
import qualified Unison.Var as Var

{-
Precedence of language constructs is identical to Haskell, except that all
operators (like +, <*>, or any sequence of non-alphanumeric characters) are
left-associative and equal precedence, and operators must have surrounding
whitespace (a + b, not a+b) to distinguish from identifiers that may contain
operator characters (like empty? or fold-left).

Sections / partial application of infix operators is not implemented.
-}

type S = TypeParser.S

type TermP v = Parser (S v) (Term v)

term :: Var v => TermP v
term = term2

term2 :: Var v => TermP v
term2 = lam term2 <|> term3

term3 :: Var v => TermP v
term3 = do
  t <- letBlock <|> handle <|> ifthen <|> and <|> or <|> match <|> infixApp
  ot <- optional (token (char ':') *> TypeParser.valueType)
  pure $ case ot of
    Nothing -> t
    Just y -> Term.ann t y

-- We disallow type annotations and lambdas,
-- just function application and operators
blockTerm :: Var v => TermP v
blockTerm = letBlock <|> handle <|> ifthen <|> and <|> or <|> match <|>
            lam term <|> infixApp

match :: Var v => TermP v
match = do
  token_ $ string "case"
  scrutinee <- term
  token_ $ string "of"
  cases <- L.vblockNextToken (sepBy L.vsemi matchCase)
  pure $ Term.match scrutinee cases

matchCase :: Var v => Parser (S v) (Term.MatchCase (Term v))
matchCase = do
  (p, boundVars) <- parsePattern
  guard <- traced "guard" $ optional $ token (string "|") *> infixApp
  traced "->" $ token_ $ string "->"
  t <- block
  pure . Term.MatchCase p guard $ ABT.absChain boundVars t

parsePattern :: Var v => Parser (S v) (Pattern, [v])
parsePattern = traced "pattern" $ constructor <|> leaf
  where
  leaf = literal <|> varOrAs <|> unbound <|>
         parenthesizedOrTuplePattern <|> effect
  literal = traced "pattern.literal" $ (,[]) <$> asum [true, false, number]
  true = Pattern.Boolean True <$ token (string "true")
  false = Pattern.Boolean False <$ token (string "false")
  number = traced "pattern.number" $ number' Pattern.Int64 Pattern.UInt64 Pattern.Float
  parenthesizedOrTuplePattern = tupleOrParenthesized parsePattern unit pair
  unit = (Pattern.Constructor (R.Builtin "()") 0 [], [])
  pair (p1, v1) (p2, v2) =
    (Pattern.Constructor (R.Builtin "Pair") 0 [p1, p2], v1 ++ v2)
  varOrAs = traced "varOrAs" $ do
    v <- prefixVar
    o <- optional (token $ string "@")
    if isJust o then
      (\(p, vs) -> (Pattern.As p, v : vs)) <$> leaf
      else pure (Pattern.Var, [v])
  unbound = traced "unbound" $ (Pattern.Unbound, []) <$ token (char '_')
  ctorName = traced "ctorName" . token $ do
    s <- wordyId keywords
    guard . isUpper . head $ s
    pure s

  effectBind0 = traced "effectBind0" $ do
    name <- ctorName
    leaves <- many leaf
    token_ (string "->")
    pure (name, leaves)

  effectBind = do
    (name, leaves) <- attempt effectBind0
    (cont, vsp) <- parsePattern
    env <- ask
    (ref,cid) <- case Map.lookup name env of
      Just (ref, cid) -> pure (ref, cid)
      Nothing -> fail $ "unknown data constructor " ++ name
    pure $ case unzip leaves of
      (patterns, vs) ->
         (Pattern.EffectBind ref cid patterns cont, join vs ++ vsp)

  effectPure = go <$> parsePattern where
    go (p, vs) = (Pattern.EffectPure p, vs)

  effect = do
    token_ (string "{")
    (effectBind <|> effectPure) <* token_ (string "}")

  constructor = traced "constructor" $ do
    name <- ctorName
    env <- ask
    case Map.lookup name env of
      Just (ref, cid) -> go <$> traced "pattern.manyleaf" (many leaf)
        where
          go pairs = case unzip pairs of
            (patterns, vs) -> (Pattern.Constructor ref cid patterns, join vs)
      Nothing ->
        trace ("unknown data constructor " ++ name) $
          (traced ("failing " ++ name) . fail $ "unknown data constructor " ++ name)

letBlock :: Var v => TermP v
letBlock = traced "letBlock" $ token (string "let") *> block

infixApp :: Var v => TermP v
infixApp = chainl1 term4 (f <$> infixVar)
  where
    f :: Ord v => v -> Term v -> Term v -> Term v
    f op lhs rhs = Term.apps (Term.var() op) [lhs,rhs]

term4 :: Var v => TermP v
term4 = traced "apply-chain" $ f <$> some termLeaf
  where
    f (func:args) = Term.apps func args
    f [] = error "'some' shouldn't produce an empty list"

termLeaf :: forall v. Var v => TermP v
termLeaf = traced "leaf" $
  asum [hashLit, prefixTerm, text, number, boolean, tupleOrParenthesizedTerm, blank, vector term]

ifthen :: Var v => TermP v
ifthen = traced "ifthen" $ do
  token_ $ string "if"
  cond <- block' $ L.virtual_rbrace <|> (lookAhead . token_ $ string "then")
  token_ $ string "then"
  iftrue <- block' $ L.virtual_rbrace <|> (lookAhead . token_ $ string "else")
  token_ $ string "else"
  iffalse <- block
  pure $ Term.iff cond iftrue iffalse

and :: Var v => TermP v
and = Term.and <$> (token (string "and") *> termLeaf) <*> termLeaf

or :: Var v => TermP v
or = Term.or <$> (token (string "or") *> termLeaf) <*> termLeaf

-- Generic parser for tuples and parenthesized patterns/terms
tupleOrParenthesized :: Parser s a -> a -> (a -> a -> a) -> Parser s a
tupleOrParenthesized rec unit pair =
  parenthesized $ go <$> sepBy (token $ string ",") rec where
    go [t] = t -- was just a parenthesized term
    go terms = foldr pair unit terms -- it's a tuple literal

-- Specialized to terms
tupleOrParenthesizedTerm :: Var v => TermP v
tupleOrParenthesizedTerm = tupleOrParenthesized term unit pair
  where
    pair t1 t2 =
      Term.constructor (R.Builtin "Pair") 0 `Term.app` t1 `Term.app` t2
    unit = Term.constructor (R.Builtin "()") 0

text' :: Parser s Text.Text
text' =
  token $ fmap Text.pack ps
  where ps = char '"' *> Unison.Parser.takeWhile "text literal" (/= '"') <* char '"'

text :: Ord v => Parser s (Term v)
text = Term.text() <$> text'

number :: Ord v => Parser s (Term v)
number = number' (Term.int64()) (Term.uint64()) (Term.float())

number' :: (Int64 -> a) -> (Word64 -> a) -> (Double -> a) -> Parser s a
number' i u f = token $ do
  let digits = takeWhile1 "number" isDigit
  sign <- optional (char '+' <|> char '-')
  ds <- digits
  fraction <- optional ((:) <$> char '.' <*> digits)
  pure $ case fraction of
    Nothing -> case sign of
      Nothing -> u (read ds)
      Just '+' -> i (read ds)
      Just '-' -> i (read ('-':ds))
      _ -> error "impossible!"
    Just fraction ->
      let signl = toList sign
      in f (read (signl ++ ds ++ fraction))

boolean :: Ord v => Parser s (Term v)
boolean =
  (Term.boolean() True <$ token (string "true")) <|>
  (Term.boolean() False <$ token (string "false"))

hashLit :: Ord v => Parser s (Term v)
hashLit = token (f =<< (mark *> hash))
  where
    f h = case Term.derived' (Text.pack h) of
      Nothing -> fail "invalid base58 string"
      Just a -> pure a
    mark = char '#'
    hash = base64urlstring

blank :: Ord v => TermP v
blank = token (char '_') $> Term.blank()

vector :: Ord v => TermP v -> TermP v
vector p = Term.vector <$> (lbracket *> elements <* rbracket)
  where
    lbracket = token (char '[')
    elements = sepBy comma (L.withoutLayout "vector element" p)
    comma = token (char ',')
    rbracket = token (char ']')

eq :: Parser (S v) ()
eq = void $ char '=' <* L.space

binding :: Var v => Parser (S v) (v, Term v)
binding = traced "binding" . label "binding" $ do
  typ <- optional typedecl
  let lhs = attempt ((\arg1 op arg2 -> (op,[arg1,arg2]))
                    <$> prefixVar <*> infixVar <*> prefixVar)
                <|> ((,) <$> prefixVar <*> many prefixVar)
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (name, args) <- attempt (lhs <* eq)
      body <- block
      pure $ mkBinding name args body
    Just (nameT, typ) -> do
      (name, args) <- lhs
      when (name /= nameT) $
        fail ("The type signature for ‘" ++ show (Var.name nameT) ++ "’ lacks an accompanying binding")
      body <- eq *> block
      pure $ fmap (\e -> Term.ann e typ) (mkBinding name args body)
  where
  mkBinding f [] body = (f, body)
  mkBinding f args body = (f, Term.lam'' args body)

typedecl :: Var v => Parser (S v) (v, Type v)
typedecl = (,) <$> attempt (prefixVar <* token (char ':'))
               <*> L.vblockIncrement TypeParser.valueType

infixVar :: Var v => Parser s v
infixVar = (Var.named . Text.pack) <$> (backticked <|> symbolyId keywords)
  where
    backticked = attempt (char '`') *> wordyId keywords <* token (char '`')

prefixVar :: Var v => Parser s v
prefixVar = traced "prefixVar" $ (Var.named . Text.pack) <$> label "symbol" (token prefixOp)
  where
    prefixOp = wordyId keywords
           <|> (char '(' *> symbolyId keywords <* token (char ')')) -- no whitespace w/in parens

prefixTerm :: Var v => TermP v
prefixTerm = Term.var() <$> prefixVar

keywords :: [String]
keywords =
  [ "->"
  , ":"
  , "="
  , "let"
  , "alias"
  , "and", "or"
  , "case", "of"
  , "handle", "in"
  , "if", "then", "else"
  , "namespace"
  , "type", "effect", "where"
  , "true", "false"
  ]

block' :: Var v => Parser (S v) () -> TermP v
block' vendbrace =
  traced "block" $
    go =<< L.vblock' L.virtual_lbrace_nextToken vendbrace (sepBy L.vsemi statement)
  where
    statement =
      traced "statement" $ (Right <$> binding) <|> (Left <$> blockTerm)
    toBinding (Right (v, e)) = (v,e)
    toBinding (Left e) = (Var.named "_", e)
    go bs = case reverse bs of
      (Right _e : _) -> fail "block must end with an expression"
      -- TODO: Inform the user that we're going to rewrite the block,
      -- possibly changing the meaning of the program (which is ambiguous anyway),
      -- or fail with a helpful error message if there's a forward reference with
      -- effects.
      (Left e : bs) -> pure $ Term.letRec (toBinding <$> reverse bs) e
      [] -> fail "empty block"

block :: Var v => TermP v
block = block' (traced "L.virtual_rbrace" L.virtual_rbrace)

handle :: Var v => TermP v
handle = traced "handle" $ do
  token_ $ string "handle"
  handler <- term
  token_ $ string "in"
  b <- block
  pure $ Term.handle handler b

lam :: Var v => TermP v -> TermP v
lam p = traced "lambda" $ attempt (Term.lam'' <$> vars <* arrow) <*> body
  where
    vars = some prefixVar
    arrow = token (string "->")
    body = p

alias :: Var v => Parser (S v) ()
alias = do
  _ <- token (string "alias")
  (fn:params) <- some (Var.named . Text.pack <$> wordyId keywords)
  _ <- eq
  body <- L.vblockIncrement TypeParser.valueType
  TypeParser.Aliases s <- get
  let s' = (fn, apply)
      apply args | length args <= length params = ABT.substs (params `zip` args) body
      apply args = apply (take n args) `Type.apps` (addUnit <$> drop n args)
      n = length params
      addUnit a = ((), a)
  set (TypeParser.Aliases (s':s))

-- bindings :: Var v => Parser (S v) [(v, Term v)]
-- bindings = do s0 <- get; L.laidout (many alias *> binding) <* set s0 where

-- moduleBindings :: Var v => Parser (S v) [(v, Term v)]
-- moduleBindings = root bindings
