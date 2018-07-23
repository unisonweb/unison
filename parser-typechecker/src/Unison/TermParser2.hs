{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Unison.TermParser2 where

import           Control.Applicative
import           Control.Monad (guard, join)
import           Control.Monad.Reader (ask)
import           Data.Char (isUpper)
import           Data.Foldable (asum)
import           Data.Int (Int64)
import           Data.List (elem)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Word (Word64)
import           Prelude hiding (and, or)
import qualified Text.Megaparsec as P
import qualified Unison.ABT as ABT
import qualified Unison.Lexer as L
import           Unison.Parser2
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import qualified Unison.Reference as R
import           Unison.Term (AnnotatedTerm)
import qualified Unison.Term as Term
import qualified Unison.TypeParser2 as TypeParser
import           Unison.Var (Var)

{-
Precedence of language constructs is identical to Haskell, except that all
operators (like +, <*>, or any sequence of non-alphanumeric characters) are
left-associative and equal precedence, and operators must have surrounding
whitespace (a + b, not a+b) to distinguish from identifiers that may contain
operator characters (like empty? or fold-left).

Sections / partial application of infix operators is not implemented.
-}

type TermP v = P (AnnotatedTerm v Ann)

term :: Var v => TermP v
term = term2

term2 :: Var v => TermP v
term2 = lam term2 <|> term3

term3 :: Var v => TermP v
term3 = do
  t <- letBlock <|> handle <|> ifthen <|> and <|> or <|> match <|> infixApp
  ot <- optional (reserved ":" *> TypeParser.computationType)
  pure $ case ot of
    Nothing -> t
    Just y -> Term.ann (mkAnn t y) t y

-- We disallow type annotations and lambdas,
-- just function application and operators
blockTerm :: Var v => TermP v
blockTerm = letBlock <|> handle <|> ifthen <|> and <|> or <|> match <|>
            lam term <|> infixApp

match :: Var v => TermP v
match = do
  start <- reserved "case"
  scrutinee <- term
  _ <- reserved "of"
  -- TODO: Produce a nice error message for empty match list
  cases <- sepBy1 semi matchCase
  pure $ Term.match (ann start <> ann (last cases)) scrutinee cases

matchCase :: Var v => P (Term.MatchCase Ann (AnnotatedTerm v Ann))
matchCase = do
  (p, boundVars) <- parsePattern
  guard <- optional $ reserved "|" *> infixApp
  _ <- reserved "->"
  t <- blockTerm
  pure . Term.MatchCase p guard $ ABT.absChain' boundVars t

parsePattern :: forall v. Var v => P (Pattern Ann, [(Ann, v)])
parsePattern = constructor <|> leaf
  where
  leaf = literal <|> varOrAs <|> unbound <|>
         parenthesizedOrTuplePattern <|> effect
  literal = (,[]) <$> asum [true, false, number]
  true = (\t -> Pattern.Boolean (ann t) True) <$> reserved "true"
  false = (\t -> Pattern.Boolean (ann t) False) <$> reserved "false"
  number = number' (tok Pattern.Int64) (tok Pattern.UInt64) (tok Pattern.Float)
  parenthesizedOrTuplePattern :: P (Pattern Ann, [(Ann, v)])
  parenthesizedOrTuplePattern = tupleOrParenthesized parsePattern unit pair
  unit ann = (Pattern.Constructor ann (R.Builtin "()") 0 [], [])
  pair (p1, v1) (p2, v2) =
    (Pattern.Constructor (ann p1 <> ann p2) (R.Builtin "Pair") 0 [p1, p2],
     v1 ++ v2)
  varOrAs :: P (Pattern Ann, [(Ann, v)])
  varOrAs = do
    v <- prefixVar
    o <- optional (reserved "@")
    if isJust o then
      (\(p, vs) -> (Pattern.As (ann v) p, (ann v, L.payload v) : vs)) <$> leaf
      else pure (Pattern.Var (ann v), [(ann v, L.payload v)])
  unbound :: P (Pattern Ann, [(Ann, v)])
  unbound = (\tok -> (Pattern.Unbound (ann tok), [])) <$> reserved "_"
  ctorName = P.try $ do
    s <- wordyId
    guard . isUpper . head . L.payload $ s
    pure s

  effectBind0 = do
    name <- ctorName
    leaves <- many leaf
    _ <- reserved "->"
    pure (name, leaves)

  effectBind = do
    (name, leaves) <- P.try effectBind0
    (cont, vsp) <- parsePattern
    env <- ask
    (ref,cid) <- case Map.lookup (L.payload name) env of
      Just (ref, cid) -> pure (ref, cid)
      -- TODO: Fail fancily
      Nothing -> fail $ "unknown data constructor " ++ (L.payload name)
    pure $ case unzip leaves of
      (patterns, vs) ->
         (Pattern.EffectBind (ann name <> ann cont) ref cid patterns cont,
          join vs ++ vsp)

  effectPure = go <$> parsePattern where
    go (p, vs) = (Pattern.EffectPure (ann p) p, vs)

  effect = do
    start <- reserved "{"
    (inner, vs) <- effectBind <|> effectPure
    end <- reserved "}"
    pure $ (Pattern.setLoc inner (ann start <> ann end), vs)

  constructor = do
    t <- ctorName
    let name = L.payload t
    env <- ask
    case Map.lookup name env of
      Just (ref, cid) -> go <$> many leaf
        where
          go pairs = case unzip pairs of
            (patterns, vs) ->
              let loc = foldl (<>) (ann t) $ map ann patterns
              in (Pattern.Constructor loc ref cid patterns, join vs)
      -- TODO: Fail fancily
      Nothing -> fail $ "unknown data constructor " ++ name


lam :: Var v => TermP v -> TermP v
lam p = mkLam <$> P.try (some prefixVar <* reserved "->") <*> p
  where
    mkLam vs b = Term.lam' (ann (head vs) <> ann b) (map L.payload vs) b

letBlock, handle, ifthen, and, or, infixApp :: Var v => TermP v
letBlock = block "let"

handle = do
  t <- reserved "handle"
  handler <- term
  b <- block "in"
  pure $ Term.handle (ann t <> ann b) handler b

ifthen = do
  c <- block "if"
  t <- block "then"
  f <- block "else"
  pure $ Term.iff (ann c <> ann f) c t f

and = undefined

or = undefined

infixApp = undefined

block :: Var v => String -> TermP v
block = undefined

number'
  :: (L.Token Int64 -> a)
  -> (L.Token Word64 -> a)
  -> (L.Token Double -> a)
  -> P a
number' i u f = fmap go numeric
  where
    go num@(L.payload -> p)
      | elem '.' p = f (read <$> num)
      | take 1 p == "+" || take 1 p == "-" = i (read <$> num)
      | otherwise = u (read <$> num)

tupleOrParenthesized :: P a -> (Ann -> a) -> (a -> a -> a) -> P a
tupleOrParenthesized = undefined
