{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Unison.TermParser where

-- import           Debug.Trace
import qualified Data.Text as Text
import           Control.Applicative
import           Control.Monad (guard, join, when)
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
import           Unison.Parser
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import qualified Unison.Reference as R
import           Unison.Term (AnnotatedTerm)
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import           Unison.Type (AnnotatedType)
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

type TermP v = P v (AnnotatedTerm v Ann)

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
            delayBlock <|> lam term <|> infixApp

delayBlock :: Var v => TermP v
delayBlock = do
  b <- block "delay"
  pure $ Term.delay (ann b) b

match :: Var v => TermP v
match = do
  start <- reserved "case"
  scrutinee <- term
  _ <- openBlockWith "of"
  -- TODO: Produce a nice error message for empty match list
  cases <- sepBy1 semi matchCase
  _ <- closeBlock
  pure $ Term.match (ann start <> ann (last cases)) scrutinee cases

matchCase :: Var v => P v (Term.MatchCase Ann (AnnotatedTerm v Ann))
matchCase = do
  (p, boundVars) <- parsePattern
  guard <- optional $ reserved "|" *> infixApp
  t <- block "->"
  pure . Term.MatchCase p guard $ ABT.absChain' boundVars t

parsePattern :: forall v. Var v => P v (Pattern Ann, [(Ann, v)])
parsePattern = constructor <|> leaf
  where
  leaf = literal <|> varOrAs <|> unbound <|>
         parenthesizedOrTuplePattern <|> effect
  literal = (,[]) <$> asum [true, false, number]
  true = (\t -> Pattern.Boolean (ann t) True) <$> reserved "true"
  false = (\t -> Pattern.Boolean (ann t) False) <$> reserved "false"
  number = number' (tok Pattern.Int64) (tok Pattern.UInt64) (tok Pattern.Float)
  parenthesizedOrTuplePattern :: P v (Pattern Ann, [(Ann, v)])
  parenthesizedOrTuplePattern = tupleOrParenthesized parsePattern unit pair
  unit ann = (Pattern.Constructor ann (R.Builtin "()") 0 [], [])
  pair (p1, v1) (p2, v2) =
    (Pattern.Constructor (ann p1 <> ann p2) (R.Builtin "Pair") 0 [p1, p2],
     v1 ++ v2)
  varOrAs :: P v (Pattern Ann, [(Ann, v)])
  varOrAs = do
    v <- prefixVar
    o <- optional (reserved "@")
    if isJust o then
      (\(p, vs) -> (Pattern.As (ann v) p, tokenToPair v : vs)) <$> leaf
      else pure (Pattern.Var (ann v), [tokenToPair v])
  unbound :: P v (Pattern Ann, [(Ann, v)])
  unbound = (\tok -> (Pattern.Unbound (ann tok), [])) <$> blank
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
      Nothing -> customFailure $ UnknownEffectConstructor name
    pure $ case unzip leaves of
      (patterns, vs) ->
         (Pattern.EffectBind (ann name <> ann cont) ref cid patterns cont,
          join vs ++ vsp)

  effectPure = go <$> parsePattern where
    go (p, vs) = (Pattern.EffectPure (ann p) p, vs)

  effect = do
    start <- openBlockWith "{"
    (inner, vs) <- effectBind <|> effectPure
    _ <- closeBlock
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
      Nothing -> customFailure $ UnknownDataConstructor t

lam :: Var v => TermP v -> TermP v
lam p = label "lambda" $ mkLam <$> P.try (some prefixVar <* reserved "->") <*> p
  where
    mkLam vs b = Term.lam' (ann (head vs) <> ann b) (map L.payload vs) b

letBlock, handle, ifthen, and, or, infixApp :: Var v => TermP v
letBlock = label "let" $ block "let"

handle = label "handle" $ do
  t <- reserved "handle"
  handler <- term
  b <- block "in"
  pure $ Term.handle (ann t <> ann b) handler b

ifthen = label "if" $ do
  c <- block "if"
  t <- block "then"
  f <- block "else"
  pure $ Term.iff (ann c <> ann f) c t f

hashLit :: Var v => TermP v
hashLit = tok Term.derived <$> hashLiteral

prefixTerm :: Var v => TermP v
prefixTerm = tok Term.var <$> prefixVar

text :: Var v => TermP v
text = tok Term.text <$> string

boolean :: Var v => TermP v
boolean = ((\t -> Term.boolean (ann t) True) <$> reserved "true") <|>
          ((\t -> Term.boolean (ann t) False) <$> reserved "false")

placeholder :: Var v => TermP v
placeholder = (\t -> Term.placeholder (ann t) (L.payload t)) <$> blank

vector :: Var v => TermP v -> TermP v
vector p = f <$> reserved "[" <*> elements <*> reserved "]"
  where
    elements = sepBy (reserved ",") p
    f open elems close = Term.vector (ann open <> ann close) elems

termLeaf :: forall v. Var v => TermP v
termLeaf =
  asum [hashLit, prefixTerm, text, number, boolean,
        tupleOrParenthesizedTerm, placeholder, vector term,
        delayQuote, bang]

delayQuote :: Var v => TermP v
delayQuote = P.label "quote" $ do
  start <- reserved "'"
  e <- termLeaf
  pure $ Term.delay (ann start <> ann e) e

bang :: Var v => TermP v
bang = P.label "bang" $ do
  start <- reserved "!"
  e <- termLeaf
  pure $ Term.force (ann start <> ann e) (ann start) e

and = label "and" $ f <$> reserved "and" <*> termLeaf <*> termLeaf
  where f kw x y = Term.and (ann kw <> ann y) x y

or = label "or" $ f <$> reserved "or" <*> termLeaf <*> termLeaf
  where f kw x y = Term.or (ann kw <> ann y) x y

var :: Var v => L.Token v -> AnnotatedTerm v Ann
var t = Term.var (ann t) (L.payload t)

term4 :: Var v => TermP v
term4 = f <$> some termLeaf
  where
    f (func:args) = Term.apps func ((\a -> (ann func <> ann a, a)) <$> args)
    f [] = error "'some' shouldn't produce an empty list"

infixApp = label "infixApp" $
  chainl1 term4 (f <$> fmap var infixVar)
    where
      f op lhs rhs =
        Term.apps op [(ann lhs, lhs), (ann rhs, rhs)]

typedecl :: Var v => P v (L.Token v, AnnotatedType v Ann)
typedecl =
  (,) <$> P.try (prefixVar <* reserved ":")
      <*> (Type.generalizeLowercase <$> TypeParser.valueType)
      <* semi

binding :: forall v. Var v => P v ((Ann, v), AnnotatedTerm v Ann)
binding = label "binding" $ do
  typ <- optional typedecl
  let infixLhs = do
        (arg1, op) <- P.try ((,) <$> prefixVar <*> infixVar)
        arg2 <- prefixVar
        pure (ann arg1, L.payload op, [arg1, arg2])
  let prefixLhs = do
        v  <- prefixVar
        vs <- many prefixVar
        pure (ann v, L.payload v, vs)
  let
    lhs :: P v (Ann, v, [L.Token v])
    lhs = infixLhs <|> prefixLhs
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (loc, name, args) <- P.try (lhs <* P.lookAhead (openBlockWith "="))
      body <- block "="
      pure $ mkBinding loc name args body
    Just (nameT, typ) -> do
      (_, name, args) <- lhs
      when (name /= L.payload nameT) $
        customFailure $ SignatureNeedsAccompanyingBody nameT
      body <- block "="
      pure $ fmap (\e -> Term.ann (ann nameT <> ann e) e typ)
                  (mkBinding (ann nameT) name args body)
  where
  mkBinding loc f [] body = ((loc, f), body)
  mkBinding loc f args body =
    ((loc, f), Term.lam' (loc <> ann body) (L.payload <$> args) body)


customFailure :: P.MonadParsec e s m => e -> m a
customFailure = P.customFailure

block :: forall v. Var v => String -> TermP v
block s = block' s (openBlockWith s) closeBlock

--module Monoid where
--  -- we replace all the binding names with Monoid.op, and
--  -- if `op` is free in the body of any binding, we replace it with `Monoid.op`
--  op : Monoid a -> (a -> a -> a)
--  op m = case m of Monoid

data BlockElement v
  = Binding ((Ann, v), AnnotatedTerm v Ann)
  | Action (AnnotatedTerm v Ann)
  | Namespace String [BlockElement v]

namespaceBlock :: Var v => P v (BlockElement v)
namespaceBlock = do
  _ <- reserved "namespace"
  name <- wordyId
  let statement = (Binding <$> binding) <|> namespaceBlock
  _ <- openBlockWith "where"
  elems <- sepBy semi statement
  _ <- closeBlock
  pure $ Namespace (L.payload name) elems

block' :: forall v b. Var v => String -> P v (L.Token ()) -> P v b -> TermP v
block' s openBlock closeBlock = do
    open <- openBlock
    statements <- sepBy semi statement
    _ <- closeBlock
    go open statements
  where
    statement = traceRemainingTokens "statement" *>
                ( (Binding <$> binding) <|>
                  (Action <$> blockTerm) <|>
                  namespaceBlock )
    toBindings (Binding ((a, v), e)) = [((a, v), e)]
    toBindings (Action e) = [((ann e, Var.named "_"), e)]
    toBindings (Namespace name bs) = scope name $ (toBindings =<< bs)
    scope :: String -> [((Ann, v), AnnotatedTerm v Ann)]
                    -> [((Ann, v), AnnotatedTerm v Ann)]
    scope name bs =
      let vs = (snd . fst) <$> bs
          prefix v = Var.named (Text.pack name `mappend` "." `mappend` Var.name v)
          vs' = prefix <$> vs
      in [ ((a, v'), ABT.substInheritAnnotation v (Term.var a v') e) |
           (((a,v),e), v') <- bs `zip` vs' ]

    go :: L.Token () -> [BlockElement v] -> P v (AnnotatedTerm v Ann)
    go open bs =
      let startAnnotation = (fst . fst . head $ toBindings =<< bs)
          endAnnotation = (fst . fst . last $ toBindings =<< bs)
      in case reverse bs of
        Namespace _ _ : _ ->
          customFailure $ BlockMustEndWithExpression
                            startAnnotation
                            endAnnotation
        Binding ((a, _v), annotatedTerm) : _ ->
          customFailure $ BlockMustEndWithExpression
                            (startAnnotation <> ann annotatedTerm)
                            (a <> ann annotatedTerm)
        Action e : es ->
          pure $ Term.letRec (startAnnotation <> ann e)
                             (toBindings =<< reverse es)
                             e
        [] -> customFailure $ EmptyBlock (const s <$> open)

number :: Var v => TermP v
number = number' (tok Term.int64) (tok Term.uint64) (tok Term.float)

number'
  :: Var v
  => (L.Token Int64 -> a)
  -> (L.Token Word64 -> a)
  -> (L.Token Double -> a)
  -> P v a
number' i u f = fmap go numeric
  where
    go num@(L.payload -> p)
      | elem '.' p = f (read <$> num)
      | take 1 p == "+" = i (read . drop 1 <$> num)
      | take 1 p == "-" = i (read <$> num)
      | otherwise = u (read <$> num)

tupleOrParenthesizedTerm :: Var v => TermP v
tupleOrParenthesizedTerm = label "tuple" $ tupleOrParenthesized term Term.unit pair
  where
    pair t1 t2 =
      Term.app (ann t1 <> ann t2)
        (Term.app (ann t1)
                  (Term.constructor (ann t1 <> ann t2) (R.Builtin "Pair") 0)
                  t1)
        t2
