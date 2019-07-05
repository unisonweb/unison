{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.TermParser where

import           Control.Applicative
import           Control.Monad (guard, join, when)
import           Control.Monad.Reader (asks, local)
import           Data.Char (isUpper)
import           Data.Foldable (asum)
import           Data.Functor
import           Data.Int (Int64)
import           Data.List (elem, find)
import           Data.Maybe (isJust, fromMaybe)
import           Data.Word (Word64)
import           Prelude hiding (and, or, seq)
import           Unison.Names3 (Names)
import           Unison.Parser hiding (seq)
import           Unison.PatternP (Pattern)
import           Unison.Term (AnnotatedTerm, IsTop)
import           Unison.Type (AnnotatedType)
import           Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Unison.ABT as ABT
import qualified Unison.ConstructorType as CT
import qualified Unison.DataDeclaration as DD
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer as L
import qualified Unison.Name as Name
import qualified Unison.Names3 as Names
import qualified Unison.Parser as Parser (seq)
import qualified Unison.PatternP as Pattern
import qualified Unison.Reference as R
import qualified Unison.Term as Term
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Var as Var

import Debug.Trace
import Unison.Reference (Reference)

watch :: Show a => String -> a -> a
watch msg a = let !_ = trace (msg ++ ": " ++ show a) () in a

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
  t <- and <|> or <|> infixApp
  ot <- optional (reserved ":" *> TypeParser.computationType)
  pure $ case ot of
    Nothing -> t
    Just y -> Term.ann (mkAnn t y) t y

keywordBlock :: Var v => TermP v
keywordBlock = letBlock <|> handle <|> ifthen <|> match

-- We disallow type annotations and lambdas,
-- just function application and operators
blockTerm :: Var v => TermP v
blockTerm = and <|> or <|> lam term <|> infixApp

match :: Var v => TermP v
match = do
  start <- reserved "case"
  scrutinee <- term
  _ <- P.try (openBlockWith "of") <|> do
         t <- anyToken
         P.customFailure (ExpectedBlockOpen "of" t)
  cases <- sepBy1 semi matchCase
  -- TODO: Add error for empty match list
  _ <- closeBlock
  pure $ Term.match (ann start <> ann (last cases)) scrutinee cases

matchCase :: Var v => P v (Term.MatchCase Ann (AnnotatedTerm v Ann))
matchCase = do
  (p, boundVars) <- parsePattern
  guard <- optional $ reserved "|" *> infixApp
  t <- block "->"
  pure . Term.MatchCase p (fmap (ABT.absChain' boundVars) guard) $ ABT.absChain' boundVars t

parsePattern :: forall v. Var v => P v (Pattern Ann, [(Ann, v)])
parsePattern =
  chainl1 (constructor <|> seqLiteral <|> leaf) patternInfixApp
  where
  patternInfixApp :: P v ((Pattern Ann, [(Ann, v)])
                  -> (Pattern Ann, [(Ann, v)])
                  -> (Pattern Ann, [(Ann, v)]))
  patternInfixApp = f <$> seqOp
    where
    f op (l, lvs) (r, rvs) =
      (Pattern.SequenceOp (ann l <> ann r) l op r, lvs ++ rvs)

  leaf = literal <|> varOrAs <|> unbound <|>
         parenthesizedOrTuplePattern <|> effect
  literal = (,[]) <$> asum [true, false, number, text]
  true = (\t -> Pattern.Boolean (ann t) True) <$> reserved "true"
  false = (\t -> Pattern.Boolean (ann t) False) <$> reserved "false"
  number = number' (tok Pattern.Int) (tok Pattern.Nat) (tok Pattern.Float)
  text = (\t -> Pattern.Text (ann t) (L.payload t)) <$> string
  parenthesizedOrTuplePattern :: P v (Pattern Ann, [(Ann, v)])
  parenthesizedOrTuplePattern = tupleOrParenthesized parsePattern unit pair
  unit ann = (Pattern.Constructor ann DD.unitRef 0 [], [])
  pair (p1, v1) (p2, v2) =
    (Pattern.Constructor (ann p1 <> ann p2) DD.pairRef 0 [p1, p2],
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
  ctor :: _ -> P v (L.Token (Reference, Int))
  ctor err = P.try $ do
    tok <- hqWordyId
    guard . isUpper . head . fst . L.payload $ tok
    names <- asks snd
    Names.lookupHQPattern
    error "todo"
    -- case Names.patternNameds env (L.payload name) of
           --      Just (ref, cid) -> pure (ref, cid)
           --      Nothing -> customFailure $ UnknownAbilityConstructor name

  unzipPatterns f elems = case unzip elems of (patterns, vs) -> f patterns (join vs)

  effectBind0 = do
    tok <- ctor UnknownAbilityConstructor
    leaves <- many leaf
    _ <- reserved "->"
    pure (tok, leaves)

  effectBind = do
    (tok, leaves) <- P.try effectBind0
    let (ref,cid) = L.payload tok
    (cont, vsp) <- parsePattern
    pure $
      let f patterns vs = (Pattern.EffectBind (ann tok <> ann cont) ref cid patterns cont, vs ++ vsp)
      in unzipPatterns f leaves

  effectPure = go <$> parsePattern where
    go (p, vs) = (Pattern.EffectPure (ann p) p, vs)

  effect = do
    start <- openBlockWith "{"
    (inner, vs) <- effectBind <|> effectPure
    end <- closeBlock
    pure (Pattern.setLoc inner (ann start <> ann end), vs)

  constructor = do
    t <- ctor UnknownDataConstructor
    ---
    error "todo"
--    let name = L.payload t
--    env <- asks snd
--    case Names.patternNameds env name of
--      Just (ref, cid) -> go <$> many leaf
--        where
--          go = unzipPatterns f
--          f patterns vs =
--            let loc = foldl (<>) (ann t) $ map ann patterns
--            in (Pattern.Constructor loc ref cid patterns, vs)
--      Nothing -> customFailure $ UnknownDataConstructor t

  seqLiteral = Parser.seq f leaf
    where f loc = unzipPatterns ((,) . Pattern.SequenceLiteral loc)

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
  start <- peekAny
  c <- block "if"
  t <- block "then"
  f <- block "else"
  pure $ Term.iff (ann start <> ann f) c t f

hashLit :: Var v => TermP v
hashLit =
  -- todo: should probably come up with syntax for hash component refs
  tok (\ann h -> Term.ref ann (R.DerivedId (R.Id h 0 1))) <$> hashLiteral

prefixTerm :: Var v => TermP v
prefixTerm = tok Term.var <$> prefixVar

text :: Var v => TermP v
text = tok Term.text <$> string

boolean :: Var v => TermP v
boolean = ((\t -> Term.boolean (ann t) True) <$> reserved "true") <|>
          ((\t -> Term.boolean (ann t) False) <$> reserved "false")

placeholder :: Var v => TermP v
placeholder = (\t -> Term.placeholder (ann t) (L.payload t)) <$> blank

seq :: Var v => TermP v -> TermP v
seq = Parser.seq Term.seq

hashQualifiedPrefixTerm :: Var v => TermP v
hashQualifiedPrefixTerm = error "todo"
--  do
--  t <- hqPrefixVar
--  flip tok t $ \ann (name, mayHash) -> case mayHash of
--    Nothing   -> pure . Term.var ann $ Var.nameds name
--    Just hash -> do
--      -- This is a hash-qualified name
--      env <- asks $ Map.toList . Names.termNames . snd
--      let hqName   = HQ.HashQualified (Name.fromString name) hash
--          mayMatch = find (\(n, r) -> HQ.matchesNamedReferent n r hqName) env
--      case mayMatch of
--        Nothing -> customFailure . UnknownHashQualifiedName $ L.Token
--          hqName
--          (L.start t)
--          (L.end t)
--        Just (_, r) -> pure $ Term.fromReferent (const CT.Data) ann r

termLeaf :: forall v . Var v => TermP v
termLeaf = do
  e <- asum
    [ prefixTerm
    , hashQualifiedPrefixTerm
    , text
    , number
    , boolean
    , tupleOrParenthesizedTerm
    , keywordBlock
    , placeholder
    , seq term
    , delayQuote
    , bang
    ]
  q <- optional (reserved "?")
  case q of
    Nothing -> pure e
    Just q  -> pure $ Term.app
      (ann q <> ann e)
      (Term.var (ann e) (positionalVar q Var.askInfo))
      e

delayQuote :: Var v => TermP v
delayQuote = P.label "quote" $ do
  start <- reserved "'"
  e <- termLeaf
  pure $ DD.delayTerm (ann start <> ann e) e

bang :: Var v => TermP v
bang = P.label "bang" $ do
  start <- reserved "!"
  e <- termLeaf
  pure $ DD.forceTerm (ann start <> ann e) (ann start) e

and = label "and" $ f <$> reserved "and" <*> termLeaf <*> termLeaf
  where f kw x y = Term.and (ann kw <> ann y) x y

or = label "or" $ f <$> reserved "or" <*> termLeaf <*> termLeaf
  where f kw x y = Term.or (ann kw <> ann y) x y

var :: Var v => L.Token v -> AnnotatedTerm v Ann
var t = Term.var (ann t) (L.payload t)

seqOp :: Var v => P v Pattern.SeqOp
seqOp =
  (Pattern.Snoc <$ matchToken (L.SymbolyId ":+" Nothing))
  <|> (Pattern.Cons <$ matchToken (L.SymbolyId "+:" Nothing))
  <|> (Pattern.Concat <$ matchToken (L.SymbolyId "++" Nothing))

term4 :: Var v => TermP v
term4 = f <$> some termLeaf
  where
    f (func:args) = Term.apps func ((\a -> (ann func <> ann a, a)) <$> args)
    f [] = error "'some' shouldn't produce an empty list"

-- e.g. term4 + term4 - term4
infixApp = label "infixApp"
  $ chainl1 term4 (f <$> fmap var (infixVar <* optional semi))
  where f op lhs rhs = Term.apps op [(ann lhs, lhs), (ann rhs, rhs)]

typedecl :: Var v => P v (L.Token v, AnnotatedType v Ann)
typedecl =
  (,) <$> P.try (prefixVar <* reserved ":")
      <*> TypeParser.valueType
      <* semi

verifyRelativeName :: Var v => P v (L.Token v) -> P v (L.Token v)
verifyRelativeName name = do
  name <- name
  verifyRelativeName' name
  pure name

verifyRelativeName' :: Var v => L.Token v -> P v ()
verifyRelativeName' name = do
  let txt = Var.name . L.payload $ name
  when (Text.isPrefixOf "." txt && txt /= ".") $ do
    _ <- P.optional anyToken -- commits to the failure
    P.customFailure (DisallowedAbsoluteName name)

binding :: forall v. Var v => P v ((Ann, v), AnnotatedTerm v Ann)
binding = label "binding" $ do
  typ <- optional typedecl
  let infixLhs = do
        (arg1, op) <- P.try ((,) <$> prefixVar <*> infixVar)
        arg2 <- prefixVar
        pure (ann arg1, op, [arg1, arg2])
  let prefixLhs = do
        v  <- prefixVar
        vs <- many prefixVar
        pure (ann v, v, vs)
  let
    lhs :: P v (Ann, L.Token v, [L.Token v])
    lhs = infixLhs <|> prefixLhs
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (loc, name, args) <- P.try (lhs <* P.lookAhead (openBlockWith "="))
      body <- block "="
      verifyRelativeName' name
      pure $ mkBinding loc (L.payload name) args body
    Just (nameT, typ) -> do
      (_, name, args) <- lhs
      verifyRelativeName' name
      when (L.payload name /= L.payload nameT) $
        customFailure $ SignatureNeedsAccompanyingBody nameT
      body <- block "="
      pure $ fmap (\e -> Term.ann (ann nameT <> ann e) e typ)
                  (mkBinding (ann nameT) (L.payload name) args body)
  where
  mkBinding loc f [] body = ((loc, f), body)
  mkBinding loc f args body =
    ((loc, f), Term.lam' (loc <> ann body) (L.payload <$> args) body)


customFailure :: P.MonadParsec e s m => e -> m a
customFailure = P.customFailure

block :: forall v. Var v => String -> TermP v
block s = block' False s (openBlockWith s) closeBlock

importp :: Var v => P v [(v, v)]
importp = do
  _        <- reserved "use"
  prefix   <- wordyId <|> dotId
  suffixes <- some (wordyId <|> symbolyId) P.<?> "one or more identifiers"
  pure $ do
    let v = Var.nameds (L.payload prefix)
    s <- suffixes
    let suffix = Var.nameds . L.payload $ s
    pure (suffix, Var.joinDot v suffix)

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
  name <- verifyRelativeName prefixVar
  let statement = (Binding <$> binding) <|> namespaceBlock
  _ <- openBlockWith "where"
  elems <- sepBy semi statement
  _ <- closeBlock
  pure $ Namespace (Var.nameStr $ L.payload name) elems

toBindings :: forall v . Var v => [BlockElement v] -> [((Ann,v), AnnotatedTerm v Ann)]
toBindings b = let
  expand (Binding ((a, v), e)) = [((a, Just v), e)]
  expand (Action e) = [((ann e, Nothing), e)]
  expand (Namespace name bs) = scope name $ expand =<< bs
  v `orBlank` i = fromMaybe (Var.nameds $ "_" ++ show i) v
  finishBindings bs =
    [((a, v `orBlank` i), e) | (((a,v), e), i) <- bs `zip` [(1::Int)..]]

  scope :: String -> [((Ann, Maybe v), AnnotatedTerm v Ann)]
                  -> [((Ann, Maybe v), AnnotatedTerm v Ann)]
  scope name bs = let
    vs :: [Maybe v]
    vs = snd . fst <$> bs
    prefix :: v -> v
    prefix v = Var.named (Text.pack name `mappend` "." `mappend` Var.name v)
    vs' :: [Maybe v]
    vs' = fmap prefix <$> vs
    substs = [ (v, Term.var () v') | (Just v, Just v') <- vs `zip` vs' ]
    sub = ABT.substsInheritAnnotation substs
    in [ ((a, v'), sub e) | (((a,_),e), v') <- bs `zip` vs' ]
  in finishBindings (expand =<< b)

topLevelBlock
  :: forall v b . Var v => String -> P v (L.Token ()) -> P v b -> TermP v
topLevelBlock = block' True

imports :: Var v => P v (Names, AnnotatedTerm v Ann -> AnnotatedTerm v Ann)
imports = do
  let sem = P.try (semi <* P.lookAhead (reserved "use"))
  imported <- mconcat . reverse <$> sepBy sem importp
  env      <- Names.importing imported <$> asks snd
  let importTerms  = [ (n, Term.var () qn) | (n, qn) <- imported ]
      substImports = ABT.substsInheritAnnotation importTerms
        . Term.typeMap (Names.bindType env)
  pure (env, substImports)

block'
  :: forall v b
   . Var v
  => IsTop
  -> String
  -> P v (L.Token ())
  -> P v b
  -> TermP v
block' isTop s openBlock closeBlock = do
    open <- openBlock
    (env, substImports) <- imports
    uniqueNames <- asks fst
    _ <- optional semi
    statements <- local (const (uniqueNames, env)) $ sepBy semi statement
    _ <- closeBlock
    substImports <$> go open statements
  where
    statement = namespaceBlock <|>
      asum [ Binding <$> binding, Action <$> blockTerm ]
    go :: L.Token () -> [BlockElement v] -> P v (AnnotatedTerm v Ann)
    go open bs
      = let
          startAnnotation = (fst . fst . head $ toBindings bs)
          endAnnotation   = (fst . fst . last $ toBindings bs)
        in
          case reverse bs of
            Namespace _v _ : _ -> pure $ Term.letRec
              isTop
              (startAnnotation <> endAnnotation)
              (toBindings bs)
              (Term.var endAnnotation
                        (positionalVar endAnnotation Var.missingResult)
              )
            Binding ((a, _v), _) : _ -> pure $ Term.letRec
              isTop
              (startAnnotation <> endAnnotation)
              (toBindings bs)
              (Term.var a (positionalVar endAnnotation Var.missingResult))
            Action e : bs -> pure $ Term.letRec
              isTop
              (startAnnotation <> ann e)
              (toBindings $ reverse bs)
              e
            [] -> customFailure $ EmptyBlock (const s <$> open)

number :: Var v => TermP v
number = number' (tok Term.int) (tok Term.nat) (tok Term.float)

number'
  :: Var v
  => (L.Token Int64 -> a)
  -> (L.Token Word64 -> a)
  -> (L.Token Double -> a)
  -> P v a
number' i u f = fmap go numeric
 where
  go num@(L.payload -> p) | '.' `elem` p    = f (read <$> num)
                          | take 1 p == "+" = i (read . drop 1 <$> num)
                          | take 1 p == "-" = i (read <$> num)
                          | otherwise       = u (read <$> num)

tupleOrParenthesizedTerm :: Var v => TermP v
tupleOrParenthesizedTerm = label "tuple" $ tupleOrParenthesized term DD.unitTerm pair
  where
    pair t1 t2 =
      Term.app (ann t1 <> ann t2)
        (Term.app (ann t1)
                  (Term.constructor (ann t1 <> ann t2) DD.pairRef 0)
                  t1)
        t2
