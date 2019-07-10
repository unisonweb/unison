{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.TermParser where

import           Control.Applicative
import           Control.Monad (join, when)
import           Control.Monad.Reader (asks, local)
import           Data.Foldable (asum)
import           Data.Functor
import           Data.Int (Int64)
import           Data.List (elem)
import           Data.Maybe (isJust, fromMaybe)
import           Data.Word (Word64)
import           Prelude hiding (and, or, seq)
import           Unison.Name (Name)
import           Unison.Names3 (Names)
import           Unison.Parser hiding (seq)
import           Unison.PatternP (Pattern)
import           Unison.Term (AnnotatedTerm, IsTop)
import           Unison.Type (AnnotatedType)
import           Unison.Var (Var)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Unison.ABT as ABT
import qualified Unison.DataDeclaration as DD
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer as L
import qualified Unison.Name as Name
import qualified Unison.Names3 as Names
import qualified Unison.Parser as Parser (seq)
import qualified Unison.PatternP as Pattern
import qualified Unison.Term as Term
import qualified Unison.Type as Type
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
  -- Foo x@(Blah 10)
  varOrAs :: P v (Pattern Ann, [(Ann, v)])
  varOrAs = do
    v <- wordyPatternName
    o <- optional (reserved "@")
    if isJust o then
      (\(p, vs) -> (Pattern.As (ann v) p, tokenToPair v : vs)) <$> leaf
      else pure (Pattern.Var (ann v), [tokenToPair v])
  unbound :: P v (Pattern Ann, [(Ann, v)])
  unbound = (\tok -> (Pattern.Unbound (ann tok), [])) <$> blank
  ctor :: _ -> P v (L.Token (Reference, Int))
  ctor err = do
    -- this might be a var, so we avoid consuming it at first
    tok <- P.try (P.lookAhead hqPrefixId)
    names <- asks names
    case Names.lookupHQPattern (L.payload tok) names of
      s | Set.null s     -> die tok s
        | Set.size s > 1 -> die tok s
        | otherwise      -> -- matched ctor name, consume the token
                            do anyToken; pure (Set.findMin s <$ tok)
    where
    die hq s = case L.payload hq of
      -- if token not hash qualified, fail w/out consuming it to allow backtracking
      HQ.NameOnly n | Set.null s -> fail $ "not a constructor name: " <> show n
      -- it was hash qualified, and wasn't found in the env, that's a failure!
      _ -> failCommitted $ err hq s

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
    tok <- ctor UnknownDataConstructor
    let (ref,cid) = L.payload tok
        f patterns vs =
          let loc = foldl (<>) (ann tok) $ map ann patterns
          in (Pattern.Constructor loc ref cid patterns, vs)
    unzipPatterns f <$> many leaf

  seqLiteral = Parser.seq f leaf
    where f loc = unzipPatterns ((,) . Pattern.SequenceLiteral loc)

lam :: Var v => TermP v -> TermP v
lam p = label "lambda" $ mkLam <$> P.try (some prefixDefinitionName <* reserved "->") <*> p
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
hashQualifiedPrefixTerm = resolveHashQualified =<< hqPrefixId

hashQualifiedInfixTerm :: Var v => TermP v
hashQualifiedInfixTerm = resolveHashQualified =<< hqInfixId

-- If the hash qualified is name only, it is treated as a var, if it
-- has a short hash, we resolve that short hash immediately and fail
-- committed if that short hash can't be found in the current environment
resolveHashQualified :: Var v => L.Token HQ.HashQualified -> TermP v
resolveHashQualified tok = do
  names <- asks names
  case Names.lookupHQTerm (L.payload tok) names of
    s | Set.null s     -> die tok s
      | Set.size s > 1 -> die tok s
      | otherwise      -> pure $ Term.fromReferent (ann tok) (Set.findMin s)
    where
      die hq s = case L.payload hq of
       HQ.NameOnly n -> pure $ Term.var (ann tok) (Name.toVar n)
       _ -> failCommitted $ UnknownTerm hq s

termLeaf :: forall v . Var v => TermP v
termLeaf = do
  e <- asum
    [ hashQualifiedPrefixTerm
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

seqOp :: Ord v => P v Pattern.SeqOp
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
  $ chainl1 term4 (f <$> (hashQualifiedInfixTerm <* optional semi))
  where f op lhs rhs = Term.apps op [(ann lhs, lhs), (ann rhs, rhs)]

typedecl :: Var v => P v (L.Token v, AnnotatedType v Ann)
typedecl =
  (,) <$> P.try (prefixDefinitionName <* reserved ":")
      <*> TypeParser.valueType
      <* semi

verifyRelativeVarName :: Var v => P v (L.Token v) -> P v (L.Token v)
verifyRelativeVarName p = do
  v <- p
  verifyRelativeName' (Name.fromVar <$> v)
  pure v

verifyRelativeName :: Ord v => P v (L.Token Name) -> P v (L.Token Name)
verifyRelativeName name = do
  name <- name
  verifyRelativeName' name
  pure name

verifyRelativeName' :: Ord v => L.Token Name -> P v ()
verifyRelativeName' name = do
  let txt = Name.toText . L.payload $ name
  when (Text.isPrefixOf "." txt && txt /= ".") $ do
    failCommitted (DisallowedAbsoluteName name)

binding :: forall v. Var v => P v ((Ann, v), AnnotatedTerm v Ann)
binding = label "binding" $ do
  typ <- optional typedecl
  -- a ++ b = ... OR
  -- foo `mappend` bar = ...
  let infixLhs = do
        (arg1, op) <- P.try $
          (,) <$> prefixDefinitionName <*> infixDefinitionName
        arg2 <- prefixDefinitionName
        pure (ann arg1, op, [arg1, arg2])
  let prefixLhs = do
        v  <- prefixDefinitionName
        vs <- many prefixDefinitionName
        pure (ann v, v, vs)
  let
    lhs :: P v (Ann, L.Token v, [L.Token v])
    lhs = infixLhs <|> prefixLhs
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (loc, name, args) <- P.try (lhs <* P.lookAhead (openBlockWith "="))
      body <- block "="
      verifyRelativeName' (fmap Name.fromVar name)
      pure $ mkBinding loc (L.payload name) args body
    Just (nameT, typ) -> do
      (_, name, args) <- lhs
      verifyRelativeName' (fmap Name.fromVar name)
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

-- example: use Foo.bar.Baz + ++ x
-- todo: doesn't support use Foo.bar ++#abc, which lets you use `++` unqualified to refer to `Foo.bar.++#abc`
importp :: Ord v => P v [(Name, Name)]
importp = do
  _        <- reserved "use"
  prefix   <- fmap L.payload $ importWordyId <|> importDotId -- use . Nat
  suffixes <- some (importWordyId <|> importSymbolyId)
              P.<?> "one or more identifiers"
  pure $ do
    suffix <- L.payload <$> suffixes
    pure (suffix, Name.joinDot prefix suffix)

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
  -- need a version of verifyRelativeName that takes a `Token Name`
  name <- verifyRelativeName importWordyId
  let statement = (Binding <$> binding) <|> namespaceBlock
  _ <- openBlockWith "where"
  elems <- sepBy semi statement
  _ <- closeBlock
  pure $ Namespace (Name.toString $ L.payload name) elems

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

-- subst
-- use Foo.Bar + blah
-- use Bar.Baz zonk zazzle
imports :: Var v => P v (Names, [(v,v)])
imports = do
  let sem = P.try (semi <* P.lookAhead (reserved "use"))
  imported <- mconcat . reverse <$> sepBy sem importp
  ns' <- Names.importing imported <$> asks names
  pure (ns', [(Name.toVar suffix, Name.toVar full) | (suffix,full) <- imported ])

substImports :: Var v => [(v,v)] -> AnnotatedTerm v Ann -> AnnotatedTerm v Ann
substImports imports =
  ABT.substsInheritAnnotation [ (suffix, Term.var () full) | (suffix,full) <- imports ] .
  Term.substTypeVars [ (suffix, Type.var () full) | (suffix, full) <- imports ]

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
    (names, imports) <- imports
    _ <- optional semi
    statements <- local (\e -> e { names = names } ) $ sepBy semi statement
    _ <- closeBlock
    substImports imports <$> go open statements
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
  :: Ord v
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
