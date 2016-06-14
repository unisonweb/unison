module Unison.TermParser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char (isDigit, isAlpha, isSymbol, isPunctuation)
import Data.Foldable (asum, toList)
import Data.Functor (($>), void)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Unison.Parser
import Unison.Reference (Reference)
import Unison.Symbol (Symbol, Symbol(..))
import Unison.Term (Term, Literal)
import Unison.Type (Type)
import Unison.View (DFO)
import qualified Unison.ABT as ABT
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Var as Var

-- todo:
-- remove use of RefLookup from all parsers -- done
-- do the lookup at the end, following what's already done in substsTest
-- convert the TypeParsers to not use RefLookup -- done
-- resolve :: Term v -> RefLookup -> Term v
--   which replaces free term AND type variables (in embedded Ann) in the input,
--   using the provided RefLookup
--   using the Term.typeMap function to perform substitution of free variables
--   and just using ABT.substs as before to perform substitution of the term's free variables
--   so two calls to subts, one for the term, and one in the function passed to typeMap to do
--   the same substitution over the type annotations

-- use the new parsers to clean up the tests
   -- add whatever convenience functions needed to make that easier

-- returns missing type vars and term vars
allFreeVars :: Term V -> Set V
allFreeVars t = Term.freeVars t `Set.union` Term.freeTypeVars t

missingVars :: Term V -> RefLookup'' -> Set V
missingVars t l = Set.filter (Maybe.isNothing . l) (allFreeVars t)

resolve :: Term V -> RefLookup'' -> Maybe (Term V)
resolve t l =
  if Set.null (missingVars t l)
  then Just (unsafeResolve t $
    \v -> fromMaybe (error "nothing was supposed to be missing") (l v))
  else Nothing


unsafeResolve :: Term V -> (V -> Reference) -> Term V
unsafeResolve t unsafeLookup = (substTermTypes . substTerms) t
  where
    lookupTerm :: V -> (V, Term V)
    lookupTerm v = (\r -> (v, Term.ref r)) $ unsafeLookup v
    lookupType :: V -> (V, Type V)
    lookupType v = (\r -> (v, Type.ref r)) $ unsafeLookup v

    readyTerms :: [(V, Term V)]
    readyTerms = lookupTerm <$> toList (Term.freeVars t)
    readyTypes :: [(V, Type V)]
    readyTypes = lookupType <$> toList (Term.freeTypeVars t)

    substTerms :: Term V -> Term V
    substTerms = ABT.substs readyTerms
    substType :: Type V -> Type V
    substType = ABT.substs readyTypes

    substTermTypes :: Term V -> Term V
    substTermTypes = Term.typeMap substType

type V = Symbol DFO

term :: Parser (Term V)
term = term1

term1 :: Parser (Term V)
term1 = possiblyAnnotated term2

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
termLeaf = asum [lit, parenthesized term, blank, vector term, prefixTerm]

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


newline :: Parser ()
newline = void $ token (char '\n')
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

-- todo: maybe split this into operator/nonoperator constructors?
--       e.g. for operator declarations in parens  (!@#$dog) a b = ...

-- data Identifier = Symboly String | Wordy String

-- varName' :: Parser Identifier
-- varName' = token $ Symboly <$> symboly <|> Wordy <$> wordy
--   where

wordyId :: Parser String
wordyId = token $ constrainedIdentifier [isAlpha . head, (`notElem` keywords)]

symbolyId :: Parser String
symbolyId = token $ constrainedIdentifier [(\c -> isSymbol c || isPunctuation c) . head, (`notElem` keywords)]

infixVar :: Parser V
infixVar = (Var.named . Text.pack) <$> infixOp
  where
    infixOp :: Parser String
    infixOp = symbolyId <|> (char '`' *> wordyId <* token (char '`')) -- no whitespace w/in backticks


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

----- temporary stuff
type RefLookup = String -> Maybe Reference
type RefLookup' = Text.Text -> Maybe Reference
type RefLookup'' = V -> Maybe Reference

resolveAllAsBuiltin :: RefLookup
resolveAllAsBuiltin s = Just (Reference.Builtin $ Text.pack s)

resolveAllAsBuiltin' :: RefLookup'
resolveAllAsBuiltin' t = Just (Reference.Builtin t)

dogCatMouse :: RefLookup
dogCatMouse s =
  if s `elem` ["dog", "cat", "mouse", "+"] then
    Just (Reference.Builtin $ Text.pack s)
  else Nothing
