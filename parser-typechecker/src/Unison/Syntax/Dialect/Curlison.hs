{-# LANGUAGE OverloadedStrings #-}

-- | The Curlison /renderer/: 'Surface' -> @Pretty SyntaxText@.
--
-- A curly-brace, comma-and-parens dialect: function application is @f(a, b)@, lambdas are @(a, b) => body@, @if@ is a
-- ternary @c ? t : e@, blocks are @{ x = e; body }@, and @match@ is @match (s) { pat => body }@. Like the other
-- dialects it is a pure tree-walk over 'Surface' (all resolution\/hygiene happened in
-- 'Unison.Syntax.Surface.Lower'). Binary symbolic operators render infix with /minimal/ parentheses driven by the
-- operator precedence carried on 'SBinOp' (e.g. @a + b * c@, @(a + b) * c@); the matching parser
-- ('Unison.Syntax.Dialect.Curlison.Parser') climbs the same precedence table so the two round-trip.
module Unison.Syntax.Dialect.Curlison
  ( prettyTerm,
    prettyBinding,
    prettyBindingWithoutTypeSignature,
    prettyType,
    prettySignatures,
    prettyDecl,
    prettyDeclW,
  )
where

import Control.Monad.Writer (Writer)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Unison.DataDeclaration (Decl)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.Reference (Reference, TypeReference)
import Unison.Referent (Referent)
import Unison.Syntax.DeclPrinter (AccessorName, RenderUniqueTypeGuids)
import Unison.Syntax.NamePrinter (prettyHashQualified, prettyName)
import Unison.Syntax.Precedence (InfixPrecedence (Level), Precedence (Application, Bottom, InfixOp), increment, operatorPrecedence)
import Unison.Syntax.Surface
import Unison.Syntax.Surface.Lower qualified as Lower
import Unison.Term (Term2)
import Unison.Type (Type)
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as PP
import Unison.Util.SyntaxText qualified as S
import Unison.Var (Var)

type SyntaxText = S.SyntaxText' Reference

fmt :: S.Element Reference -> Pretty SyntaxText -> Pretty SyntaxText
fmt = PP.withSyntax

ctrl :: Pretty SyntaxText -> Pretty SyntaxText
ctrl = fmt S.ControlKeyword

parens :: Pretty SyntaxText -> Pretty SyntaxText
parens p = fmt S.Parenthesis "(" <> p <> fmt S.Parenthesis ")"

commas :: [Pretty SyntaxText] -> Pretty SyntaxText
commas = PP.sep (fmt S.DelimiterChar ", ")

renderName :: SName -> Pretty SyntaxText
renderName = prettyHashQualified

renderPlain :: Name -> Pretty SyntaxText
renderPlain n = fmt S.Var (prettyName n)

renderLit :: SLit -> Pretty SyntaxText
renderLit = \case
  SInt i -> fmt S.NumericLiteral (PP.string ((if i >= 0 then "+" else "") <> show i))
  SNat n -> fmt S.NumericLiteral (PP.string (show n))
  SFloat f -> fmt S.NumericLiteral (PP.string (show f))
  SBool b -> fmt S.BooleanLiteral (if b then "true" else "false")
  SText t -> fmt S.TextLiteral (PP.string (show t))
  SChar c -> fmt S.CharLiteral (PP.string ('\'' : c : "'"))

-- Terms ---------------------------------------------------------------------------------------------------------------

-- | Precedences for the two built-in boolean operators (looked up the same way the parser does).
andPrec, orPrec :: Precedence
andPrec = fromMaybe (InfixOp (Level 1)) (operatorPrecedence "&&")
orPrec = fromMaybe (InfixOp (Level 0)) (operatorPrecedence "||")

-- | Render a term at the top level (no surrounding operator), i.e. with the loosest ambient precedence.
renderTerm :: STerm -> Pretty SyntaxText
renderTerm = renderTermP Bottom

-- | Render a term whose result sits in a context of the given ambient precedence. Infix operators wrap themselves in
-- parentheses exactly when their own precedence is looser than the context (and the right operand is rendered one level
-- tighter, so equal-precedence chains stay left-associative on re-parse). All other forms are self-delimiting.
renderTermP :: Precedence -> STerm -> Pretty SyntaxText
renderTermP ctx (STerm _ f) = case f of
  SLit l -> renderLit l
  SName n -> renderName n
  SBinOp n p a b -> wrapIf (p < ctx) (infixOp p (renderName n) a b)
  SAnd a b -> wrapIf (andPrec < ctx) (infixOp andPrec "&&" a b)
  SOr a b -> wrapIf (orPrec < ctx) (infixOp orPrec "||" a b)
  SApp h args -> renderTermP Application h <> parens (commas (map renderTerm args))
  SLam ps body -> parens (commas [renderPlain p | SParam _ p <- ps]) <> " " <> fmt S.ControlKeyword "=>" <> " " <> renderTerm body
  SLet bs body -> block bs body
  SLetRec bs body -> block bs body
  SIf c t e -> parens (renderTerm c <> " ? " <> renderTerm t <> " : " <> renderTerm e)
  SMatch s cs -> ctrl "match" <> " " <> parens (renderTerm s) <> " " <> braceBlock (map renderCase cs)
  SHandle h e -> ctrl "handle" <> " " <> parens (renderTerm e) <> " " <> ctrl "with" <> " " <> parens (renderTerm h)
  SDelay e -> ctrl "delay" <> parens (renderTerm e)
  SList xs -> fmt S.DelimiterChar "[" <> commas (map renderTerm xs) <> fmt S.DelimiterChar "]"
  STuple xs -> parens (commas (map renderTerm xs))
  SAnn e t -> parens (renderTerm e <> " : " <> renderType t)
  SHole -> fmt S.Blank "_"
  STermLink n -> ctrl "termLink" <> parens (renderName n)
  STypeLink n -> ctrl "typeLink" <> parens (renderName n)
  SDocLit t -> fmt S.DocDelimiter (PP.text t)
  where
    wrapIf cond p = if cond then parens p else p
    infixOp p opName a b = renderTermP p a <> " " <> opName <> " " <> renderTermP (increment p) b

-- | A @{ name = e; … ; body }@ block.
block :: [SBinding] -> STerm -> Pretty SyntaxText
block bs body =
  braceBlock ([renderPlain (bName b) <> " " <> fmt S.BindingEquals "=" <> " " <> renderTerm (bValue b) | b <- bs] ++ [renderTerm body])

braceBlock :: [Pretty SyntaxText] -> Pretty SyntaxText
braceBlock items =
  fmt S.DelimiterChar "{"
    <> PP.indentN 2 (PP.newline <> PP.lines (map (<> fmt S.DelimiterChar ";") items))
    <> PP.newline
    <> fmt S.DelimiterChar "}"

renderCase :: SCase -> Pretty SyntaxText
renderCase (SCase pat guard body) =
  let guardP = maybe mempty (\g -> " " <> ctrl "if" <> " " <> renderTerm g) guard
   in renderPattern pat <> guardP <> " " <> fmt S.ControlKeyword "=>" <> " " <> renderTerm body

renderPattern :: SPattern -> Pretty SyntaxText
renderPattern (SPattern _ p) = case p of
  SPWild -> fmt S.DelimiterChar "_"
  SPVar n -> renderPlain n
  SPLit l -> renderLit l
  SPCtor n [] -> renderName n
  SPCtor n subs -> renderName n <> parens (commas (map renderPattern subs))
  SPAs n sub -> renderPlain n <> fmt S.DelimiterChar "@" <> renderPattern sub
  SPList subs -> fmt S.DelimiterChar "[" <> commas (map renderPattern subs) <> fmt S.DelimiterChar "]"
  SPSeqOp l op r -> parens (renderPattern l <> " " <> seqOp op <> " " <> renderPattern r)
  SPEffect n subs k -> fmt S.DelimiterChar "{" <> renderName n <> parens (commas (map renderPattern subs)) <> " -> " <> renderPattern k <> fmt S.DelimiterChar "}"
  SPEffectPure sub -> fmt S.DelimiterChar "{" <> renderPattern sub <> fmt S.DelimiterChar "}"
  where
    seqOp = \case SCons -> "+:"; SSnoc -> ":+"; SConcat -> "++"

-- Types ---------------------------------------------------------------------------------------------------------------

renderType :: SType -> Pretty SyntaxText
renderType st@(SType _ t) = case t of
  STyVar n -> renderPlain n
  STyRef n -> renderName n
  STyForall vs body -> ctrl "forall" <> fmt S.DelimiterChar "<" <> commas (map renderPlain vs) <> fmt S.DelimiterChar ">" <> " " <> renderType body
  STyApp f args -> renderType f <> fmt S.DelimiterChar "<" <> commas (map renderType args) <> fmt S.DelimiterChar ">"
  STyEffects es -> renderEffects es
  STyArrow {} -> parens (PP.sep (" " <> fmt S.TypeOperator "->" <> " ") (arrowComponents st))
  where
    arrowComponents (SType _ (STyArrow i Nothing o)) = renderType i : arrowComponents o
    arrowComponents (SType _ (STyArrow i (Just es) o)) = (renderType i <> renderEffects es) : arrowComponents o
    arrowComponents other = [renderType other]

renderEffects :: [SType] -> Pretty SyntaxText
renderEffects es = fmt S.AbilityBraces "{" <> commas (map renderType es) <> fmt S.AbilityBraces "}"

-- Declarations --------------------------------------------------------------------------------------------------------

-- | @structural type Color { Red; Green; Blue }@ / @type List<a> { Nil; Cons(a, List<a>) }@ /
-- @record Point { x : Nat; y : Nat }@.
renderSDecl :: SDecl -> Pretty SyntaxText
renderSDecl sd
  | Just fields <- recordFields sd =
      headerWith (ctrl "record") <> " " <> braceBlock [renderPlain f <> " : " <> renderType t | (f, t) <- fields]
  | otherwise =
      headerWith kw <> " " <> braceBlock (map (renderCtor (dIsAbility sd)) (dConstructors sd))
  where
    kw = if dIsAbility sd then ctrl "ability" else ctrl "type"
    tyParams = case dTypeParams sd of
      [] -> mempty
      vs -> fmt S.DelimiterChar "<" <> commas (map renderPlain vs) <> fmt S.DelimiterChar ">"
    headerWith k = renderModifier (dModifier sd) <> k <> " " <> renderPlain (dName sd) <> tyParams

-- | When a declaration is a record, pair each field name with its (positional) field type.
recordFields :: SDecl -> Maybe [(Name, SType)]
recordFields sd = do
  fields <- dFields sd
  ctor <- case dConstructors sd of [c] -> Just c; _ -> Nothing
  pure (zip fields (ctorArgTypes (cType ctor)))

-- | Renders the modifier (with a trailing space). @unique@ is the default, so unique types render no modifier and no
-- GUID — matching the regular syntax; the GUID is recovered by name on re-parse.
renderModifier :: SModifier -> Pretty SyntaxText
renderModifier = \case
  SStructural -> fmt S.DataTypeModifier "structural" <> " "
  SUnique _ -> mempty

renderCtor :: Bool -> SConstructor -> Pretty SyntaxText
renderCtor isAbility (SConstructor _ name ty)
  | isAbility = renderPlain name <> " : " <> renderType ty
  | otherwise = case ctorArgTypes ty of
      [] -> renderPlain name
      args -> renderPlain name <> parens (commas (map renderType args))

ctorArgTypes :: SType -> [SType]
ctorArgTypes (SType _ (STyArrow i _ o)) = i : ctorArgTypes o
ctorArgTypes _ = []

-- Dialect-facing entry points -----------------------------------------------------------------------------------------

prettyTerm :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Pretty SyntaxText
prettyTerm ppe = renderTerm . Lower.lowerTerm ppe

prettyType :: (Var v) => PrettyPrintEnv -> Type v a -> Pretty SyntaxText
prettyType ppe = renderType . Lower.lowerType ppe

-- | Render @name : type@ signatures (for @find@\/slurp).
prettySignatures :: (Var v) => PrettyPrintEnv -> [(Referent, HQ.HashQualified Name, Type v a)] -> [Pretty SyntaxText]
prettySignatures ppe ts =
  [prettyHashQualified n <> " " <> fmt S.TypeAscriptionColon ":" <> " " <> renderType (Lower.lowerType ppe t) | (_, n, t) <- ts]

prettyDecl :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Pretty SyntaxText
prettyDecl pped _guid r hq decl = renderSDecl (Lower.lowerDecl pped r hq decl)

prettyDeclW :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Writer (Set AccessorName) (Pretty SyntaxText)
prettyDeclW pped guid r hq decl = pure (prettyDecl pped guid r hq decl)

-- | A Curlison (curly-brace) binding:
--
--   * a typed function (a typed lambda) becomes @RetType name(ArgType a, …) { …; return body; }@,
--   * a typed value becomes @Type name = value;@, and
--   * an untyped value (rare; the type is normally known) becomes @name = value;@.
prettyBinding :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBinding ppe hq term =
  case Lower.lowerTerm ppe term of
    STerm _ (SAnn (STerm _ (SLam params body)) ty)
      | Just (argTys, retTy) <- splitArrowPure (length params) ty ->
          cFunction retTy params argTys body
    STerm _ (SAnn e ty) -> renderType ty <> " " <> defAssign hq e <> semi
    s -> defAssign hq s <> semi
  where
    cFunction retTy params argTys body =
      renderType retTy
        <> " "
        <> prettyHashQualified hq
        <> parens (commas [renderType t <> " " <> renderPlain p | (SParam _ p, t) <- zip params argTys])
        <> " "
        <> braceBlock (funcStmts body)
    funcStmts body = case body of
      STerm _ (SLet bs e) -> map stmtBinding bs ++ [ret e]
      STerm _ (SLetRec bs e) -> map stmtBinding bs ++ [ret e]
      e -> [ret e]
    ret e = ctrl "return" <> " " <> renderTerm e
    stmtBinding b = renderPlain (bName b) <> " " <> fmt S.BindingEquals "=" <> " " <> renderTerm (bValue b)
    semi = fmt S.DelimiterChar ";"

-- | Peel exactly @n@ pure (effect-free) argument arrows off a type, returning the argument types and the result type.
-- 'Nothing' if the type doesn't have @n@ such arrows (e.g. an effectful function) — those fall back to a value binding.
splitArrowPure :: Int -> SType -> Maybe ([SType], SType)
splitArrowPure 0 ty = Just ([], ty)
splitArrowPure n (SType _ (STyArrow i Nothing o))
  | n > 0 = do
      (args, ret) <- splitArrowPure (n - 1) o
      pure (i : args, ret)
splitArrowPure _ _ = Nothing

prettyBindingWithoutTypeSignature :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBindingWithoutTypeSignature ppe hq term = defAssign hq (peelAnn (Lower.lowerTerm ppe term))
  where
    peelAnn = \case STerm _ (SAnn e _) -> e; s -> s

defAssign :: HQ.HashQualified Name -> STerm -> Pretty SyntaxText
defAssign hq s =
  prettyHashQualified hq <> " " <> fmt S.BindingEquals "=" <> " " <> renderTerm s
