{-# LANGUAGE OverloadedStrings #-}

-- | The Pyson-like /renderer/: 'Surface' -> @Pretty SyntaxText@.
--
-- Indentation-significant. Function bodies, @let@, and @match@ introduce an indented block after a colon; @if@ is the
-- Pyson conditional expression @t if c else e@; lambdas are @lambda x, y: body@; application is @f(a, b)@. Like the
-- other dialects it is a pure tree-walk over 'Surface'.
module Unison.Syntax.Dialect.Pyson
  ( prettyTerm,
    prettyBinding,
    prettyBindingWithoutTypeSignature,
    prettyType,
    prettySignatures,
    prettyDecl,
    prettyDeclW,
    prettyDoc2,
  )
where

import Control.Monad.Writer (Writer)
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
import Unison.Syntax.Precedence (Precedence (Application, Bottom), increment)
import Unison.Syntax.Surface
import Unison.Syntax.Surface.Lower qualified as Lower
import Unison.Syntax.TermPrinter qualified as TermPrinter
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
  SBool b -> fmt S.BooleanLiteral (if b then "True" else "False")
  SText t -> fmt S.TextLiteral (PP.string (show t))
  SChar c -> fmt S.CharLiteral (PP.string ('\'' : c : "'"))

-- | An indented block: a colon-introduced suite of lines.
suite :: Pretty SyntaxText -> [Pretty SyntaxText] -> Pretty SyntaxText
suite header items = header <> fmt S.DelimiterChar ":" <> PP.indentN 2 (PP.newline <> PP.lines items)

-- | Render a term at the top level (loosest ambient precedence).
renderTerm :: STerm -> Pretty SyntaxText
renderTerm = renderTermP Bottom

-- | Render a term in a context of the given ambient precedence. Symbolic infix operators wrap themselves in
-- parentheses only when their precedence is looser than the context (with the right operand one level tighter, keeping
-- equal-precedence chains left-associative on re-parse). The keyword forms (@and@\/@or@\/conditional) stay
-- parenthesized, matching the parser, which only accepts them inside parentheses.
renderTermP :: Precedence -> STerm -> Pretty SyntaxText
renderTermP ctx (STerm _ f) = case f of
  SLit l -> renderLit l
  SName n -> renderName n
  SBinOp n p a b -> (if p < ctx then parens else id) (renderTermP p a <> " " <> renderName n <> " " <> renderTermP (increment p) b)
  SApp h args -> renderTermP Application h <> parens (commas (map renderTerm args))
  SLam ps body -> ctrl "lambda" <> " " <> commas [renderPlain p | SParam _ p <- ps] <> fmt S.DelimiterChar ":" <> " " <> renderTerm body
  SLet bs body -> suite (ctrl "let") (map renderLetBinding bs ++ [renderTerm body])
  SLetRec bs body -> suite (ctrl "letrec") (map renderLetBinding bs ++ [renderTerm body])
  SIf c t e -> parens (renderTerm t <> " " <> ctrl "if" <> " " <> renderTerm c <> " " <> ctrl "else" <> " " <> renderTerm e)
  SAnd a b -> parens (renderTerm a <> " " <> ctrl "and" <> " " <> renderTerm b)
  SOr a b -> parens (renderTerm a <> " " <> ctrl "or" <> " " <> renderTerm b)
  SMatch s cs -> suite (ctrl "match" <> " " <> renderTerm s) (map renderCase cs)
  SHandle h e -> suite (ctrl "handle" <> " " <> renderTerm e <> " " <> ctrl "with") [renderTerm h]
  SDelay e -> ctrl "delay" <> parens (renderTerm e)
  SList xs -> fmt S.DelimiterChar "[" <> commas (map renderTerm xs) <> fmt S.DelimiterChar "]"
  STuple xs -> parens (commas (map renderTerm xs))
  SAnn e t -> parens (renderTerm e <> " : " <> renderType t)
  SHole -> fmt S.Blank "_"
  STermLink n -> ctrl "termLink" <> parens (renderName n)
  STypeLink n -> ctrl "typeLink" <> parens (renderName n)
  SDocLit t -> fmt S.DocDelimiter (PP.text t)

-- | A binding inside a @let@\/@letrec@ block. A function-valued binding becomes a nested @def@ (Python's @lambda@ is
-- expression-only, so a multi-statement function body can't be a @lambda@); a plain value becomes @name = value@.
renderLetBinding :: SBinding -> Pretty SyntaxText
renderLetBinding b = case bValue b of
  STerm _ (SLam ps body) ->
    suite
      (ctrl "def" <> " " <> renderPlain (bName b) <> parens (commas [renderPlain p | SParam _ p <- ps]))
      [renderTerm body]
  v -> renderPlain (bName b) <> " " <> fmt S.BindingEquals "=" <> " " <> renderTerm v

renderCase :: SCase -> Pretty SyntaxText
renderCase (SCase pat guard body) =
  let guardP = maybe mempty (\g -> " " <> ctrl "if" <> " " <> renderTerm g) guard
   in suite (ctrl "case" <> " " <> renderPattern pat <> guardP) [renderTerm body]

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

renderType :: SType -> Pretty SyntaxText
renderType st@(SType _ t) = case t of
  STyVar n -> renderPlain n
  STyRef n -> renderName n
  STyForall vs body -> ctrl "forall" <> " " <> commas (map renderPlain vs) <> fmt S.DelimiterChar "." <> " " <> renderType body
  STyApp f args -> renderType f <> fmt S.DelimiterChar "[" <> commas (map renderType args) <> fmt S.DelimiterChar "]"
  STyEffects es -> renderEffects es
  STyArrow {} -> arrowSpine st
  where
    -- Effects sit on the arrow: `a ->{e} b`, not `a{e} -> b`.
    arrowSpine (SType _ (STyArrow i mes o)) =
      arrowInput i <> " " <> fmt S.TypeOperator "->" <> maybe mempty renderEffects mes <> " " <> arrowSpine o
    arrowSpine other = renderType other
    -- A function-typed argument needs parens so it doesn't reassociate: `(a -> b) -> c`, not `a -> b -> c`.
    arrowInput t@(SType _ (STyArrow {})) = parens (renderType t)
    arrowInput t = renderType t

renderEffects :: [SType] -> Pretty SyntaxText
renderEffects es = fmt S.AbilityBraces "{" <> commas (map renderType es) <> fmt S.AbilityBraces "}"

-- Declarations --------------------------------------------------------------------------------------------------------

renderSDecl :: SDecl -> Pretty SyntaxText
renderSDecl sd
  | Just fields <- recordFields sd =
      suite (headerWith (ctrl "record")) [renderPlain f <> " : " <> renderType t | (f, t) <- fields]
  | otherwise =
      suite (headerWith kw) (map (renderCtor (dIsAbility sd)) (dConstructors sd))
  where
    kw = if dIsAbility sd then ctrl "ability" else ctrl "type"
    tyParams = case dTypeParams sd of
      [] -> mempty
      vs -> fmt S.DelimiterChar "[" <> commas (map renderPlain vs) <> fmt S.DelimiterChar "]"
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

-- | Render terms embedded in docs (code blocks) in this dialect, recursively (so nested docs stay in-dialect too).
docRender :: PrettyPrintEnv -> TermPrinter.DialectTermRenderer
docRender ppe = TermPrinter.DialectTermRenderer (\t -> renderTerm (Lower.lowerTermD (Just (docRender ppe)) ppe t))

-- | Lower a term with this dialect's doc-code renderer in effect.
lowerT :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> STerm
lowerT ppe = Lower.lowerTermD (Just (docRender ppe)) ppe

prettyTerm :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Pretty SyntaxText
prettyTerm ppe = renderTerm . lowerT ppe

-- | Render a Doc2 term as a doc literal, with embedded code in this dialect.
prettyDoc2 :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Maybe (Pretty SyntaxText)
prettyDoc2 ppe = TermPrinter.prettyDoc2With (Just (docRender ppe)) ppe

prettyType :: (Var v) => PrettyPrintEnv -> Type v a -> Pretty SyntaxText
prettyType ppe = renderType . Lower.lowerType ppe

-- | Render @name : type@ signatures (for @find@\/slurp).
prettySignatures :: (Var v) => PrettyPrintEnv -> [(Referent, HQ.HashQualified Name, Type v a)] -> [Pretty SyntaxText]
prettySignatures ppe ts = [prettyHashQualified n <> " : " <> renderType (Lower.lowerType ppe t) | (_, n, t) <- ts]

prettyDecl :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Pretty SyntaxText
prettyDecl pped _guid r hq decl = renderSDecl (Lower.lowerDecl pped r hq decl)

prettyDeclW :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Writer (Set AccessorName) (Pretty SyntaxText)
prettyDeclW pped guid r hq decl = pure (prettyDecl pped guid r hq decl)

prettyBinding :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBinding ppe hq term =
  case lowerT ppe term of
    STerm _ (SAnn e ty) -> sig ty <> PP.newline <> def e
    s -> def s
  where
    sig ty = prettyHashQualified hq <> " " <> fmt S.TypeAscriptionColon ":" <> " " <> renderType ty
    def = defForm hq

prettyBindingWithoutTypeSignature :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBindingWithoutTypeSignature ppe hq term = defForm hq (peelAnn (lowerT ppe term))
  where
    peelAnn = \case STerm _ (SAnn e _) -> e; s -> s

-- | @def name(params): <indented body>@ for functions, @name = value@ for values.
defForm :: HQ.HashQualified Name -> STerm -> Pretty SyntaxText
defForm hq s = case s of
  STerm _ (SLam ps body) ->
    suite (ctrl "def" <> " " <> prettyHashQualified hq <> parens (commas [renderPlain p | SParam _ p <- ps])) [renderTerm body]
  _ -> prettyHashQualified hq <> " " <> fmt S.BindingEquals "=" <> " " <> renderTerm s
