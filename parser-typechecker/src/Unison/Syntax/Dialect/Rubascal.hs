{-# LANGUAGE OverloadedStrings #-}

-- | The Rubascal /renderer/: 'Surface' -> @Pretty SyntaxText@.
--
-- A Pascal\/Ruby hybrid: functions are @def name(p, …) … end@ with an implicit return (the last expression is the
-- result, Ruby-style), pattern matching is @case s … when p then body … end@, conditionals are
-- @if c then t else e end@, value bindings use Pascal's @:=@, type ascription uses @:@, and blocks close with a bare
-- @end@. The full type of a definition rides on a preceding @name : Type@ signature line. Like the other dialects it is
-- a pure tree-walk over 'Surface'; symbolic operators render infix with minimal parentheses driven by 'SBinOp'.
module Unison.Syntax.Dialect.Rubascal
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
import Unison.Syntax.Name qualified as Name (toText)
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
  SBool b -> fmt S.BooleanLiteral (if b then "true" else "false")
  SText t -> fmt S.TextLiteral (PP.string (show t))
  SChar c -> fmt S.CharLiteral (PP.string ('\'' : c : "'"))

-- | A header line, then its body lines indented, then a closing keyword on its own line.
blockOf :: Pretty SyntaxText -> [Pretty SyntaxText] -> Pretty SyntaxText -> Pretty SyntaxText
blockOf header items ender =
  header <> PP.indentNAfterNewline 2 (PP.newline <> PP.lines items) <> PP.newline <> ender

-- | The statement separator (Pascal's @;@). Each statement in a block is terminated by it, except a nested @def@ (which
-- already ends in @end@), so the parser can tell where one statement ends and the next begins.
sep :: Pretty SyntaxText
sep = fmt S.DelimiterChar ";"

bindingStmtSep :: SBinding -> Pretty SyntaxText
bindingStmtSep b = bindingStmt b <> if isFuncBinding b then mempty else sep

isFuncBinding :: SBinding -> Bool
isFuncBinding b
  | Name.toText (bName b) == "_" = False
  | otherwise = case bValue b of
      STerm _ (SLam _ _) -> True
      STerm _ (SAnn (STerm _ (SLam _ _)) _) -> True
      _ -> False

-- Terms ---------------------------------------------------------------------------------------------------------------

renderTerm :: STerm -> Pretty SyntaxText
renderTerm = renderTermP Bottom

renderTermP :: Precedence -> STerm -> Pretty SyntaxText
renderTermP ctx (STerm _ f) = case f of
  SLit l -> renderLit l
  SName n -> renderName n
  SBinOp n p a b -> wrapIf (p < ctx) (renderTermP p a <> " " <> renderName n <> " " <> renderTermP (increment p) b)
  SAnd a b -> parens (renderTerm a <> " " <> ctrl "and" <> " " <> renderTerm b)
  SOr a b -> parens (renderTerm a <> " " <> ctrl "or" <> " " <> renderTerm b)
  -- Forcing a delayed computation `f ()` prints as `f()` (an empty argument list).
  SApp h [STerm _ (STuple [])] -> renderTermP Application h <> parens mempty
  SApp h args -> renderTermP Application h <> parens (commas (map renderTerm args))
  SLam ps body -> fmt S.ControlKeyword "->" <> parens (commas [renderPlain p | SParam _ p <- ps]) <> " " <> renderTerm body
  SLet bs body -> blockOf (ctrl "begin") (map bindingStmtSep bs ++ [renderTerm body]) (ctrl "end")
  SLetRec bs body -> blockOf (ctrl "begin") (map bindingStmtSep bs ++ [renderTerm body]) (ctrl "end")
  SIf c t e -> ctrl "if" <> " " <> renderTerm c <> " " <> ctrl "then" <> " " <> renderTerm t <> " " <> ctrl "else" <> " " <> renderTerm e <> " " <> ctrl "end"
  SMatch s cs -> blockOf (ctrl "case" <> " " <> renderTerm s) (map renderCase cs) (ctrl "end")
  SHandle h e -> ctrl "handle" <> " " <> renderTerm e <> " " <> ctrl "with" <> " " <> renderTerm h <> " " <> ctrl "end"
  SDelay e -> ctrl "delay" <> parens (renderTerm e)
  SList xs -> fmt S.DelimiterChar "[" <> commas (map renderTerm xs) <> fmt S.DelimiterChar "]"
  STuple xs -> parens (commas (map renderTerm xs))
  SAnn e t -> parens (renderTerm e <> " " <> fmt S.TypeAscriptionColon ":" <> " " <> renderType t)
  SHole -> fmt S.Blank "_"
  STermLink n -> ctrl "termLink" <> parens (renderName n)
  STypeLink n -> ctrl "typeLink" <> parens (renderName n)
  SDocLit t -> fmt S.DocDelimiter (PP.text t)
  where
    wrapIf cond p = if cond then parens p else p

-- | A statement line in a block: a discarded statement (bound to @_@) prints bare; a function-valued binding becomes a
-- nested @def@ (with a preceding @name : Type@ when typed); a plain value becomes @name := value@ (Pascal assignment).
bindingStmt :: SBinding -> Pretty SyntaxText
bindingStmt b
  | Name.toText (bName b) == "_" = renderTerm (bValue b)
  | STerm _ (SAnn (STerm _ (SLam ps body)) ty) <- bValue b =
      PP.lines [renderPlain (bName b) <> " " <> fmt S.TypeAscriptionColon ":" <> " " <> renderType ty, defOf (bName b) ps body]
  | STerm _ (SLam ps body) <- bValue b = defOf (bName b) ps body
  | otherwise = renderPlain (bName b) <> " " <> fmt S.BindingEquals ":=" <> " " <> renderTerm (bValue b)

-- | A @def name(p, …) … end@ definition (used for both top-level and nested functions). The last body line is the
-- implicit result (Ruby-style).
defOf :: Name -> [SParam] -> STerm -> Pretty SyntaxText
defOf name ps body =
  blockOf
    (ctrl "def" <> " " <> renderPlain name <> parens (commas [renderPlain p | SParam _ p <- ps]))
    (funcBody body)
    (ctrl "end")

-- | The body lines of a @def@: a leading let\/letrec's bindings become statements directly, then the result expression
-- (no @return@ keyword — the last expression is the value).
funcBody :: STerm -> [Pretty SyntaxText]
funcBody body = case body of
  STerm _ (SLet bs e) -> map bindingStmtSep bs ++ [renderTerm e]
  STerm _ (SLetRec bs e) -> map bindingStmtSep bs ++ [renderTerm e]
  e -> [renderTerm e]

renderCase :: SCase -> Pretty SyntaxText
renderCase (SCase pat guard body) =
  let guardP = maybe mempty (\g -> " " <> ctrl "if" <> " " <> renderTerm g) guard
   in (ctrl "when" <> " " <> renderPattern pat <> guardP <> " " <> ctrl "then") <> PP.indentNAfterNewline 2 (PP.newline <> renderTerm body)

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
  SPTuple subs -> parens (commas (map renderPattern subs))
  SPEffect n subs k -> fmt S.DelimiterChar "{" <> renderName n <> parens (commas (map renderPattern subs)) <> " -> " <> renderPattern k <> fmt S.DelimiterChar "}"
  SPEffectPure sub -> fmt S.DelimiterChar "{" <> renderPattern sub <> fmt S.DelimiterChar "}"
  where
    seqOp = \case SCons -> "+:"; SSnoc -> ":+"; SConcat -> "++"

-- Types ---------------------------------------------------------------------------------------------------------------

renderType :: SType -> Pretty SyntaxText
renderType st@(SType _ t) = case t of
  STyVar n -> renderPlain n
  STyRef n -> renderName n
  STyForall vs body -> ctrl "forall" <> " " <> commas (map renderPlain vs) <> fmt S.DelimiterChar "." <> " " <> renderType body
  STyApp f args -> renderType f <> fmt S.DelimiterChar "<" <> commas (map renderType args) <> fmt S.DelimiterChar ">"
  STyEffects es -> renderEffects es
  STyTuple xs -> parens (commas (map renderType xs))
  STyEffectful es ty -> renderEffects es <> " " <> renderType ty
  STyArrow {} -> arrowSpine st
  where
    arrowSpine (SType _ (STyArrow i mes o)) =
      arrowInput i <> " " <> fmt S.TypeOperator "->" <> maybe mempty renderEffects mes <> " " <> arrowSpine o
    arrowSpine other = renderType other
    arrowInput ti@(SType _ (STyArrow {})) = parens (renderType ti)
    arrowInput ti = renderType ti

renderEffects :: [SType] -> Pretty SyntaxText
renderEffects es = fmt S.AbilityBraces "{" <> commas (map renderType es) <> fmt S.AbilityBraces "}"

-- Declarations --------------------------------------------------------------------------------------------------------

renderSDecl :: SDecl -> Pretty SyntaxText
renderSDecl sd
  | Just fields <- recordFields sd =
      blockOf (headerWith (ctrl "record")) [renderPlain f <> " " <> fmt S.TypeAscriptionColon ":" <> " " <> renderType t | (f, t) <- fields] (ctrl "end")
  | otherwise =
      blockOf (headerWith kw) (map (renderCtor (dIsAbility sd)) (dConstructors sd)) (ctrl "end")
  where
    kw = if dIsAbility sd then ctrl "ability" else ctrl "type"
    tyParams = case dTypeParams sd of
      [] -> mempty
      vs -> fmt S.DelimiterChar "<" <> commas (map renderPlain vs) <> fmt S.DelimiterChar ">"
    headerWith k = renderModifier (dModifier sd) <> k <> " " <> renderPlain (dName sd) <> tyParams

recordFields :: SDecl -> Maybe [(Name, SType)]
recordFields sd = do
  fields <- dFields sd
  ctor <- case dConstructors sd of [c] -> Just c; _ -> Nothing
  pure (zip fields (ctorArgTypes (cType ctor)))

renderModifier :: SModifier -> Pretty SyntaxText
renderModifier = \case
  SStructural -> fmt S.DataTypeModifier "structural" <> " "
  SUnique _ -> mempty

renderCtor :: Bool -> SConstructor -> Pretty SyntaxText
renderCtor isAbility (SConstructor _ name ty)
  | isAbility = renderPlain name <> " " <> fmt S.TypeAscriptionColon ":" <> " " <> renderType ty
  | otherwise = case ctorArgTypes ty of
      [] -> renderPlain name
      args -> renderPlain name <> parens (commas (map renderType args))

ctorArgTypes :: SType -> [SType]
ctorArgTypes (SType _ (STyForall _ b)) = ctorArgTypes b
ctorArgTypes (SType _ (STyArrow i _ o)) = i : ctorArgTypes o
ctorArgTypes _ = []

-- Dialect-facing entry points -----------------------------------------------------------------------------------------

docRender :: PrettyPrintEnv -> TermPrinter.DialectTermRenderer
docRender ppe = TermPrinter.DialectTermRenderer (\t -> renderTerm (Lower.lowerTermD (Just (docRender ppe)) ppe t))

lowerT :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> STerm
lowerT ppe = Lower.lowerTermD (Just (docRender ppe)) ppe

prettyTerm :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Pretty SyntaxText
prettyTerm ppe = renderTerm . lowerT ppe

prettyDoc2 :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Maybe (Pretty SyntaxText)
prettyDoc2 ppe = TermPrinter.prettyDoc2With (Just (docRender ppe)) ppe

prettyType :: (Var v) => PrettyPrintEnv -> Type v a -> Pretty SyntaxText
prettyType ppe = renderType . Lower.lowerType ppe

prettySignatures :: (Var v) => PrettyPrintEnv -> [(Referent, HQ.HashQualified Name, Type v a)] -> [Pretty SyntaxText]
prettySignatures ppe ts =
  [prettyHashQualified n <> " " <> fmt S.TypeAscriptionColon ":" <> " " <> renderType (Lower.lowerType ppe t) | (_, n, t) <- ts]

prettyDecl :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Pretty SyntaxText
prettyDecl pped _guid r hq decl = renderSDecl (Lower.lowerDecl pped r hq decl)

prettyDeclW :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Writer (Set AccessorName) (Pretty SyntaxText)
prettyDeclW pped guid r hq decl = pure (prettyDecl pped guid r hq decl)

-- | A Rubascal binding: a typed binding emits a @name : Type@ signature line then the definition; a function becomes
-- @def name(p, …) … end@, a plain value becomes @name := value@.
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

defForm :: HQ.HashQualified Name -> STerm -> Pretty SyntaxText
defForm hq s = case s of
  STerm _ (SLam ps body) ->
    blockOf
      (ctrl "def" <> " " <> prettyHashQualified hq <> parens (commas [renderPlain p | SParam _ p <- ps]))
      (funcBody body)
      (ctrl "end")
  _ -> prettyHashQualified hq <> " " <> fmt S.BindingEquals ":=" <> " " <> renderTerm s
