{-# LANGUAGE OverloadedStrings #-}

-- | The Basison /renderer/: 'Surface' -> @Pretty SyntaxText@.
--
-- A BASIC-flavored dialect (think QBASIC\/FreeBASIC): functions are @FUNCTION name(p, …) … END FUNCTION@ with an
-- explicit @RETURN@, pattern matching is @SELECT CASE s … CASE p … END SELECT@, conditionals are
-- @IF c THEN t ELSE e END IF@, type ascription uses @AS@, and keywords are UPPERCASE (so lowercase names like @as@ and
-- @then@ stay ordinary identifiers). The full type of a definition rides on a preceding @name AS Type@ signature line,
-- so the @FUNCTION@ header carries only bare parameter names. Like the other dialects it is a pure tree-walk over
-- 'Surface' (resolution\/hygiene happened in 'Unison.Syntax.Surface.Lower'); symbolic operators render infix with
-- minimal parentheses driven by 'SBinOp'\'s precedence.
module Unison.Syntax.Dialect.Basison
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
  SBool b -> fmt S.BooleanLiteral (if b then "TRUE" else "FALSE")
  SText t -> fmt S.TextLiteral (PP.string (show t))
  SChar c -> fmt S.CharLiteral (PP.string ('\'' : c : "'"))

-- | A header line, then its body lines indented, then a closing keyword on its own line.
blockOf :: Pretty SyntaxText -> [Pretty SyntaxText] -> Pretty SyntaxText -> Pretty SyntaxText
blockOf header items ender =
  header <> PP.indentNAfterNewline 2 (PP.newline <> PP.lines items) <> PP.newline <> ender

-- | The statement separator (BASIC's @:@). Each statement in a block is terminated by it, except a nested @FUNCTION@
-- (which already ends in @END FUNCTION@), so the parser can tell where one statement ends and the next begins.
sep :: Pretty SyntaxText
sep = fmt S.DelimiterChar ":"

-- | A block statement followed by its separator (unless it is a nested function, which is self-delimiting).
bindingStmtSep :: SBinding -> Pretty SyntaxText
bindingStmtSep b = bindingStmt b <> if isFuncBinding b then mempty else sep

-- | Whether a binding renders as a nested @FUNCTION@ (a lambda value, possibly type-ascribed, not discarded).
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
  SAnd a b -> parens (renderTerm a <> " " <> ctrl "AND" <> " " <> renderTerm b)
  SOr a b -> parens (renderTerm a <> " " <> ctrl "OR" <> " " <> renderTerm b)
  -- Forcing a delayed computation `f ()` prints as `f()` (an empty argument list).
  SApp h [STerm _ (STuple [])] -> renderTermP Application h <> parens mempty
  SApp h args -> renderTermP Application h <> parens (commas (map renderTerm args))
  SLam ps body -> ctrl "LAMBDA" <> parens (commas [renderPlain p | SParam _ p <- ps]) <> " " <> renderTerm body
  SLet bs body -> blockOf (ctrl "BLOCK") (map bindingStmtSep bs ++ [renderTerm body]) (ctrl "END BLOCK")
  SLetRec bs body -> blockOf (ctrl "BLOCK") (map bindingStmtSep bs ++ [renderTerm body]) (ctrl "END BLOCK")
  SIf c t e -> ctrl "IF" <> " " <> renderTerm c <> " " <> ctrl "THEN" <> " " <> renderTerm t <> " " <> ctrl "ELSE" <> " " <> renderTerm e <> " " <> ctrl "END IF"
  SMatch s cs -> blockOf (ctrl "SELECT CASE" <> " " <> renderTerm s) (map renderCase cs) (ctrl "END SELECT")
  SHandle h e -> ctrl "HANDLE" <> " " <> renderTerm e <> " " <> ctrl "WITH" <> " " <> renderTerm h <> " " <> ctrl "END HANDLE"
  SDelay e -> ctrl "DELAY" <> parens (renderTerm e)
  SList xs -> fmt S.DelimiterChar "[" <> commas (map renderTerm xs) <> fmt S.DelimiterChar "]"
  STuple xs -> parens (commas (map renderTerm xs))
  SAnn e t -> parens (renderTerm e <> " " <> ctrl "AS" <> " " <> renderType t)
  SHole -> fmt S.Blank "_"
  STermLink n -> ctrl "TERMLINK" <> parens (renderName n)
  STypeLink n -> ctrl "TYPELINK" <> parens (renderName n)
  SDocLit t -> fmt S.DocDelimiter (PP.text t)
  where
    wrapIf cond p = if cond then parens p else p

-- | A statement line in a block: a discarded statement (bound to @_@) prints bare; a function-valued binding becomes a
-- nested @FUNCTION@ (with a preceding @name AS Type@ when typed, so it reads like a top-level definition); a plain value
-- becomes @LET name = value@.
bindingStmt :: SBinding -> Pretty SyntaxText
bindingStmt b
  | Name.toText (bName b) == "_" = renderTerm (bValue b)
  | STerm _ (SAnn (STerm _ (SLam ps body)) ty) <- bValue b =
      PP.lines [renderPlain (bName b) <> " " <> ctrl "AS" <> " " <> renderType ty, funcOf (bName b) ps body]
  | STerm _ (SLam ps body) <- bValue b = funcOf (bName b) ps body
  | otherwise = ctrl "LET" <> " " <> renderPlain (bName b) <> " " <> fmt S.BindingEquals "=" <> " " <> renderTerm (bValue b)

-- | A @FUNCTION name(p, …) … RETURN result END FUNCTION@ definition (used for both top-level and nested functions).
funcOf :: Name -> [SParam] -> STerm -> Pretty SyntaxText
funcOf name ps body =
  blockOf
    (ctrl "FUNCTION" <> " " <> renderPlain name <> parens (commas [renderPlain p | SParam _ p <- ps]))
    (funcBody body)
    (ctrl "END FUNCTION")

-- | The body lines of a @FUNCTION@: a leading let\/letrec's bindings become statements directly, then @RETURN result@.
funcBody :: STerm -> [Pretty SyntaxText]
funcBody body = case body of
  STerm _ (SLet bs e) -> map bindingStmtSep bs ++ [ret e]
  STerm _ (SLetRec bs e) -> map bindingStmtSep bs ++ [ret e]
  e -> [ret e]
  where
    ret e = ctrl "RETURN" <> " " <> renderTerm e

renderCase :: SCase -> Pretty SyntaxText
renderCase (SCase pat guard body) =
  let guardP = maybe mempty (\g -> " " <> ctrl "IF" <> " " <> renderTerm g) guard
   in (ctrl "CASE" <> " " <> renderPattern pat <> guardP <> fmt S.DelimiterChar ":") <> PP.indentNAfterNewline 2 (PP.newline <> renderTerm body)

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
  STyForall vs body -> ctrl "FORALL" <> " " <> commas (map renderPlain vs) <> fmt S.DelimiterChar "." <> " " <> renderType body
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
      blockOf (headerWith (ctrl "RECORD")) [renderPlain f <> " " <> ctrl "AS" <> " " <> renderType t | (f, t) <- fields] (ctrl "END RECORD")
  | otherwise =
      blockOf (headerWith kw) (map (renderCtor (dIsAbility sd)) (dConstructors sd)) ender
  where
    kw = if dIsAbility sd then ctrl "ABILITY" else ctrl "TYPE"
    ender = if dIsAbility sd then ctrl "END ABILITY" else ctrl "END TYPE"
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
  SStructural -> fmt S.DataTypeModifier "STRUCTURAL" <> " "
  SUnique _ -> mempty

renderCtor :: Bool -> SConstructor -> Pretty SyntaxText
renderCtor isAbility (SConstructor _ name ty)
  | isAbility = renderPlain name <> " " <> ctrl "AS" <> " " <> renderType ty
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
  [prettyHashQualified n <> " " <> ctrl "AS" <> " " <> renderType (Lower.lowerType ppe t) | (_, n, t) <- ts]

prettyDecl :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Pretty SyntaxText
prettyDecl pped _guid r hq decl = renderSDecl (Lower.lowerDecl pped r hq decl)

prettyDeclW :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Writer (Set AccessorName) (Pretty SyntaxText)
prettyDeclW pped guid r hq decl = pure (prettyDecl pped guid r hq decl)

-- | A Basison binding: a typed binding emits an @name AS Type@ signature line then the definition; a function becomes
-- @FUNCTION name(p, …) … END FUNCTION@, a plain value becomes @LET name = value@.
prettyBinding :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBinding ppe hq term =
  case lowerT ppe term of
    STerm _ (SAnn e ty) -> sig ty <> PP.newline <> def e
    s -> def s
  where
    sig ty = prettyHashQualified hq <> " " <> ctrl "AS" <> " " <> renderType ty
    def = defForm hq

prettyBindingWithoutTypeSignature :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBindingWithoutTypeSignature ppe hq term = defForm hq (peelAnn (lowerT ppe term))
  where
    peelAnn = \case STerm _ (SAnn e _) -> e; s -> s

defForm :: HQ.HashQualified Name -> STerm -> Pretty SyntaxText
defForm hq s = case s of
  STerm _ (SLam ps body) ->
    blockOf
      (ctrl "FUNCTION" <> " " <> prettyHashQualified hq <> parens (commas [renderPlain p | SParam _ p <- ps]))
      (funcBody body)
      (ctrl "END FUNCTION")
  _ -> ctrl "LET" <> " " <> prettyHashQualified hq <> " " <> fmt S.BindingEquals "=" <> " " <> renderTerm s
