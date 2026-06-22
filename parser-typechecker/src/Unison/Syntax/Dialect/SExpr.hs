{-# LANGUAGE OverloadedStrings #-}

-- | The S-expression (Clojure-like) /renderer/: 'Surface' -> @Pretty SyntaxText@.
--
-- Since the syntax-neutral work (name resolution, hygiene, sugar recovery) happens in
-- 'Unison.Syntax.Surface.Lower', this renderer is a small, pure tree-walk over the 'Surface' IR — it needs no
-- 'PrettyPrintEnv'. The dialect-facing entry points ('prettyTerm', 'prettyBinding', …) simply compose @render . lower@.
module Unison.Syntax.Dialect.SExpr
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

parenize :: Pretty SyntaxText -> Pretty SyntaxText
parenize p = fmt S.Parenthesis "(" <> p <> fmt S.Parenthesis ")"

callP :: [Pretty SyntaxText] -> Pretty SyntaxText
callP = parenize . PP.sep " "

hangForm :: Pretty SyntaxText -> Pretty SyntaxText -> Pretty SyntaxText
hangForm hd body = parenize (PP.hang hd body)

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
  SChar c -> fmt S.CharLiteral (PP.string ('\\' : [c]))

-- Terms ---------------------------------------------------------------------------------------------------------------

renderSTerm :: STerm -> Pretty SyntaxText
renderSTerm (STerm _ f) = case f of
  SLit l -> renderLit l
  SName n -> renderName n
  SApp h args -> callP (renderSTerm h : map renderSTerm args)
  SBinOp n _ a b -> callP [renderName n, renderSTerm a, renderSTerm b]
  SLam ps body -> hangForm (ctrl "fn" <> " " <> paramList ps) (renderSTerm body)
  SLet bs body -> hangForm (ctrl "let" <> " " <> bindingList bs) (renderSTerm body)
  SLetRec bs body -> hangForm (ctrl "letrec" <> " " <> bindingList bs) (renderSTerm body)
  SIf c t e -> callP [ctrl "if", renderSTerm c, renderSTerm t, renderSTerm e]
  SAnd a b -> callP [ctrl "and", renderSTerm a, renderSTerm b]
  SOr a b -> callP [ctrl "or", renderSTerm a, renderSTerm b]
  SMatch s cs -> hangForm (ctrl "match" <> " " <> renderSTerm s) (PP.lines (map renderCase cs))
  SHandle h e -> hangForm (ctrl "handle" <> " " <> renderSTerm h) (renderSTerm e)
  SDelay e -> callP [ctrl "delay", renderSTerm e]
  SList xs -> fmt S.DelimiterChar "[" <> PP.sep " " (map renderSTerm xs) <> fmt S.DelimiterChar "]"
  STuple xs -> callP (ctrl "tuple" : map renderSTerm xs)
  SAnn e t -> callP [ctrl "ann", renderSTerm e, renderSType t]
  SHole -> fmt S.Blank "_"
  STermLink n -> callP [ctrl "termLink", renderName n]
  STypeLink n -> callP [ctrl "typeLink", renderName n]
  SDocLit t -> fmt S.DocDelimiter (PP.text t)

paramList :: [SParam] -> Pretty SyntaxText
paramList ps = parenize (PP.sep " " [renderPlain n | SParam _ n <- ps])

bindingList :: [SBinding] -> Pretty SyntaxText
bindingList bs = parenize (PP.lines [parenize (renderPlain (bName b) <> " " <> renderSTerm (bValue b)) | b <- bs])

renderCase :: SCase -> Pretty SyntaxText
renderCase (SCase pat guard body) =
  let guardForms = maybe [] (\g -> [callP [ctrl "when", renderSTerm g]]) guard
   in hangForm (ctrl "case" <> " " <> PP.sep " " (renderSPattern pat : guardForms)) (renderSTerm body)

renderSPattern :: SPattern -> Pretty SyntaxText
renderSPattern (SPattern _ p) = case p of
  SPWild -> fmt S.DelimiterChar "_"
  SPVar n -> renderPlain n
  SPLit l -> renderLit l
  SPCtor n [] -> renderName n
  SPCtor n subs -> parenize (PP.sep " " (renderName n : map renderSPattern subs))
  SPAs n sub -> parenize (ctrl "as" <> " " <> renderPlain n <> " " <> renderSPattern sub)
  SPList subs -> fmt S.DelimiterChar "[" <> PP.sep " " (map renderSPattern subs) <> fmt S.DelimiterChar "]"
  SPSeqOp l op r -> callP [ctrl (seqOpName op), renderSPattern l, renderSPattern r]
  SPTuple subs -> callP (ctrl "tuple" : map renderSPattern subs)
  SPEffect n subs k -> parenize (ctrl "request" <> " " <> PP.sep " " (renderName n : map renderSPattern subs) <> " " <> renderSPattern k)
  SPEffectPure sub -> callP [ctrl "pure", renderSPattern sub]
  where
    seqOpName = \case SCons -> "cons"; SSnoc -> "snoc"; SConcat -> "concat"

-- Types ---------------------------------------------------------------------------------------------------------------

renderSType :: SType -> Pretty SyntaxText
renderSType st@(SType _ t) = case t of
  STyVar n -> renderPlain n
  STyRef n -> renderName n
  STyForall vs body -> hangForm (fmt S.TypeOperator "forall" <> " " <> parenize (PP.sep " " (map renderPlain vs))) (renderSType body)
  STyApp f args -> callP (renderSType f : map renderSType args)
  STyEffects es -> renderEffects es
  STyTuple xs -> callP (ctrl "tuple" : map renderSType xs)
  STyEffectful es t -> parenize (renderEffects es <> " " <> renderSType t)
  STyArrow {} -> callP (fmt S.TypeOperator "->" : arrowComponents st)
  where
    -- Flatten the arrow spine into rendered components, inserting an ability row (e.g. @{e}@) just before the
    -- component it precedes.
    arrowComponents (SType _ (STyArrow i Nothing o)) = renderSType i : arrowComponents o
    arrowComponents (SType _ (STyArrow i (Just es) o)) = renderSType i : renderEffects es : arrowComponents o
    arrowComponents other = [renderSType other]

renderEffects :: [SType] -> Pretty SyntaxText
renderEffects es = fmt S.AbilityBraces "{" <> PP.sep " " (map renderSType es) <> fmt S.AbilityBraces "}"

-- Declarations --------------------------------------------------------------------------------------------------------

renderSDecl :: SDecl -> Pretty SyntaxText
renderSDecl sd
  | Just fields <- recordFields sd =
      hangForm (headerWith (ctrl "record")) (PP.lines [parenize (renderPlain f <> " " <> renderSType t) | (f, t) <- fields])
  | otherwise =
      hangForm (headerWith kw) (PP.lines (map (renderCtor (dIsAbility sd)) (dConstructors sd)))
  where
    kw = if dIsAbility sd then ctrl "ability" else ctrl "type"
    headerWith k =
      k
        <> " "
        <> renderModifier (dModifier sd)
        <> renderPlain (dName sd)
        <> " "
        <> parenize (PP.sep " " (map renderPlain (dTypeParams sd)))

-- | When a declaration is a record, pair each field name with its (positional) field type.
recordFields :: SDecl -> Maybe [(Name, SType)]
recordFields sd = do
  fields <- dFields sd
  ctor <- case dConstructors sd of [c] -> Just c; _ -> Nothing
  pure (zip fields (ctorArgTypes (cType ctor)))

-- | Renders the modifier (with a trailing space) for the declaration header. @unique@ is the default, so unique types
-- render no modifier at all (and no GUID) — matching the regular syntax; the GUID is recovered by name on re-parse.
renderModifier :: SModifier -> Pretty SyntaxText
renderModifier = \case
  SStructural -> fmt S.DataTypeModifier "structural" <> " "
  SUnique _ -> mempty

renderCtor :: Bool -> SConstructor -> Pretty SyntaxText
renderCtor isAbility (SConstructor _ name ty)
  | isAbility = parenize (renderPlain name <> " " <> renderSType ty)
  | otherwise = parenize (PP.sep " " (renderPlain name : map renderSType (ctorArgTypes ty)))

-- | The argument types of a constructor (its full type minus the result).
ctorArgTypes :: SType -> [SType]
ctorArgTypes (SType _ (STyForall _ b)) = ctorArgTypes b
ctorArgTypes (SType _ (STyArrow i _ o)) = i : ctorArgTypes o
ctorArgTypes _ = []

-- Dialect-facing entry points (render . lower) -----------------------------------------------------------------------

-- | Render terms embedded in docs (code blocks) in this dialect, recursively (so nested docs stay in-dialect too).
docRender :: PrettyPrintEnv -> TermPrinter.DialectTermRenderer
docRender ppe = TermPrinter.DialectTermRenderer (\t -> renderSTerm (Lower.lowerTermD (Just (docRender ppe)) ppe t))

-- | Lower a term with this dialect's doc-code renderer in effect.
lowerT :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> STerm
lowerT ppe = Lower.lowerTermD (Just (docRender ppe)) ppe

prettyTerm :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Pretty SyntaxText
prettyTerm ppe = renderSTerm . lowerT ppe

-- | Render a Doc2 term as a doc literal, with embedded code in this dialect.
prettyDoc2 :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Maybe (Pretty SyntaxText)
prettyDoc2 ppe = TermPrinter.prettyDoc2With (Just (docRender ppe)) ppe

prettyType :: (Var v) => PrettyPrintEnv -> Type v a -> Pretty SyntaxText
prettyType ppe = renderSType . Lower.lowerType ppe

-- | Render @name : type@ signatures (for @find@\/slurp), in S-expr form @(: name type)@.
prettySignatures :: (Var v) => PrettyPrintEnv -> [(Referent, HQ.HashQualified Name, Type v a)] -> [Pretty SyntaxText]
prettySignatures ppe ts = [callP [ctrl ":", prettyHashQualified n, renderSType (Lower.lowerType ppe t)] | (_, n, t) <- ts]

prettyDecl :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Pretty SyntaxText
prettyDecl pped _guid r hq decl = renderSDecl (Lower.lowerDecl pped r hq decl)

prettyDeclW :: (Var v) => PrettyPrintEnvDecl -> RenderUniqueTypeGuids -> TypeReference -> HQ.HashQualified Name -> Decl v a -> Writer (Set AccessorName) (Pretty SyntaxText)
prettyDeclW pped guid r hq decl = pure (prettyDecl pped guid r hq decl)

prettyBinding :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBinding ppe hq term =
  case lowerT ppe term of
    STerm _ (SAnn e ty) ->
      callP [ctrl ":", prettyHashQualified hq, renderSType ty] <> PP.newline <> defForm e
    s -> defForm s
  where
    defForm s = case s of
      STerm _ (SLam ps body) ->
        hangForm (ctrl "defn" <> " " <> prettyHashQualified hq <> " " <> paramList ps) (renderSTerm body)
      _ -> hangForm (ctrl "def" <> " " <> prettyHashQualified hq) (renderSTerm s)

prettyBindingWithoutTypeSignature :: (Var v) => PrettyPrintEnv -> HQ.HashQualified Name -> Term2 v at ap v a -> Pretty SyntaxText
prettyBindingWithoutTypeSignature ppe hq term =
  case peelAnn (lowerT ppe term) of
    STerm _ (SLam ps body) ->
      hangForm (ctrl "defn" <> " " <> prettyHashQualified hq <> " " <> paramList ps) (renderSTerm body)
    s -> hangForm (ctrl "def" <> " " <> prettyHashQualified hq) (renderSTerm s)
  where
    peelAnn = \case
      STerm _ (SAnn e _) -> e
      s -> s
