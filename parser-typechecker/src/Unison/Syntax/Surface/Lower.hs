-- | Lowering the content-addressed AST to the 'Surface' IR (the write\/print direction).
--
-- This is the syntax-neutral half of printing: it resolves names against a 'PrettyPrintEnv', recovers surface sugar
-- (tuples, @cases@, …) and is the single home of variable hygiene (so generated binders — e.g. the parameter @cases@
-- desugars to — get readable, capture-avoiding names instead of leaking a hash like @i7q2mr6bit1@). Every dialect's
-- renderer consumes the result, so they all inherit this work.
--
-- Annotations are set to 'External': the print direction does not need source positions (the eventual renderer ignores
-- them). The parse direction fills in real spans.
module Unison.Syntax.Surface.Lower
  ( lowerTerm,
    lowerTermD,
    lowerType,
    lowerDecl,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls (pattern TuplePattern, pattern TupleTerm', pattern TupleType')
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name (fromSegment, lastSegment)
import Unison.Parser.Ann (Ann (External))
import Unison.Pattern (Pattern)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TypeReference)
import Unison.Referent qualified as Referent
import Unison.Syntax.DeclPrinter qualified as DeclPrinter (getFieldAndAccessorNames)
import Unison.Syntax.Name qualified as Name (unsafeParseText)
import Unison.Syntax.NameSegment qualified as NameSegment (isSymboly, toEscapedText)
import Unison.Syntax.Precedence (InfixPrecedence (Lowest), Precedence (InfixOp), operatorPrecedence)
import Unison.Syntax.Surface
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.Pretty qualified as PP
import Unison.Var (Var)
import Unison.Var qualified as Var

ext :: STermF -> STerm
ext = STerm External

extTy :: STypeF -> SType
extTy = SType External

-- | The surface name of a local variable.
localName :: (Var v) => v -> SName
localName v = HQ.NameOnly (varName v)

varName :: (Var v) => v -> Name
varName = Name.unsafeParseText . Var.name

-- | A @cases@ expression desugars to @\\…args v -> match v with …@ where the parser invents @v@ (a name like
-- @uj2tktbkej1@). When that synthetic variable occurs only as the scrutinee, rename it to a short readable name so it
-- doesn't leak into dialect output. This is plain alpha-renaming, so it round-trips. (The default printer instead
-- recovers the @cases@ keyword; the dialects have no such keyword, so a readable binder is the next best thing.)
cleanCasesVars :: forall v at ap a. (Var v) => [v] -> Term2 v at ap v a -> ([v], Term2 v at ap v a)
cleanCasesVars vs body = case reverse vs of
  v : restRev
    | isSynthetic (Var.name v),
      Match' (Var' v') branches <- body,
      v == v',
      v `Set.notMember` branchFreeVars branches ->
        let avoid = Set.map Var.name (Set.fromList restRev <> branchFreeVars branches)
            fresh = head [Var.named n | n <- namePool, n `Set.notMember` avoid]
         in (reverse restRev ++ [fresh], ABT.rename v fresh body)
  _ -> (vs, body)
  where
    branchFreeVars = foldMap \(MatchCase _ g b) -> maybe mempty ABT.freeVars g <> ABT.freeVars b
    namePool = ["m", "x", "s", "y", "t", "u"] <> [("m" <> tShow i) | i <- [(0 :: Int) ..]]
    -- A name the parser invented for a `cases` scrutinee (via `uniqueName`): a longish base32hex string containing a
    -- digit. User-written parameter names (`s`, `acc`, …) are preserved.
    isSynthetic n = Text.length n >= 8 && Text.any Char.isDigit n && Text.all isBase32Hex n
    isBase32Hex c = c >= '0' && c <= '9' || c >= 'a' && c <= 'v'

-- | If a name denotes a symbolic operator, return the precedence infix dialects should use for it. Operators with no
-- entry in the precedence table get the loosest infix level (so they parenthesize conservatively but consistently with
-- the dialect parsers, which look the precedence up the same way).
opPrecedence :: SName -> Maybe Precedence
opPrecedence n = do
  name <- HQ.toName n
  let seg = Name.lastSegment name
  guard (NameSegment.isSymboly seg)
  pure (fromMaybe (InfixOp Lowest) (operatorPrecedence (NameSegment.toEscapedText seg)))

-- | Lower a term to the Surface IR, resolving names against the given 'PrettyPrintEnv'.
lowerTerm :: forall v at ap a. (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> STerm
lowerTerm = lowerTermD Nothing

-- | Like 'lowerTerm', but with an optional dialect renderer for code embedded in docs (so doc code blocks captured in
-- 'SDocLit' are written in the active dialect rather than the default Unison syntax).
lowerTermD :: forall v at ap a. (Var v) => Maybe TermPrinter.DialectTermRenderer -> PrettyPrintEnv -> Term2 v at ap v a -> STerm
lowerTermD docRender ppe = go
  where
    go :: Term2 v at ap v a -> STerm
    go term
      -- Docs render to their `{{ … }}` source text (re-parsed by Elaborate). The doc markup itself is
      -- dialect-independent; only embedded code blocks differ, rendered via `docRender` when supplied.
      | Just p <- TermPrinter.prettyDoc2With docRender ppe term = ext (SDocLit (PP.toPlain 80 (PP.syntaxToColor p)))
    go term = ext case term of
      Var' v -> SName (localName v)
      Int' i -> SLit (SInt i)
      Nat' n -> SLit (SNat n)
      Float' f -> SLit (SFloat f)
      Boolean' b -> SLit (SBool b)
      Text' t -> SLit (SText t)
      Char' c -> SLit (SChar c)
      Ref' r -> SName (PPE.termNameOrHashOnly ppe (Referent.Ref r))
      Constructor' (ConstructorReference r i) -> SName (PPE.termNameOrHashOnly ppe (Referent.Con (ConstructorReference r i) CT.Data))
      Request' (ConstructorReference r i) -> SName (PPE.termNameOrHashOnly ppe (Referent.Con (ConstructorReference r i) CT.Effect))
      TermLink' rt -> STermLink (PPE.termNameOrHashOnly ppe rt)
      TypeLink' rf -> STypeLink (PPE.typeNameOrHashOnly ppe rf)
      List' xs -> SList (map go (toList xs))
      TupleTerm' xs -> STuple (map go xs)
      If' c t f -> SIf (go c) (go t) (go f)
      And' a b -> SAnd (go a) (go b)
      Or' a b -> SOr (go a) (go b)
      Handle' h body -> SHandle (go h) (go body)
      Delay' body -> SDelay (go body)
      Ann' e t -> SAnn (go e) (lowerType ppe t)
      LamsNamed' vs body -> let (vs', body') = cleanCasesVars vs body in SLam [SParam External (varName v) | v <- vs'] (go body')
      LetRecNamed' bs body -> SLetRec [binding v b | (v, b) <- bs] (go body)
      Lets' bs body -> SLet (zipWith letBinding [0 ..] bs) (go body)
        where
          -- A discarded statement (a desugar-generated @_<n>@ var that is never referenced downstream — e.g. from a
          -- @do@\/sequencing block) renders as @_ = e@ rather than leaking the raw generated name. Elaborate gives each
          -- @_@ binding a fresh distinct var, so they don't collide.
          tailFree i = foldMap (\(_, _, b') -> ABT.freeVars b') (drop (i + 1) bs) <> ABT.freeVars body
          letBinding i (_, v, b)
            | isDiscardName (Var.name v), v `Set.notMember` tailFree i = SBinding External discardName Nothing (go b)
            | otherwise = binding v b
          discardName = Name.unsafeParseText "_"
          isDiscardName n = Text.length n > 1 && Text.head n == '_' && Text.all Char.isDigit (Text.drop 1 n)
      Match' scrutinee cases -> SMatch (go scrutinee) (map (lowerCase ppe) cases)
      Apps' f args -> case (go f, map go args) of
        -- A two-argument application of a symbolic operator is recovered to 'SBinOp', carrying the operator's
        -- precedence so infix dialects can render minimal parens and parse them back.
        (STerm _ (SName n), [x, y]) | Just prec <- opPrecedence n -> SBinOp n prec x y
        (sf, sargs) -> SApp sf sargs
      Blank' _ -> SHole
      -- Fallback: anything not recognized becomes a hole. (Will shrink to nothing as lowering grows; for now keeps
      -- lowering total. Such terms are rare in practice.)
      _ -> SHole

    binding :: v -> Term2 v at ap v a -> SBinding
    binding v b = SBinding External (varName v) Nothing (go b)

    lowerCase :: PrettyPrintEnv -> MatchCase ap (Term2 v at ap v a) -> SCase
    lowerCase ppe' (MatchCase pat guard body) =
      let (vs, body') = unAbsN body
          (spat, _) = lowerPattern ppe' vs pat
          sguard = (\g -> let (_, g') = unAbsN g in go g') <$> guard
       in SCase spat sguard (go body')

unAbsN :: Term2 v at ap v a -> ([v], Term2 v at ap v a)
unAbsN (ABT.AbsN' vs body) = (vs, body)

-- | Lower a pattern, consuming bound-variable names from the supplied list (left-to-right, the order the case body
-- abstracts them).
lowerPattern :: (Var v) => PrettyPrintEnv -> [v] -> Pattern loc -> (SPattern, [v])
lowerPattern ppe = go
  where
    conName cref ct = PPE.termNameOrHashOnly ppe (Referent.Con cref ct)
    go vs p = case p of
      Pattern.Unbound _ -> (sp SPWild, vs)
      Pattern.Var _ -> case vs of
        (v : tl) -> (sp (SPVar (varName v)), tl)
        [] -> (sp SPWild, [])
      Pattern.Boolean _ b -> (sp (SPLit (SBool b)), vs)
      Pattern.Int _ i -> (sp (SPLit (SInt i)), vs)
      Pattern.Nat _ n -> (sp (SPLit (SNat n)), vs)
      Pattern.Float _ f -> (sp (SPLit (SFloat f)), vs)
      Pattern.Text _ t -> (sp (SPLit (SText t)), vs)
      Pattern.Char _ c -> (sp (SPLit (SChar c)), vs)
      -- Tuple patterns `(p, q, …)` are sugar for the `Tuple` constructor pattern. (A 1-element tuple pattern is just
      -- the element, so it isn't sugared.)
      TuplePattern ps | length ps /= 1 ->
        let (sps, vs') = goList vs ps in (sp (SPTuple sps), vs')
      Pattern.Constructor _ cref ps ->
        let (sps, vs') = goList vs ps
         in (sp (SPCtor (conName cref CT.Data) sps), vs')
      Pattern.As _ p' -> case vs of
        (v : tl) -> let (sp', vs') = go tl p' in (sp (SPAs (varName v) sp'), vs')
        [] -> go vs p'
      Pattern.SequenceLiteral _ ps ->
        let (sps, vs') = goList vs ps in (sp (SPList sps), vs')
      Pattern.SequenceOp _ l op r ->
        let (sl, vs') = go vs l
            (sr, vs'') = go vs' r
         in (sp (SPSeqOp sl (seqOp op) sr), vs'')
      Pattern.EffectPure _ p' -> let (sp', vs') = go vs p' in (sp (SPEffectPure sp'), vs')
      Pattern.EffectBind _ cref ps k ->
        let (sps, vs') = goList vs ps
            (sk, vs'') = go vs' k
         in (sp (SPEffect (conName cref CT.Effect) sps sk), vs'')
      Pattern.Bytes _ _ -> (sp SPWild, vs) -- bytes patterns unsupported in the IR for now
    sp = SPattern External
    goList vs [] = ([], vs)
    goList vs (p : ps) =
      let (s, vs') = go vs p
          (ss, vs'') = goList vs' ps
       in (s : ss, vs'')

seqOp :: Pattern.SeqOp -> SSeqOp
seqOp = \case
  Pattern.Cons -> SCons
  Pattern.Snoc -> SSnoc
  Pattern.Concat -> SConcat

-- | Lower a data\/ability declaration to the Surface IR. Constructor argument types are kept on each
-- 'SConstructor' as the full constructor type (the renderer strips the result).
lowerDecl :: forall v a. (Var v) => PrettyPrintEnvDecl -> TypeReference -> HQ.HashQualified Name -> Decl v a -> SDecl
lowerDecl pped ref hq decl =
  SDecl External modi isAbility name (map varName (DD.bound dd)) ctors fields
  where
    dd = either DD.toDataDecl id decl
    isAbility = either (const True) (const False) decl
    -- Records are recovered the same way the default decl printer does it (by hashing candidate accessors); when found,
    -- 'dFields' carries the field names so the renderer prints record syntax and the elaborator regenerates accessors.
    fields
      | isAbility = Nothing
      | otherwise = fst <$> DeclPrinter.getFieldAndAccessorNames (PPED.unsuffixifiedPPE pped) ref hq dd
    ct = if isAbility then CT.Effect else CT.Data
    modi = case DD.modifier dd of
      DD.Structural -> SStructural
      DD.Unique t -> SUnique t
    name = fromMaybe (Name.unsafeParseText "?") (HQ.toName hq)
    -- Constructor names come from the PPE (the stored decl's constructor vars are anonymized placeholders).
    ctorName i =
      case HQ.toName (PPE.termName (PPED.suffixifiedPPE pped) (Referent.Con (ConstructorReference ref (fromIntegral i)) ct)) of
        Just n -> Name.fromSegment (Name.lastSegment n)
        Nothing -> Name.unsafeParseText ("Constructor" <> tShow i)
    ctors =
      [ SConstructor External (ctorName i) (lowerType (PPED.suffixifiedPPE pped) ty)
        | (i, (_, _v, ty)) <- zip [(0 :: Int) ..] (DD.constructors' dd)
      ]

-- | Lower a type to the Surface IR. Applies the same normalization the default type printer does
-- ('Type.removeEmptyEffects' . 'Type.cleanup') so that pure arrows render without spurious empty ability rows.
lowerType :: forall v a. (Var v) => PrettyPrintEnv -> Type v a -> SType
lowerType ppe = go . Type.removeEmptyEffects . Type.cleanup
  where
    go :: Type v a -> SType
    go typ = extTy case typ of
      Type.Var' v -> STyVar (varName v)
      Type.Ref' r -> STyRef (PPE.typeNameOrHashOnly ppe r)
      Type.ForallsNamed' vs body | not (null vs) -> STyForall (map varName vs) (go body)
      Type.Arrow'' i es o -> STyArrow (go i) (if null es then Nothing else Just (map go es)) (go o)
      -- Tuple types `(a, b, …)` and the unit type `()` are sugar for the `Tuple`/`Unit` encoding. (A 1-element
      -- `TupleType'` is just the element type, so it isn't sugared.)
      TupleType' xs | length xs /= 1 -> STyTuple (map go xs)
      Type.Apps' f args -> STyApp (go f) (map go args)
      Type.Effects' es -> STyEffects (map go es)
      _ -> STyVar (Name.unsafeParseText "_")
