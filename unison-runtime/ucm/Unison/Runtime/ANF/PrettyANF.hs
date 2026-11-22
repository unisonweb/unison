{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Shows intermediate code (ANF) in a syntax that resembles Unison's, for
-- the `ir` and `ir.optimized` commands in ucm. The syntax is explained, with
-- a reference table, in unison-src/transcripts/idempotent/ir.md.
module Unison.Runtime.ANF.PrettyANF
  ( IRNames (..),
    prettyIRGroup,
    prettyIRCombs,
  )
where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.Bifoldable (bifoldMap, bifoldr)
import Data.Bits (bit, shiftL, shiftR, testBit, (.&.))
import Data.Char (isDigit)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Float (castWord64ToDouble)
import Numeric (showHex)
import Unison.ABT.Normalized qualified as ABTN
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv, termName, typeName)
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Rfn
import Unison.Runtime.ANF
import Unison.Runtime.Builtin (builtinForeignNames, builtinPrimNames)
import Unison.Runtime.Builtin.Decls (pairRef, unitRef)
import Unison.Runtime.Wide (wideOn)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NamePrinter (SyntaxText, prettyHashQualified)
import Unison.Type qualified as Ty
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.EnumContainers qualified as EC
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as P
import Unison.Util.SyntaxText qualified as S
import Unison.Util.Text qualified as Util.Text
import Unison.Var qualified as Var

-- | How to name what the code refers to.
data IRNames = IRNames
  { -- | A top-level function (combinator): the definition it came from, or
    -- the name of a lifted lambda.
    combName :: Reference -> Either (Pretty SyntaxText) (HQ.HashQualified Name),
    -- | Constructors, abilities and links.
    ppe :: PrettyPrintEnv,
    -- | The name of a local function by group and index, for the labels of
    -- the combinators the rewrites add ('indexLabel'); set for the group
    -- being shown.
    localName :: Reference -> Word64 -> Maybe (Pretty SyntaxText)
  }

-- | A name, and whether it is an operator (shown infix with two
-- arguments, and in parentheses otherwise).
data Head = Head Bool (Pretty SyntaxText)

-- | A line or lines of a block. The flag says whether it spans more than
-- one line, which decides whether a binding of it goes on one line.
data Stmt = Stmt Bool (Pretty SyntaxText)

-- | How to show the variables of a definition.
data Names = Names
  { varNames :: Map Symbol (Pretty SyntaxText),
    -- | the variables of a lambda bound to the inner pairs of a tuple
    -- shown as one ('tupleSugar'), with the elements of each
    tupleTails :: Map Symbol [Symbol],
    -- | the variables wide where this code is: bound by a wide let and not
    -- boxed yet on this path
    wideVars :: Set Symbol
  }

-- | A group (one top-level function and its local functions), as the
-- definition of the given reference.
prettyIRGroup :: IRNames -> Reference -> SuperGroup Reference Symbol -> Pretty SyntaxText
prettyIRGroup irn ref (Rec grp ent) =
  prettyIRCombs irn ref (map fst grp) ((0, ent) : zip localIndices (map snd grp))

-- | The combinators of a group by index, as the runtime numbers them: the
-- entry at 0, the local functions at @n * 2^16@ (named by the given
-- variables, which is how the bodies refer to them), and those the
-- loader's rewrites add (Unison.Runtime.MCode.Explode): specialisations,
-- a product-return worker and join points. The entry is the definition
-- of the reference with the local functions in its block; each added
-- combinator is a definition of its own, named for its index
-- ('indexLabel').
prettyIRCombs :: IRNames -> Reference -> [Symbol] -> [(Word64, SuperNormal Reference Symbol)] -> Pretty SyntaxText
prettyIRCombs irn0 ref locals cmbs =
  P.linesSpaced (main ++ map added others)
  where
    (entry, rest) = partition ((== 0) . fst) cmbs
    (localCmbs, others) = partition ((`elem` localIndices') . fst) rest
    localIndices' = take (length locals) localIndices
    localNames = zip localIndices' locals
    vnames = nameVars (foldMap (affineCells . snd) cmbs) (foldMap (continuations . snd) cmbs) locals (concatMap (superBinders . snd) cmbs)
    irn = irn0 {localName = \r i -> if r == ref then lookupLocal vnames localNames i else localName irn0 r i}
    base = refHead irn (combName irn ref)
    local (i, sn) = case lookup i localNames of
      Just v -> definition irn vnames (var vnames v) sn []
      Nothing -> definition irn vnames (P.shown i) sn []
    main = [p | (_, sn) <- entry, Stmt _ p <- [definition irn vnames (defName base) sn (map local localCmbs)]]
    added (i, sn) = case definition irn vnames (headText base <> indexLabel (localName irn ref) i) sn [] of
      Stmt _ p -> p
    defName (Head True op) = P.parenthesize op
    defName (Head False f) = f

headText :: Head -> Pretty SyntaxText
headText (Head _ p) = p

localIndices :: [Word64]
localIndices = map (`shiftL` 16) [1 ..]

lookupLocal :: Names -> [(Word64, Symbol)] -> Word64 -> Maybe (Pretty SyntaxText)
lookupLocal vns localNames i = var vns <$> lookup i localNames

-- | The suffix that names a combinator of a group by its index
-- (Unison.Runtime.MCode.Explode: 'specIndex', 'retIndex', 'joinIndex'):
-- nothing for the entry, @#spec1@ for a specialisation, @#return@ for the
-- product-return worker, @#join1@ after its owner's for a join point, and
-- @#f1@ (the local function's name, when known) for a local function.
indexLabel :: (Word64 -> Maybe (Pretty SyntaxText)) -> Word64 -> Pretty SyntaxText
indexLabel localName i
  | k <- i `shiftR` 48,
    k /= 0 =
      indexLabel localName (i .&. (bit 48 - 1)) <> "#join" <> P.shown k
  | i == 0 = mempty
  | testBit i 15 =
      let k = i `shiftR` 16
       in if k == 0 then "#return" else "#spec" <> P.shown k
  | i .&. 0xffff == 0 = "#" <> fromMaybe ("local" <> P.shown (i `shiftR` 16)) (localName i)
  | otherwise = "#" <> P.shown i

definition :: IRNames -> Names -> Pretty SyntaxText -> SuperNormal Reference Symbol -> [Stmt] -> Stmt
definition irn vns0 name (Lambda ccs (ABTN.TAbss vs body)) locals =
  binding conventionNote lhs "=" (locals ++ block irn vns body)
  where
    vns = vns0 {tupleTails = tupleSugar body}
    lhs = P.sep " " (name : map (var vns) vs)
    conventionNote
      | all (== BX) ccs = Nothing
      | otherwise =
          Just . comment $
            "calling convention: "
              <> P.sep " " [var vns v <> ":" <> P.shown c | (v, c) <- zip vs ccs]

-- | `lhs = e` on one line, or `lhs =` and the block indented below it.
binding :: Maybe (Pretty SyntaxText) -> Pretty SyntaxText -> Pretty SyntaxText -> [Stmt] -> Stmt
binding note lhs eq stmts = case stmts of
  [Stmt False e] -> Stmt (isJust note) $ withNote (lhs <> " " <> kw eq <> " " <> e)
  _ -> Stmt True $ withNote (lhs <> " " <> kw eq) `above` indentBlock stmts
  where
    withNote p = maybe p (`above` p) note

above :: Pretty SyntaxText -> Pretty SyntaxText -> Pretty SyntaxText
above a b = P.lines [a, b]

indentBlock :: [Stmt] -> Pretty SyntaxText
indentBlock stmts = P.indentN 2 . P.lines $ [p | Stmt _ p <- stmts]

-- | The statements of a block: its bindings, then its result.
block :: IRNames -> Names -> ANormal Reference Symbol -> [Stmt]
block irn vns tm = case tm of
  -- the inner pair of a tuple shown as one ('tupleSugar')
  TLets _ [v] _ _ bo | Map.member v (tupleTails vns) -> block irn vns bo
  -- a box: the variable is an ordinary value from here on
  TLets _ [] _ (TBox v) bo ->
    Stmt False (var vns' v <> " " <> kw "=" <> " " <> kw "box" <> " " <> var vns v)
      : block irn vns' bo
    where
      vns' = vns {wideVars = Set.delete v (wideVars vns)}
  -- a let that binds nothing: its binding, as an action
  TLets _ [] _ bn bo -> block irn vns bn ++ block irn vns bo
  -- a wide variable (Unison.Runtime.MCode.Wide)
  TLets _ [v] [WD] bn bo
    | wideOn ->
        let vns' = vns {wideVars = Set.insert v (wideVars vns)}
         in binding Nothing (var vns' v) "=" (block irn vns bn) : block irn vns' bo
  TLets _ vs _ bn bo ->
    binding Nothing (lhsVars vs) "=" (block irn vns bn) : block irn vns bo
  TName v f as bo ->
    Stmt False (var vns v <> " " <> kw "=" <> " " <> app (Head False (kw "partialApply")) (headText (nameHead f) : map (var vns) as))
      : block irn vns bo
  TMatch v bs ->
    [Stmt True $ (kw "match" <> " " <> var vns v <> " " <> kw "with") `above` P.indentN 2 (P.lines $ branches irn vns bs)]
  TShift r v bo ->
    Stmt False (var vns v <> " " <> kw "=" <> " " <> kw "shift" <> " " <> abilities irn [r])
      : block irn vns bo
  THnd rs nh ah bo ->
    [ Stmt True $
        P.lines
          [ kw "handle" <> " " <> abilities irn rs,
            indentBlock (block irn vns bo),
            kw "with" <> " " <> case ah of
              Nothing -> var vns nh
              Just a ->
                kw "if" <> " handle.env.allAffine " <> kw "then" <> " " <> var vns a <> " " <> kw "else" <> " " <> var vns nh
          ]
    ]
  TLocal h bo ->
    [ Stmt True $
        P.lines
          [ kw "handle.affine",
            indentBlock (block irn vns bo),
            kw "with" <> " " <> var vns h
          ]
    ]
  _ -> [Stmt False (expr irn vns tm)]
  where
    lhsVars [] = "_"
    lhsVars [v] = var vns v
    lhsVars vs = values (map (var vns) vs)
    -- prefix even for an operator: a closure is not a call
    nameHead (Left r@(Builtin _)) = builtinFunction irn r
    nameHead (Left r) = case refHead irn (combName irn r) of
      Head True op -> Head False (P.parenthesize op)
      h -> h
    nameHead (Right v) = Head False (var vns v)

-- | A term that is a single expression.
expr :: IRNames -> Names -> ANormal Reference Symbol -> Pretty SyntaxText
expr irn vns tm = case tm of
  TLit l -> lit irn l
  TBLit l -> lit irn l
  TFrc v -> var vns v <> fmt S.DelayForceChar "()"
  TVar v -> var vns v
  TApp (FCon r t) [a, w]
    | r == pairRef,
      rawTag t == 0,
      Just es <- Map.lookup w (tupleTails vns) ->
        P.parenthesizeCommas (map (var vns) (a : es))
  TApp f as -> func irn vns f (map (var vns) as)
  TDiscard h -> kw "affine.discard" <> " " <> var vns h
  TUpdate _ h v -> kw "affine.set" <> " " <> var vns h <> " " <> var vns v
  TBox v -> kw "box" <> " " <> var vns v
  -- the multi-line forms, should one be nested in an expression
  _ -> P.lines [p | Stmt _ p <- block irn vns tm]

func :: IRNames -> Names -> Func Reference Symbol -> [Pretty SyntaxText] -> Pretty SyntaxText
func irn vns f as = case f of
  FVar v -> app (Head False $ var vns v) as
  FCont v -> app (Head False $ var vns v) as
  FComb r@(Builtin _) -> app (builtinFunction irn r) as
  FComb r -> app (refHead irn $ combName irn r) as
  FCon r t
    | r == Ty.booleanRef, [] <- as -> fmt S.BooleanLiteral (if rawTag t == 0 then "false" else "true")
    | r == unitRef, [] <- as -> fmt S.Unit "()"
    | otherwise -> app (constructor irn CT.Data r t) as
  FReq r t -> app (constructor irn CT.Effect r t) as
  FPrim (Left op) -> app (primitive irn ("prim." <> tShow op) (Map.lookup op builtinPrimNames)) as
  FPrim (Right ff) -> app (primitive irn ("foreign." <> tShow ff) (Map.lookup ff builtinForeignNames)) as
  FRec (RecordSchema fields) ->
    record [fieldName fld <> ": " <> a | (fld, a) <- zip (Set.toList fields) as]
  FRecSet fld
    | [r, v] <- as -> r <> "@" <> fieldName fld <> ".set " <> v
    | otherwise -> app (Head False $ "recordSet." <> P.text fld) as
  FWorker r i -> app (Head False $ headText (refHead irn $ combName irn r) <> indexLabel (localName irn r) i) as
  FYield -> kw "yield" <> " " <> values as
  where
    record ps = "{ " <> P.sep ", " ps <> " }"

-- | A primitive operation or foreign function, as the builtin it is, or
-- the fallback when it is none.
primitive :: IRNames -> Text -> Maybe Text -> Head
primitive irn _ (Just name) = refHead irn . Right . termName (ppe irn) . Referent.Ref $ Builtin name
primitive _ fallback Nothing = Head False . fmt S.ControlKeyword $ P.text fallback

-- | A builtin's function, called (or closed over) rather than inlined:
-- `(builtin Nat.+)`, prefix.
builtinFunction :: IRNames -> Reference -> Head
builtinFunction irn r =
  Head False . P.parenthesize $ kw "builtin" <> " " <> headText (refHead irn (combName irn r))

app :: Head -> [Pretty SyntaxText] -> Pretty SyntaxText
app (Head True op) [x, y] = P.sep " " [x, op, y]
app (Head True op) as = P.sep " " (P.parenthesize op : as)
app (Head False f) as = P.sep " " (f : as)

refHead :: IRNames -> Either (Pretty SyntaxText) (HQ.HashQualified Name) -> Head
refHead _ (Left p) = Head False p
refHead _ (Right hq) = Head (maybe False Name.isSymboly (HQ.toName hq)) (prettyHashQualified hq)

constructor :: IRNames -> CT.ConstructorType -> Reference -> CTag -> Head
constructor irn ct r t =
  refHead irn . Right . termName (ppe irn) $
    Referent.Con (ConstructorReference r (fromIntegral (rawTag t))) ct

abilities :: IRNames -> [Reference] -> Pretty SyntaxText
abilities irn rs =
  fmt S.AbilityBraces "{" <> P.sep ", " (prettyHashQualified . typeName (ppe irn) <$> rs) <> fmt S.AbilityBraces "}"

branches :: IRNames -> Names -> Branched Reference (ANormal Reference Symbol) -> [Pretty SyntaxText]
branches irn vns bs = case bs of
  MatchEmpty -> [comment "no cases"]
  MatchIntegral cs df -> cases (first (patWith . P.shown) <$> EC.mapToList cs) df
  MatchNumeric r cs df -> cases (first (patWith . numeric r) <$> EC.mapToList cs) df
  MatchText cs df -> cases (first (patWith . textLit) <$> Map.toList cs) df
  MatchBytes cs df -> cases (first (patWith . bytesLit) <$> Map.toList cs) df
  MatchData r cs df ->
    cases [(dataPat r t, e) | (t, (_, e)) <- EC.mapToList cs] df
  MatchRequest abs df ->
    [ caseOf (\vs -> braces . app (constructor irn CT.Effect r t) $ map (var vns) vs) e
    | (r, cs) <- abs,
      (t, (_, e)) <- EC.mapToList cs
    ]
      ++ [caseOf (braces . P.sep " " . map (var vns)) df]
  MatchSum cs -> cases [(\vs -> P.sep " " ("tag" : P.shown t : map (var vns) vs), e) | (t, (_, e)) <- EC.mapToList cs] Nothing
  MatchRec (RecordSchema fields) e ->
    [caseOf (\vs -> "{ " <> P.sep ", " [fieldName f <> ": " <> var vns v | (f, v) <- zip (Set.toList fields) vs] <> " }") e]
  where
    cases :: [([Symbol] -> Pretty SyntaxText, ANormal Reference Symbol)] -> Maybe (ANormal Reference Symbol) -> [Pretty SyntaxText]
    cases cs df =
      [caseOf p e | (p, e) <- cs] ++ maybe [] (pure . caseOf defaultPat) df
    -- a literal pattern binds nothing
    patWith p [] = p
    patWith p vs = P.sep " " (p : map (var vns) vs)
    defaultPat [] = fmt S.Blank "_"
    defaultPat vs = P.sep " " (map (var vns) vs)
    dataPat r t vs
      | r == Ty.booleanRef, [] <- vs = fmt S.BooleanLiteral (if rawTag t == 0 then "false" else "true")
      | r == unitRef, [] <- vs = fmt S.Unit "()"
      | otherwise = app (constructor irn CT.Data r t) (map (var vns) vs)
    caseOf pat (ABTN.TAbss vs e) = case binding Nothing (pat vs) "->" (block irn vns e) of
      Stmt _ p -> p
    braces p = fmt S.AbilityBraces "{" <> p <> fmt S.AbilityBraces "}"

numeric :: Reference -> Word64 -> Pretty SyntaxText
numeric r w
  | r == Ty.intRef = lit' (I $ fromIntegral w)
  | r == Ty.charRef = lit' (C . toEnum $ fromIntegral w)
  | r == Ty.floatRef = lit' (F $ castWord64ToDouble w)
  | otherwise = lit' (N w)
  where
    lit' :: Lit Reference -> Pretty SyntaxText
    lit' = lit IRNames {combName = const (Left mempty), ppe = mempty, localName = \_ _ -> Nothing}

lit :: IRNames -> Lit Reference -> Pretty SyntaxText
lit irn l = case l of
  N n -> fmt S.NumericLiteral (P.shown n)
  I i
    | i >= 0 -> fmt S.NumericLiteral ("+" <> P.shown i)
    | otherwise -> fmt S.NumericLiteral (P.shown i)
  F d -> fmt S.NumericLiteral (P.shown d)
  T t -> textLit t
  C c -> fmt S.CharLiteral ("?" <> P.string (init . drop 1 $ show c))
  LM (Rfn.Ref' r) -> fmt S.LinkKeyword "termLink" <> " " <> either id prettyHashQualified (combName irn r)
  LM (Rfn.Con' (ConstructorReference r t) ct) ->
    fmt S.LinkKeyword "termLink" <> " " <> prettyHashQualified (termName (ppe irn) $ Referent.Con (ConstructorReference r t) ct)
  LY r -> fmt S.LinkKeyword "typeLink" <> " " <> prettyHashQualified (typeName (ppe irn) r)

textLit :: Util.Text.Text -> Pretty SyntaxText
textLit = fmt S.TextLiteral . P.shown . Util.Text.toString

bytesLit :: Bytes.Bytes -> Pretty SyntaxText
bytesLit b = fmt S.BytesLiteral . P.string $ "0xs" ++ concatMap hex (Bytes.toWord8s b)
  where
    hex w = showHex (w `shiftR` 4) (showHex (w .&. 15) "")

fieldName :: Text -> Pretty SyntaxText
fieldName = fmt (S.RecordFieldName "") . P.text

var :: Names -> Symbol -> Pretty SyntaxText
var vns v
  | Set.member v (wideVars vns) = fmt S.Var "#" <> plain
  | otherwise = plain
  where
    plain = fromMaybe (fmt S.Var . P.text $ Var.name v) (Map.lookup v (varNames vns))

-- | Several values, not a tuple: `(# x, y #)`.
values :: [Pretty SyntaxText] -> Pretty SyntaxText
values ps = "(# " <> P.sep ", " ps <> " #)"

-- * Tuples

-- | The variables of a lambda's body that are bound to the inner pairs of a
-- tuple shown as one, with the elements of each. A tuple is nested pairs
-- ending in `()`, and in ANF each pair is a let: `v1 = ()`,
-- `v2 = Cons y v1`, `Cons x v2`. A pair whose second field is a variable
-- bound to `()` or to such a pair, and used only there, is shown with the
-- elements of the tuple, `(x, y)`, when that makes two or more, and the
-- lets of those variables are not shown: they are part of it. A variable
-- used anywhere else keeps its let, so the sugar hides no sharing.
tupleSugar :: ANormal Reference Symbol -> Map Symbol [Symbol]
tupleSugar body = Map.fromList [(w, elems w) | w <- Set.toList folded]
  where
    uses = useCounts body
    once v = Map.lookup v uses == Just 1
    -- the variables a pair's second field can be folded through: bound to
    -- `()`, or to a pair whose second field is one, and used once
    chains :: Map Symbol (Maybe Symbol, [Symbol])
    chains = foldl' add mempty (singleLets body)
    add m (v, bn)
      | not (once v) = m
      | TCon r _ [] <- bn, r == unitRef = Map.insert v (Nothing, []) m
      | TCon r t [a, w] <- bn,
        r == pairRef,
        rawTag t == 0,
        Just (_, es) <- Map.lookup w m =
          Map.insert v (Just w, a : es) m
      | otherwise = m
    elems w = maybe [] snd (Map.lookup w chains)
    -- every pair with two or more elements folds its second field's chain
    folded =
      Set.fromList
        [ u
        | (_, w) <- pairs body,
          Just (_, es) <- [Map.lookup w chains],
          length es >= 1,
          u <- chainOf w
        ]
    chainOf w = w : maybe [] chainOf (fst =<< Map.lookup w chains)

-- | How many times each variable occurs in a term (binders aside).
useCounts :: ANormal Reference Symbol -> Map Symbol Int
useCounts (ABTN.Term _ abt) = case abt of
  ABTN.Abs _ bd -> useCounts bd
  ABTN.Tm f -> bifoldr (\v -> Map.insertWith (+) v 1) (Map.unionWith (+) . useCounts) mempty f

-- | The lets of one variable in a term, outermost first, so that a
-- variable's let comes before the lets that use it.
singleLets :: ANormal Reference Symbol -> [(Symbol, ANormal Reference Symbol)]
singleLets tm = case tm of
  TLets _ [v] _ bn bo -> (v, bn) : singleLets bn ++ singleLets bo
  ABTN.Term _ (ABTN.Abs _ bd) -> singleLets bd
  ABTN.Term _ (ABTN.Tm f) -> bifoldMap (const []) singleLets f

-- | The pairs a term builds: each one's fields.
pairs :: ANormal Reference Symbol -> [(Symbol, Symbol)]
pairs tm = here ++ below
  where
    here = case tm of
      TCon r t [a, w] | r == pairRef, rawTag t == 0 -> [(a, w)]
      _ -> []
    below = case tm of
      ABTN.Term _ (ABTN.Abs _ bd) -> pairs bd
      ABTN.Term _ (ABTN.Tm f) -> bifoldMap (const []) pairs f

kw :: Pretty SyntaxText -> Pretty SyntaxText
kw = fmt S.ControlKeyword

comment :: Pretty SyntaxText -> Pretty SyntaxText
comment p = fmt S.DocDelimiter ("-- " <> p)

fmt :: S.Element Reference -> Pretty SyntaxText -> Pretty SyntaxText
fmt = P.withSyntax

-- * Variable names

-- | The variables a function binds, in the order they are bound.
superBinders :: SuperNormal Reference Symbol -> [Symbol]
superBinders (Lambda _ tm) = binders tm

binders :: ANormal Reference Symbol -> [Symbol]
binders (ABTN.Term _ abt) = case abt of
  ABTN.Abs v bd -> v : binders bd
  ABTN.Tm f -> bifoldMap (const []) binders f

-- | The affine cells of a lambda: the variables `handle.affine`,
-- `affine.set` and `affine.discard` act on.
affineCells :: SuperNormal Reference Symbol -> Set Symbol
affineCells (Lambda _ tm) = go tm
  where
    go t@(ABTN.Term _ abt) = here t <> below abt
    here = \case
      TLocal h _ -> Set.singleton h
      TUpdate _ h _ -> Set.singleton h
      TDiscard h -> Set.singleton h
      _ -> mempty
    below = \case
      ABTN.Abs _ bd -> go bd
      ABTN.Tm f -> bifoldMap (const mempty) go f

-- | The continuations a lambda captures: the variables `shift` binds.
continuations :: SuperNormal Reference Symbol -> Set Symbol
continuations (Lambda _ tm) = go tm
  where
    go t@(ABTN.Term _ abt) = here t <> below abt
    here = \case
      TShift _ k _ -> Set.singleton k
      _ -> mempty
    below = \case
      ABTN.Abs _ bd -> go bd
      ABTN.Tm f -> bifoldMap (const mempty) go f

-- | Display names for the variables of a definition, given its affine
-- cells, its continuations, its local functions and then the variables it
-- binds: a user's variable keeps its name unless that is taken (also when
-- the pattern compiler renamed it, 'Var.PatternNamed'), an affine cell is
-- `aff`, a continuation `cont1`, `cont2`, ..., and another generated one
-- `v1`, `v2`, ... (`f1`, ... for a local function).
nameVars :: Set Symbol -> Set Symbol -> [Symbol] -> [Symbol] -> Names
nameVars cells conts fs vs =
  Names (evalState (foldM (add "f") mempty fs >>= \m -> foldM (add "v") m vs) mempty) mempty mempty
  where
    add :: Text -> Map Symbol (Pretty SyntaxText) -> Symbol -> State (Set Text) (Map Symbol (Pretty SyntaxText))
    add dflt m v
      | Map.member v m = pure m
      | otherwise = do
          n <- fresh (base dflt v)
          pure $ Map.insert v (fmt S.Var $ P.text n) m
    base dflt v
      | Set.member v cells = ("aff", False)
      | Set.member v conts = ("cont", True)
      | otherwise = case Var.typeOf v of
          Var.User n -> user n
          Var.PatternNamed n -> user n
          Var.Pattern -> ("p", True)
          Var.Eta -> ("eta", True)
          _ -> (dflt, True)
    user n
      | generatedName n = ("x", True)
      | otherwise = (n, False)
    -- the parser names the arguments of a `cases` function with ten
    -- characters of base32hex and a number (TermParser.lamCase)
    generatedName n =
      let (h, rest) = Text.splitAt 10 n
       in Text.length h == 10
            && Text.all (\c -> isDigit c || (c >= 'a' && c <= 'v')) h
            && not (Text.null rest)
            && Text.all isDigit rest
    fresh :: (Text, Bool) -> State (Set Text) Text
    fresh (b, numbered) = do
      let candidates = [b | not numbered] ++ [b <> tShow i | i <- [1 :: Int ..]]
      n <- firstFree candidates
      modify (Set.insert n)
      pure n
    firstFree (c : cs) = gets (Set.member c) >>= \taken -> if taken then firstFree cs else pure c
    firstFree [] = error "nameVars: impossible"
