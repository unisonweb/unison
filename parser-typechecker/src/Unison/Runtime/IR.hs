{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.IR where

import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Data.Bifunctor (first, second)
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import Unison.Hash (Hash)
import Unison.NamePrinter (prettyHashQualified0)
import qualified Unison.PatternCompat as Pattern
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as R
import Unison.Referent (Referent)
import qualified Unison.Runtime.ANF as ANF
import Unison.Symbol (Symbol)
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TP
import qualified Unison.Util.Bytes as Bytes
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.CycleTable as CyT
import Unison.Util.CyclicEq (CyclicEq, cyclicEq)
import Unison.Util.CyclicOrd (CyclicOrd, cyclicOrd)
import qualified Unison.Util.CyclicOrd as COrd
import Unison.Util.Monoid (intercalateMap)
import qualified Unison.Util.Pretty as P
import Unison.Var (Var)
import qualified Unison.Var as Var

type Pos = Int

type Arity = Int

type ConstructorId = Int

type Term v = Term.Term v ()

data CompilationEnv e cont = CompilationEnv
  { toIR' :: Map R.Reference (IR e cont),
    constructorArity' :: Map (R.Reference, Int) Int
  }

toIR :: CompilationEnv e cont -> R.Reference -> Maybe (IR e cont)
toIR = flip Map.lookup . toIR'

constructorArity :: CompilationEnv e cont -> R.Reference -> Int -> Maybe Int
constructorArity e r i = Map.lookup (r, i) $ constructorArity' e

-- SymbolC = Should this variable be compiled as a LazySlot?
data SymbolC = SymbolC
  { isLazy :: Bool,
    underlyingSymbol :: Symbol
  } -- deriving Show

instance Show SymbolC where
  show (SymbolC lazy s) = (if lazy then "'" else "") <> show s

makeLazy :: SymbolC -> SymbolC
makeLazy s = s {isLazy = True}

toSymbolC :: Symbol -> SymbolC
toSymbolC = SymbolC False

-- Values, in normal form
type RefID = Int

data Value e cont
  = I Int64
  | F Double
  | N Word64
  | B Bool
  | T Text
  | C Char
  | Bs Bytes.Bytes
  | TermLink Referent
  | TypeLink R.Reference
  | Lam Arity (UnderapplyStrategy e cont) (IR e cont)
  | Data R.Reference ConstructorId [Value e cont]
  | Sequence (Sequence.Seq (Value e cont))
  | Ref RefID Symbol (IORef (Value e cont))
  | Pure (Value e cont)
  | Requested (Req e cont)
  | Cont cont
  | UninitializedLetRecSlot Symbol [(Symbol, IR e cont)] (IR e cont)

instance (Eq cont, Eq e) => Eq (Value e cont) where
  I x == I y = x == y
  F x == F y = x == y
  N x == N y = x == y
  B x == B y = x == y
  T x == T y = x == y
  C x == C y = x == y
  Bs x == Bs y = x == y
  Lam n us _ == Lam n2 us2 _ = n == n2 && us == us2
  Data r1 cid1 vs1 == Data r2 cid2 vs2 = r1 == r2 && cid1 == cid2 && vs1 == vs2
  Sequence vs == Sequence vs2 = vs == vs2
  Ref _ _ io1 == Ref _ _ io2 = io1 == io2
  Pure x == Pure y = x == y
  Requested r1 == Requested r2 = r1 == r2
  Cont k1 == Cont k2 = k1 == k2
  TermLink r1 == TermLink r2 = r1 == r2
  TypeLink r1 == TypeLink r2 = r1 == r2
  _ == _ = False

instance (Eq cont, Eq e) => Eq (UnderapplyStrategy e cont) where
  FormClosure h _ vs == FormClosure h2 _ vs2 = h == h2 && vs == vs2
  Specialize h _ vs == Specialize h2 _ vs2 = h == h2 && vs == vs2
  _ == _ = False

-- would have preferred to make pattern synonyms
maybeToOptional :: Maybe (Value e cont) -> Value e cont
maybeToOptional = \case
  Just a -> Data DD.optionalRef 1 [a]
  Nothing -> Data DD.optionalRef 0 []

unit :: Value e cont
unit = Data DD.unitRef 0 []

pair :: (Value e cont, Value e cont) -> Value e cont
pair (a, b) = Data DD.pairRef 0 [a, b]

tuple :: [Value e cont] -> Value e cont
tuple [v] = v
tuple vs = foldr (curry pair) unit vs

-- When a lambda is underapplied, for instance, `(x y -> x) 19`, we can do
-- one of two things: we can substitute away the arguments that have
-- been applied, in this example, creating the lambda `x -> 19`. This
-- is called specialization and requires recompiling the lambda with its new
-- body.
--
-- The other option is to just stash the arguments until the rest of the
-- args are supplied later. This keeps the original lambda around and
-- doesn't involve recompiling. This would just create the closure
-- `((x y -> x) 19)`, which when given one more arg, would call the original
-- `x y -> x` function with both arguments.
--
-- Specialization can be done for any Unison term definition, like
--
--   blah x y = x + y
--
-- Closure formation is used for:
--
--   * builtin functions
--   * constructor functions
--
-- The reason is that builtins and constructor functions don't have a body
-- with variables that we could substitute - the functions only compute
-- to anything when all the arguments are available.

data UnderapplyStrategy e cont
  = FormClosure Hash (Term SymbolC) [Value e cont] -- head is the latest argument
  | Specialize Hash (Term SymbolC) [(SymbolC, Value e cont)] -- same
  deriving (Show)

decompileUnderapplied :: (External e, External cont) => UnderapplyStrategy e cont -> DS (Term Symbol)
decompileUnderapplied u = case u of -- todo: consider unlambda-lifting here
  FormClosure _ lam vals ->
    Term.apps' (Term.vmap underlyingSymbol lam) . reverse
      <$> traverse decompileImpl vals
  Specialize _ lam symvals -> do
    lam <-
      Term.apps' (Term.vmap underlyingSymbol lam) . reverse
        <$> traverse (decompileImpl . snd) symvals
    pure $ Term.betaReduce lam

-- Patterns - for now this follows Unison.Pattern exactly, but
-- we may switch to more efficient runtime representation of patterns
data Pattern
  = PatternI Int64
  | PatternF Double
  | PatternN Word64
  | PatternB Bool
  | PatternT Text
  | PatternC Char
  | PatternData R.Reference ConstructorId [Pattern]
  | PatternSequenceLiteral [Pattern]
  | PatternSequenceCons Pattern Pattern
  | PatternSequenceSnoc Pattern Pattern
  | -- `Either Int Int` here represents the known constant length of either
    -- the left or right side of a sequence concat operation
    PatternSequenceConcat (Either Int Int) Pattern Pattern
  | PatternPure Pattern
  | PatternBind R.Reference ConstructorId [Pattern] Pattern
  | PatternAs Pattern
  | PatternIgnore
  | PatternVar
  deriving (Eq, Show)

-- Leaf level instructions - these return immediately without using any stack
data Z e cont
  = Slot Pos
  | LazySlot Pos
  | Val (Value e cont)
  | External e
  deriving (Eq)

-- The `Set Int` is the set of de bruijn indices that are free in the body
-- of `Let` instructions.
type IR e cont = IR' (Set Int) (Z e cont)

-- Computations - evaluation reduces these to values
data IR' ann z
  = Leaf z
  | -- Ints
    AddI z z
  | SubI z z
  | MultI z z
  | DivI z z
  | GtI z z
  | LtI z z
  | GtEqI z z
  | LtEqI z z
  | EqI z z
  | SignumI z
  | NegateI z
  | Truncate0I z
  | ModI z z
  | PowI z z
  | ShiftLI z z
  | ShiftRI z z
  | BitAndI z z
  | BitOrI z z
  | BitXorI z z
  | ComplementI z
  | LeadZeroI z
  | TrailZeroI z
  | -- Nats
    AddN z z
  | DropN z z
  | SubN z z
  | MultN z z
  | DivN z z
  | GtN z z
  | LtN z z
  | GtEqN z z
  | LtEqN z z
  | EqN z z
  | ModN z z
  | ToIntN z
  | PowN z z
  | ShiftLN z z
  | ShiftRN z z
  | BitOrN z z
  | BitXorN z z
  | BitAndN z z
  | ComplementN z
  | LeadZeroN z
  | TrailZeroN z
  | -- Floats
    AddF z z
  | SubF z z
  | MultF z z
  | DivF z z
  | GtF z z
  | LtF z z
  | GtEqF z z
  | LtEqF z z
  | EqF z z
  | -- Universals
    EqU z z -- universal equality
  | CompareU z z -- universal ordering
  -- Debugging/Utilities
  | Todo z
  | Bug z
  | -- Control flow

    -- `Let` has an `ann` associated with it, e.g `ann = Set Int` which is the
    -- set of "free" stack slots referenced by the body of the `let`
    Let Symbol (IR' ann z) (IR' ann z) ann
  | LetRec [(Symbol, IR' ann z)] (IR' ann z)
  | MakeSequence [z]
  | Apply (IR' ann z) [z]
  | Construct R.Reference ConstructorId [z]
  | Request R.Reference ConstructorId [z]
  | Handle z (IR' ann z)
  | If z (IR' ann z) (IR' ann z)
  | And z (IR' ann z)
  | Or z (IR' ann z)
  | Not z
  | -- pattern, optional guard, rhs
    Match z [(Pattern, [Symbol], Maybe (IR' ann z), IR' ann z)]
  deriving (Functor, Foldable, Traversable, Eq, Show)

prettyZ ::
  PPE.PrettyPrintEnv ->
  (e -> P.Pretty String) ->
  (cont -> P.Pretty String) ->
  Z e cont ->
  P.Pretty String
prettyZ ppe prettyE prettyCont z = case z of
  Slot i -> "@" <> P.shown i
  LazySlot i -> "'@" <> P.shown i
  Val v -> prettyValue ppe prettyE prettyCont v
  External e -> "External" `P.hang` prettyE e

prettyIR ::
  PPE.PrettyPrintEnv ->
  (e -> P.Pretty String) ->
  (cont -> P.Pretty String) ->
  IR e cont ->
  P.Pretty String
prettyIR ppe prettyE prettyCont = pir
  where
    unlets (Let s hd tl _) = (Just s, hd) : unlets tl
    unlets e = [(Nothing, e)]
    pz = prettyZ ppe prettyE prettyCont
    pir ir = case ir of
      Leaf z -> pz z
      AddI a b -> P.parenthesize $ "AddI" `P.hang` P.spaced [pz a, pz b]
      SubI a b -> P.parenthesize $ "SubI" `P.hang` P.spaced [pz a, pz b]
      MultI a b -> P.parenthesize $ "MultI" `P.hang` P.spaced [pz a, pz b]
      DivI a b -> P.parenthesize $ "DivI" `P.hang` P.spaced [pz a, pz b]
      GtI a b -> P.parenthesize $ "GtI" `P.hang` P.spaced [pz a, pz b]
      LtI a b -> P.parenthesize $ "LtI" `P.hang` P.spaced [pz a, pz b]
      GtEqI a b -> P.parenthesize $ "GtEqI" `P.hang` P.spaced [pz a, pz b]
      LtEqI a b -> P.parenthesize $ "LtEqI" `P.hang` P.spaced [pz a, pz b]
      EqI a b -> P.parenthesize $ "EqI" `P.hang` P.spaced [pz a, pz b]
      SignumI a -> P.parenthesize $ "SignumI" `P.hang` P.spaced [pz a]
      NegateI a -> P.parenthesize $ "NegateI" `P.hang` P.spaced [pz a]
      Truncate0I a -> P.parenthesize $ "Truncate0I" `P.hang` P.spaced [pz a]
      ModI a b -> P.parenthesize $ "ModI" `P.hang` P.spaced [pz a, pz b]
      PowI a b -> P.parenthesize $ "PowI" `P.hang` P.spaced [pz a, pz b]
      ShiftRI a b -> P.parenthesize $ "ShiftRI" `P.hang` P.spaced [pz a, pz b]
      ShiftLI a b -> P.parenthesize $ "ShiftLI" `P.hang` P.spaced [pz a, pz b]
      BitAndI a b -> P.parenthesize $ "BitAndI" `P.hang` P.spaced [pz a, pz b]
      BitOrI a b -> P.parenthesize $ "BitOrI" `P.hang` P.spaced [pz a, pz b]
      BitXorI a b -> P.parenthesize $ "BitXorI" `P.hang` P.spaced [pz a, pz b]
      ComplementI a -> P.parenthesize $ "ComplementI" `P.hang` P.spaced [pz a]
      LeadZeroI a -> P.parenthesize $ "LeadZeroI" `P.hang` P.spaced [pz a]
      TrailZeroI a -> P.parenthesize $ "TrailZeroI" `P.hang` P.spaced [pz a]
      AddN a b -> P.parenthesize $ "AddN" `P.hang` P.spaced [pz a, pz b]
      SubN a b -> P.parenthesize $ "SubN" `P.hang` P.spaced [pz a, pz b]
      DropN a b -> P.parenthesize $ "DropN" `P.hang` P.spaced [pz a, pz b]
      MultN a b -> P.parenthesize $ "MultN" `P.hang` P.spaced [pz a, pz b]
      DivN a b -> P.parenthesize $ "DivN" `P.hang` P.spaced [pz a, pz b]
      GtN a b -> P.parenthesize $ "GtN" `P.hang` P.spaced [pz a, pz b]
      LtN a b -> P.parenthesize $ "LtN" `P.hang` P.spaced [pz a, pz b]
      GtEqN a b -> P.parenthesize $ "GtEqN" `P.hang` P.spaced [pz a, pz b]
      LtEqN a b -> P.parenthesize $ "LtEqN" `P.hang` P.spaced [pz a, pz b]
      EqN a b -> P.parenthesize $ "EqN" `P.hang` P.spaced [pz a, pz b]
      ModN a b -> P.parenthesize $ "ModN" `P.hang` P.spaced [pz a, pz b]
      ToIntN a -> P.parenthesize $ "ToIntN" `P.hang` P.spaced [pz a]
      PowN a b -> P.parenthesize $ "PowN" `P.hang` P.spaced [pz a, pz b]
      ShiftLN a b -> P.parenthesize $ "ShiftLN" `P.hang` P.spaced [pz a, pz b]
      ShiftRN a b -> P.parenthesize $ "ShiftRN" `P.hang` P.spaced [pz a, pz b]
      BitAndN a b -> P.parenthesize $ "BitAndN" `P.hang` P.spaced [pz a, pz b]
      BitOrN a b -> P.parenthesize $ "BitOrN" `P.hang` P.spaced [pz a, pz b]
      BitXorN a b -> P.parenthesize $ "BitXorN" `P.hang` P.spaced [pz a, pz b]
      ComplementN a -> P.parenthesize $ "ComplementN" `P.hang` P.spaced [pz a]
      LeadZeroN a -> P.parenthesize $ "LeadZeroN" `P.hang` P.spaced [pz a]
      TrailZeroN a -> P.parenthesize $ "TrailZeroN" `P.hang` P.spaced [pz a]
      AddF a b -> P.parenthesize $ "AddF" `P.hang` P.spaced [pz a, pz b]
      SubF a b -> P.parenthesize $ "SubF" `P.hang` P.spaced [pz a, pz b]
      MultF a b -> P.parenthesize $ "MultF" `P.hang` P.spaced [pz a, pz b]
      DivF a b -> P.parenthesize $ "DivF" `P.hang` P.spaced [pz a, pz b]
      GtF a b -> P.parenthesize $ "GtF" `P.hang` P.spaced [pz a, pz b]
      LtF a b -> P.parenthesize $ "LtF" `P.hang` P.spaced [pz a, pz b]
      GtEqF a b -> P.parenthesize $ "GtEqF" `P.hang` P.spaced [pz a, pz b]
      LtEqF a b -> P.parenthesize $ "LtEqF" `P.hang` P.spaced [pz a, pz b]
      EqF a b -> P.parenthesize $ "EqF" `P.hang` P.spaced [pz a, pz b]
      EqU a b -> P.parenthesize $ "EqU" `P.hang` P.spaced [pz a, pz b]
      CompareU a b -> P.parenthesize $ "CompareU" `P.hang` P.spaced [pz a, pz b]
      Bug a -> P.parenthesize $ "Bug" `P.hang` P.spaced [pz a]
      Todo a -> P.parenthesize $ "Todo" `P.hang` P.spaced [pz a]
      ir@Let {} ->
        P.group $ "let" `P.hang` P.lines (blockElem <$> block)
        where
          block = unlets ir
          blockElem (Nothing, binding) = pir binding
          blockElem (Just name, binding) =
            (P.shown name <> " =") `P.hang` pir binding
      LetRec bs body -> P.group $ "letrec" `P.hang` P.lines ls
        where
          blockElem (Nothing, binding) = pir binding
          blockElem (Just name, binding) =
            (P.shown name <> " =") `P.hang` pir binding
          ls =
            fmap blockElem $
              [(Just n, ir) | (n, ir) <- bs]
                ++ [(Nothing, body)]
      MakeSequence vs ->
        P.group $
          P.surroundCommas "[" "]" (pz <$> vs)
      Apply fn args -> P.parenthesize $ pir fn `P.hang` P.spaced (pz <$> args)
      Construct r cid args ->
        P.parenthesize $
          ("Construct " <> prettyHashQualified0 (PPE.patternName ppe r cid))
            `P.hang` P.surroundCommas "[" "]" (pz <$> args)
      Request r cid args ->
        P.parenthesize $
          ("Request " <> prettyHashQualified0 (PPE.patternName ppe r cid))
            `P.hang` P.surroundCommas "[" "]" (pz <$> args)
      Handle h body ->
        P.parenthesize $
          P.group ("Handle " <> pz h) `P.hang` pir body
      If cond t f ->
        P.parenthesize $
          ("If " <> pz cond) `P.hang` P.spaced [pir t, pir f]
      And x y -> P.parenthesize $ "And" `P.hang` P.spaced [pz x, pir y]
      Or x y -> P.parenthesize $ "Or" `P.hang` P.spaced [pz x, pir y]
      Not x -> P.parenthesize $ "Not" `P.hang` pz x
      Match scrute cases ->
        P.parenthesize $
          P.group ("Match " <> pz scrute) `P.hang` P.lines (pcase <$> cases)
        where
          pcase (pat, vs, guard, rhs) =
            let lhs =
                  P.spaced . P.nonEmpty $
                    [P.parenthesize (P.shown pat), P.shown vs, maybe mempty pir guard]
             in (lhs <> " ->" `P.hang` pir rhs)

prettyValue ::
  PPE.PrettyPrintEnv ->
  (e -> P.Pretty String) ->
  (cont -> P.Pretty String) ->
  Value e cont ->
  P.Pretty String
prettyValue ppe prettyE prettyCont = pv
  where
    pv v = case v of
      I i -> (if i >= 0 then "+" else "") <> P.string (show i)
      F d -> P.shown d
      N n -> P.shown n
      B b -> if b then "true" else "false"
      T t -> P.shown t
      C c -> P.shown c
      Bs bs -> P.shown bs
      TermLink r ->
        P.parenthesize $
          ("TermLink " <> prettyHashQualified0 (PPE.termName ppe r))
      TypeLink r ->
        P.parenthesize $
          ("TypeLink " <> prettyHashQualified0 (PPE.typeName ppe r))
      Lam arity _u b ->
        P.parenthesize $
          ("Lambda " <> P.string (show arity))
            `P.hang` prettyIR ppe prettyE prettyCont b
      Data r cid vs ->
        P.parenthesize $
          ("Data " <> prettyHashQualified0 (PPE.patternName ppe r cid))
            `P.hang` P.surroundCommas "[" "]" (pv <$> vs)
      Sequence vs -> P.surroundCommas "[" "]" (pv <$> vs)
      Ref id name _ ->
        P.parenthesize $
          P.sep " " ["Ref", P.shown id, P.shown name]
      Pure v -> P.surroundCommas "{" "}" [pv v]
      Requested (Req r cid vs cont) ->
        P.parenthesize $
          ("Request " <> prettyHashQualified0 (PPE.patternName ppe r cid))
            `P.hang` P.spaced
              [ P.surroundCommas "[" "]" (pv <$> vs),
                prettyCont cont
              ]
      Cont k -> P.parenthesize $ "Cont" `P.hang` prettyCont k
      UninitializedLetRecSlot s _ _ ->
        P.parenthesize $
          "Uninitialized " <> P.shown s

-- Contains the effect ref and ctor id, the args, and the continuation
-- which expects the result at the top of the stack
data Req e cont = Req R.Reference ConstructorId [Value e cont] cont
  deriving (Eq, Show)

-- Annotate all `z` values with the number of outer bindings, useful for
-- tracking free variables or converting away from debruijn indexing.
-- Currently used as an implementation detail by `specializeIR`.
annotateDepth :: IR' a z -> IR' a (z, Int)
annotateDepth = go 0
  where
    go depth ir = case ir of
      -- Only the binders modify the depth
      Let v b body ann -> Let v (go depth b) (go (depth + 1) body) ann
      LetRec bs body ->
        let depth' = depth + length bs
         in LetRec (second (go depth') <$> bs) (go depth' body)
      Match scrute cases -> Match (scrute, depth) (tweak <$> cases)
        where
          tweak (pat, boundVars, guard, rhs) =
            let depth' = depth + length boundVars
             in (pat, boundVars, go depth' <$> guard, go depth' rhs)
      -- All the other cases just leave depth alone and recurse
      Apply f args -> Apply (go depth f) ((,depth) <$> args)
      Handle f body -> Handle (f, depth) (go depth body)
      If c a b -> If (c, depth) (go depth a) (go depth b)
      And a b -> And (a, depth) (go depth b)
      Or a b -> Or (a, depth) (go depth b)
      ir -> (,depth) <$> ir

-- Given an environment mapping of de bruijn indices to values, specialize
-- the given `IR` by replacing slot lookups with the provided values.
specializeIR :: Map Int (Value e cont) -> IR' a (Z e cont) -> IR' a (Z e cont)
specializeIR env ir =
  let ir' = annotateDepth ir
      go (s@(Slot i), depth) = maybe s Val $ Map.lookup (i - depth) env
      go (s@(LazySlot i), depth) = maybe s Val $ Map.lookup (i - depth) env
      go (s, _) = s
   in go <$> ir'

compile :: (Show e, Show cont) => CompilationEnv e cont -> Term Symbol -> IR e cont
compile env t =
  compile0
    env
    []
    (ABT.rewriteDown ANF.minimizeCyclesOrCrash $ Term.vmap toSymbolC t)

freeVars :: [(SymbolC, a)] -> Term SymbolC -> Set SymbolC
freeVars bound t =
  -- let fv = trace "free:" . traceShowId $ ABT.freeVars t
  --     bv = trace "bound:" . traceShowId $ Set.fromList (fst <$> bound)
  -- in trace "difference:" . traceShowId $ fv `Set.difference` bv
  ABT.freeVars t `Set.difference` Set.fromList (fst <$> bound)

-- Main compilation function - converts an arbitrary term to an `IR`.
-- Takes a way of resolving `Reference`s and an environment of variables,
-- some of which may already be precompiled to `V`s. (This occurs when
-- recompiling a function that is being partially applied)
compile0 ::
  (Show e, Show cont) =>
  CompilationEnv e cont ->
  [(SymbolC, Maybe (Value e cont))] ->
  Term SymbolC ->
  IR e cont
compile0 env bound t =
  if Set.null fvs
    then -- Annotates the term with this [(SymbolC, Maybe (Value e))]
    -- where a `Just v` indicates an immediate value, and `Nothing` indicates
    -- a stack lookup is needed at the stack index equal to the symbol's index.
    -- ABT.annotateBound' produces an initial annotation consisting of the a
    -- stack of bound variables, with the innermost bound variable at the top.
    -- We tag each of these with `Nothing`, and then tack on the immediates at
    -- the end.  Their indices don't correspond to stack positions (although
    -- they may reflect shadowing).

      let wrangle vars = ((,Nothing) <$> vars) ++ bound
          t0 = ANF.fromTerm' makeLazy t
          _msg =
            "ANF form:\n"
              <> TP.pretty' (Just 80) mempty t0
              <> "\n---------"
       in go (wrangle <$> ABT.annotateBound' t0)
    else error $ "can't compile a term with free variables: " ++ show (toList fvs)
  where
    fvs = freeVars bound t
    go t = case t of
      Term.Nat' n -> Leaf . Val . N $ n
      Term.Int' n -> Leaf . Val . I $ n
      Term.Float' n -> Leaf . Val . F $ n
      Term.Boolean' n -> Leaf . Val . B $ n
      Term.Text' n -> Leaf . Val . T $ n
      Term.Char' n -> Leaf . Val . C $ n
      Term.TermLink' r -> Leaf . Val . TermLink $ r
      Term.TypeLink' r -> Leaf . Val . TypeLink $ r
      Term.And' x y -> And (toZ "and" t x) (go y)
      Term.LamsNamed' vs body ->
        Leaf . Val $
          Lam
            (length vs)
            (Specialize (ABT.hash t) (void t) [])
            (compile0 env (ABT.annotation body) (void body))
      Term.Or' x y -> Or (toZ "or" t x) (go y)
      Term.Let1Named' v b body -> Let (underlyingSymbol v) (go b) (go body) (freeSlots body)
      Term.LetRecNamed' bs body ->
        LetRec ((\(v, b) -> (underlyingSymbol v, go b)) <$> bs) (go body)
      Term.Constructor' r cid -> ctorIR con (Term.constructor ()) r cid
        where
          con 0 r cid [] = Leaf . Val $ Data r cid []
          con _ r cid args = Construct r cid args
      Term.Request' r cid -> ctorIR (const Request) (Term.request ()) r cid
      Term.Apps' f args -> Apply (go f) (map (toZ "apply-args" t) args)
      Term.Handle' h body -> Handle (toZ "handle" t h) (go body)
      Term.Ann' e _ -> go e
      Term.Match' scrutinee cases ->
        Match (toZ "match" t scrutinee) (compileCase <$> cases)
      ABT.Abs1NA' _ body -> go body
      Term.If' cond ifT ifF -> If (toZ "cond" t cond) (go ifT) (go ifF)
      Term.Var' _ -> Leaf $ toZ "var" t t
      Term.Ref' r -> case toIR env r of
        Nothing -> error $ reportBug "B8920912182" msg
          where
            msg =
              "The program being compiled referenced this definition "
                <> show r
                <> "\nbut the compilation environment has no compiled form for this reference."
        Just ir -> ir
      Term.Sequence' vs -> MakeSequence . toList . fmap (toZ "sequence" t) $ vs
      _ ->
        error $
          "TODO - don't know how to compile this term:\n"
            <> (CT.toPlain . P.render 80 . TP.pretty mempty $ void t)
      where
        compileVar _ v [] = unknown v
        compileVar i v ((v', o) : tl)
          | v == v' = case o of
            Nothing
              | isLazy v -> LazySlot i
              | otherwise -> Slot i
            Just v -> Val v
          | isJust o = compileVar i v tl
          | otherwise = compileVar (i + 1) v tl

        -- freeSlots :: _ -> Set Int
        freeSlots t =
          let vars = ABT.freeVars t
              env = ABT.annotation t
           in Set.fromList $
                toList vars >>= \v -> case compileVar 0 v env of
                  Slot i -> [i]
                  LazySlot i -> [i]
                  _ -> []

        ctorIR ::
          (Int -> R.Reference -> Int -> [Z e cont] -> IR e cont) ->
          (R.Reference -> Int -> Term SymbolC) ->
          R.Reference ->
          Int ->
          IR e cont
        ctorIR con src r cid = case constructorArity env r cid of
          Nothing ->
            error $
              "the compilation env is missing info about how "
                ++ "to compile this constructor: "
                ++ show (r, cid)
                ++ "\n"
                ++ show (constructorArity' env)
          Just 0 -> con 0 r cid []
          -- Just 0 -> Leaf . Val $ Data "Optional" 0
          Just arity -> Leaf . Val $ Lam arity (FormClosure (ABT.hash s) s []) ir
            where
              s = src r cid
              -- if `arity` is 1, then `Slot 0` is the sole argument.
              -- if `arity` is 2, then `Slot 1` is the first arg, and `Slot 0`
              -- get the second arg, etc.
              -- Note: [1..10] is inclusive of both `1` and `10`
              ir = con arity r cid (reverse $ map Slot [0 .. (arity - 1)])

        unknown v = error $ "free variable during compilation: " ++ show v
        toZ _msg t (Term.Var' v) = compileVar 0 v (ABT.annotation t)
        toZ msg _t e = case go e of
          Leaf v -> v
          e -> error $ msg ++ ": ANF should have eliminated any non-Z arguments from: " ++ show e
        compileCase (Term.MatchCase pat guard rhs@(ABT.unabs -> (vs, _))) =
          (compilePattern pat, underlyingSymbol <$> vs, go <$> guard, go rhs)

        getSeqLength :: Pattern.Pattern -> Maybe Int
        getSeqLength p = case p of
          Pattern.SequenceLiteral ps -> Just (length ps)
          Pattern.SequenceOp l op r -> case op of
            Pattern.Snoc -> (+ 1) <$> getSeqLength l
            Pattern.Cons -> (+ 1) <$> getSeqLength r
            Pattern.Concat -> (+) <$> getSeqLength l <*> getSeqLength r
          Pattern.As p -> getSeqLength p
          _ -> Nothing

        compilePattern :: Pattern.Pattern -> Pattern
        compilePattern pat = case pat of
          Pattern.Unbound -> PatternIgnore
          Pattern.Var -> PatternVar
          Pattern.Boolean b -> PatternB b
          Pattern.Int n -> PatternI n
          Pattern.Nat n -> PatternN n
          Pattern.Float n -> PatternF n
          Pattern.Text t -> PatternT t
          Pattern.Char c -> PatternC c
          Pattern.Constructor r cid args -> PatternData r cid (compilePattern <$> args)
          Pattern.As pat -> PatternAs (compilePattern pat)
          Pattern.EffectPure p -> PatternPure (compilePattern p)
          Pattern.EffectBind r cid args k -> PatternBind r cid (compilePattern <$> args) (compilePattern k)
          Pattern.SequenceLiteral ps -> PatternSequenceLiteral (compilePattern <$> ps)
          Pattern.SequenceOp l op r -> case op of
            Pattern.Snoc -> PatternSequenceSnoc (compilePattern l) (compilePattern r)
            Pattern.Cons -> PatternSequenceCons (compilePattern l) (compilePattern r)
            Pattern.Concat -> fromMaybe concatErr ((concat Left <$> getSeqLength l) <|> (concat Right <$> getSeqLength r))
              where
                concat :: (Int -> Either Int Int) -> Int -> Pattern
                concat f i = PatternSequenceConcat (f i) (compilePattern l) (compilePattern r)
                concatErr =
                  error $
                    "At least one side of a concat must have a constant length. "
                      <> "This code should never be reached as this constraint is "
                      <> "applied in the typechecker."
          _ -> error $ "todo - compilePattern " ++ show pat

type DS = StateT (Map Symbol (Term Symbol), Set RefID) IO

runDS :: DS (Term Symbol) -> IO (Term Symbol)
runDS ds = do
  (body, (letRecBindings, _)) <- runStateT ds mempty
  pure $
    if null letRecBindings
      then body
      else Term.letRec' False (Map.toList letRecBindings) body

decompile :: (External e, External cont) => Value e cont -> IO (Term Symbol)
decompile v = runDS (decompileImpl v)

decompileImpl ::
  (External e, External cont) => Value e cont -> DS (Term Symbol)
decompileImpl v = case v of
  I n -> pure $ Term.int () n
  N n -> pure $ Term.nat () n
  F n -> pure $ Term.float () n
  B b -> pure $ Term.boolean () b
  T t -> pure $ Term.text () t
  C c -> pure $ Term.char () c
  Bs bs -> pure $ Term.builtin () "Bytes.fromList" `Term.apps'` [bsv]
    where
      bsv =
        Term.seq' () . Sequence.fromList $
          [Term.nat () (fromIntegral w8) | w8 <- Bytes.toWord8s bs]
  Lam _ f _ -> decompileUnderapplied f
  Data r cid args ->
    Term.apps' <$> pure (Term.constructor () r cid)
      <*> traverse decompileImpl (toList args)
  Sequence vs -> Term.seq' () <$> traverse decompileImpl vs
  Ref id symbol ioref -> do
    seen <- gets snd
    symbol <- pure $ Var.freshenId (fromIntegral id) symbol
    if Set.member id seen
      then pure $ Term.var () symbol
      else do
        modify (second $ Set.insert id)
        t <- decompileImpl =<< lift (readIORef ioref)
        modify (first $ Map.insert symbol t)
        pure (Term.etaNormalForm t)
  Cont k -> liftIO $ decompileExternal k
  Pure a -> do
    -- `{a}` doesn't have a term syntax, so it's decompiled as
    -- `handle (x -> x) in a`, which has the type `Request ambient e a`
    a <- decompileImpl a
    pure $ Term.handle () id a
  Requested (Req r cid vs k) -> do
    -- `{req a b -> k}` doesn't have a term syntax, so it's decompiled as
    -- `handle (x -> x) in k (req a b)`
    vs <- traverse decompileImpl vs
    kt <- liftIO $ decompileExternal k
    pure . Term.handle () id $
      Term.apps' kt [Term.apps' (Term.request () r cid) vs]
  UninitializedLetRecSlot _b _bs _body ->
    error "unpossible - decompile UninitializedLetRecSlot"
  TermLink r -> pure $ Term.termLink () r
  TypeLink r -> pure $ Term.typeLink () r
  where
    idv = Var.named "x"
    id = Term.lam () idv (Term.var () idv)

boundVarsIR :: IR e cont -> Set Symbol
boundVarsIR = \case
  Let v b body _ -> Set.singleton v <> boundVarsIR b <> boundVarsIR body
  LetRec bs body -> Set.fromList (fst <$> bs) <> foldMap (boundVarsIR . snd) bs <> boundVarsIR body
  Apply lam _ -> boundVarsIR lam
  Handle _ body -> boundVarsIR body
  If _ t f -> foldMap boundVarsIR [t, f]
  And _ b -> boundVarsIR b
  Or _ b -> boundVarsIR b
  Match _ cases -> foldMap doCase cases
    where
      doCase (_, _, b, body) = maybe mempty boundVarsIR b <> boundVarsIR body
  -- I added all these cases for exhaustiveness checking in the future,
  -- and also because I needed the patterns for decompileIR anyway.
  -- Sure is ugly though.  This ghc doesn't support Language MultiCase.
  -- I want to be able to say `_ -> mempty` where _ refers to exactly the other
  -- cases that existed at the time I wrote it!
  Leaf _ -> mempty
  AddI _ _ -> mempty
  SubI _ _ -> mempty
  MultI _ _ -> mempty
  DivI _ _ -> mempty
  GtI _ _ -> mempty
  LtI _ _ -> mempty
  GtEqI _ _ -> mempty
  LtEqI _ _ -> mempty
  EqI _ _ -> mempty
  SignumI _ -> mempty
  NegateI _ -> mempty
  Truncate0I _ -> mempty
  ModI _ _ -> mempty
  PowI _ _ -> mempty
  ShiftRI _ _ -> mempty
  ShiftLI _ _ -> mempty
  BitAndI _ _ -> mempty
  BitOrI _ _ -> mempty
  BitXorI _ _ -> mempty
  ComplementI _ -> mempty
  TrailZeroI _ -> mempty
  LeadZeroI _ -> mempty
  AddN _ _ -> mempty
  DropN _ _ -> mempty
  SubN _ _ -> mempty
  MultN _ _ -> mempty
  DivN _ _ -> mempty
  GtN _ _ -> mempty
  LtN _ _ -> mempty
  GtEqN _ _ -> mempty
  LtEqN _ _ -> mempty
  EqN _ _ -> mempty
  ModN _ _ -> mempty
  PowN _ _ -> mempty
  ShiftLN _ _ -> mempty
  ShiftRN _ _ -> mempty
  ToIntN _ -> mempty
  BitAndN _ _ -> mempty
  BitOrN _ _ -> mempty
  BitXorN _ _ -> mempty
  ComplementN _ -> mempty
  LeadZeroN _ -> mempty
  TrailZeroN _ -> mempty
  AddF _ _ -> mempty
  SubF _ _ -> mempty
  MultF _ _ -> mempty
  DivF _ _ -> mempty
  GtF _ _ -> mempty
  LtF _ _ -> mempty
  GtEqF _ _ -> mempty
  LtEqF _ _ -> mempty
  EqF _ _ -> mempty
  EqU _ _ -> mempty
  CompareU _ _ -> mempty
  Bug _ -> mempty
  Todo _ -> mempty
  MakeSequence _ -> mempty
  Construct {} -> mempty
  Request {} -> mempty
  Not {} -> mempty

class External e where
  decompileExternal :: e -> IO (Term Symbol)

decompileIR ::
  (External e, External cont) => [Symbol] -> IR e cont -> DS (Term Symbol)
decompileIR stack = \case
  -- added all these cases for exhaustiveness checking in the future,
  -- and also because I needed the patterns for decompileIR anyway.
  Leaf z -> decompileZ z
  AddI x y -> builtin "Int.+" [x, y]
  SubI x y -> builtin "Int.-" [x, y]
  MultI x y -> builtin "Int.*" [x, y]
  DivI x y -> builtin "Int./" [x, y]
  GtI x y -> builtin "Int.>" [x, y]
  LtI x y -> builtin "Int.<" [x, y]
  GtEqI x y -> builtin "Int.>=" [x, y]
  LtEqI x y -> builtin "Int.<=" [x, y]
  EqI x y -> builtin "Int.==" [x, y]
  SignumI x -> builtin "Int.signum" [x]
  NegateI x -> builtin "Int.negate" [x]
  Truncate0I x -> builtin "Int.truncate0" [x]
  ModI x y -> builtin "Int.mod" [x, y]
  PowI x y -> builtin "Int.pow" [x, y]
  ShiftRI x y -> builtin "Int.shiftRight" [x, y]
  ShiftLI x y -> builtin "Int.shiftLeft" [x, y]
  BitAndI x y -> builtin "Int.and" [x, y]
  BitOrI x y -> builtin "Int.or" [x, y]
  BitXorI x y -> builtin "Int.xor" [x, y]
  ComplementI x -> builtin "Int.complement" [x]
  LeadZeroI x -> builtin "Int.leadingZeros" [x]
  TrailZeroI x -> builtin "Int.trailingZeros" [x]
  AddN x y -> builtin "Nat.+" [x, y]
  DropN x y -> builtin "Nat.drop" [x, y]
  SubN x y -> builtin "Nat.sub" [x, y]
  MultN x y -> builtin "Nat.*" [x, y]
  DivN x y -> builtin "Nat./" [x, y]
  GtN x y -> builtin "Nat.>" [x, y]
  LtN x y -> builtin "Nat.<" [x, y]
  GtEqN x y -> builtin "Nat.>=" [x, y]
  LtEqN x y -> builtin "Nat.<=" [x, y]
  EqN x y -> builtin "Nat.==" [x, y]
  ModN x y -> builtin "Nat.mod" [x, y]
  ToIntN x -> builtin "Nat.toInt" [x]
  PowN x y -> builtin "Nat.pow" [x, y]
  ShiftRN x y -> builtin "Nat.shiftRight" [x, y]
  ShiftLN x y -> builtin "Nat.shiftLeft" [x, y]
  BitAndN x y -> builtin "Nat.and" [x, y]
  BitOrN x y -> builtin "Nat.or" [x, y]
  BitXorN x y -> builtin "Nat.xor" [x, y]
  ComplementN x -> builtin "Nat.complement" [x]
  LeadZeroN x -> builtin "Nat.leadingZeros" [x]
  TrailZeroN x -> builtin "Nat.trailingZeros" [x]
  AddF x y -> builtin "Float.+" [x, y]
  SubF x y -> builtin "Float.-" [x, y]
  MultF x y -> builtin "Float.*" [x, y]
  DivF x y -> builtin "Float./" [x, y]
  GtF x y -> builtin "Float.>" [x, y]
  LtF x y -> builtin "Float.<" [x, y]
  GtEqF x y -> builtin "Float.>=" [x, y]
  LtEqF x y -> builtin "Float.<=" [x, y]
  EqF x y -> builtin "Float.==" [x, y]
  EqU x y -> builtin "Universal.==" [x, y]
  CompareU x y -> builtin "Universal.compare" [x, y]
  Bug x -> builtin "bug" [x]
  Todo x -> builtin "todo" [x]
  Let v b body _ -> do
    b' <- decompileIR stack b
    body' <- decompileIR (v : stack) body
    pure $ Term.let1_ False [(v, b')] body'
  LetRec bs body -> do
    let stack' = reverse (fmap fst bs) ++ stack
        secondM f (x, y) = (x,) <$> f y
    bs' <- traverse (secondM $ decompileIR stack') bs
    body' <- decompileIR stack' body
    pure $ Term.letRec' False bs' body'
  MakeSequence args ->
    Term.seq () <$> traverse decompileZ args
  Apply lam args ->
    Term.apps' <$> decompileIR stack lam <*> traverse decompileZ args
  Construct r cid args ->
    Term.apps' (Term.constructor () r cid) <$> traverse decompileZ args
  Request r cid args ->
    Term.apps' (Term.request () r cid) <$> traverse decompileZ args
  Handle h body ->
    Term.handle () <$> decompileZ h <*> decompileIR stack body
  If c t f ->
    Term.iff () <$> decompileZ c <*> decompileIR stack t <*> decompileIR stack f
  And x y ->
    Term.and () <$> decompileZ x <*> decompileIR stack y
  Or x y ->
    Term.or () <$> decompileZ x <*> decompileIR stack y
  Not x -> builtin "Boolean.not" [x]
  Match scrutinee cases ->
    Term.match () <$> decompileZ scrutinee <*> traverse decompileMatchCase cases
  where
    builtin :: (External e, External cont) => Text -> [Z e cont] -> DS (Term Symbol)
    builtin t args =
      Term.apps' (Term.ref () (R.Builtin t)) <$> traverse decompileZ args
    at :: Pos -> Term Symbol
    at i = Term.var () (stack !! i)
    decompileZ :: (External e, External cont) => Z e cont -> DS (Term Symbol)
    decompileZ = \case
      Slot p -> pure $ at p
      LazySlot p -> pure $ at p
      Val v -> decompileImpl v
      External e -> liftIO $ decompileExternal e
    decompilePattern :: Pattern -> Pattern.Pattern
    decompilePattern = \case
      PatternI i -> Pattern.Int i
      PatternN n -> Pattern.Nat n
      PatternF f -> Pattern.Float f
      PatternB b -> Pattern.Boolean b
      PatternT t -> Pattern.Text t
      PatternC c -> Pattern.Char c
      PatternData r cid pats ->
        Pattern.Constructor r cid (d <$> pats)
      PatternSequenceLiteral ps -> Pattern.SequenceLiteral $ decompilePattern <$> ps
      PatternSequenceCons l r -> Pattern.SequenceOp (decompilePattern l) Pattern.Cons (decompilePattern r)
      PatternSequenceSnoc l r -> Pattern.SequenceOp (decompilePattern l) Pattern.Snoc (decompilePattern r)
      PatternSequenceConcat _ l r -> Pattern.SequenceOp (decompilePattern l) Pattern.Concat (decompilePattern r)
      PatternPure pat -> Pattern.EffectPure (d pat)
      PatternBind r cid pats k ->
        Pattern.EffectBind r cid (d <$> pats) (d k)
      PatternAs pat -> Pattern.As (d pat)
      PatternIgnore -> Pattern.Unbound
      PatternVar -> Pattern.Var
    d = decompilePattern
    decompileMatchCase (pat, vars, guard, body) = do
      let stack' = reverse vars ++ stack
      guard' <- traverse (decompileIR stack') guard
      body' <- decompileIR stack' body
      pure $ Term.MatchCase (d pat) guard' body'

instance (Show e, Show cont) => Show (Z e cont) where
  show (LazySlot i) = "'#" ++ show i
  show (Slot i) = "#" ++ show i
  show (Val v) = show v
  show (External e) = "External:" <> show e

freeSlots :: IR e cont -> Set Int
freeSlots ir = case ir of
  Let _ _ _ free -> decrementFrees free
  LetRec bs body ->
    let n = length bs
     in foldMap (decrementFreesBy n . freeSlots . snd) bs
          <> decrementFreesBy n (freeSlots body)
  Apply lam args -> freeSlots lam <> foldMap free args
  Handle h body -> free h <> freeSlots body
  If c t f -> free c <> freeSlots t <> freeSlots f
  And x y -> free x <> freeSlots y
  Or x y -> free x <> freeSlots y
  Match scrutinee cases -> free scrutinee <> foldMap freeInCase cases
    where
      freeInCase (_pat, bound, guard, rhs) =
        let n = length bound
         in decrementFreesBy n (freeSlots rhs)
              <> maybe mempty (decrementFreesBy n . freeSlots) guard
  _ -> foldMap free (toList ir)
  where
    free z = case z of
      Slot i -> Set.singleton i
      LazySlot i -> Set.singleton i
      _ -> Set.empty

-- todo: could make this more efficient
decrementFreesBy :: Int -> Set Int -> Set Int
decrementFreesBy 0 s = s
decrementFreesBy n s = decrementFreesBy (n -1) (decrementFrees s)

decrementFrees :: Set Int -> Set Int
decrementFrees frees =
  Set.map (\x -> x - 1) (Set.delete 0 frees)

let' :: Symbol -> IR e cont -> IR e cont -> IR e cont
let' name binding body =
  Let name binding body (decrementFrees $ freeSlots body)

builtins :: Map R.Reference (IR e cont)
builtins = Map.fromList $ arity0 <> arityN
  where
    -- slot = Leaf . Slot
    val = Leaf . Val
    underapply name =
      let r = Term.ref () $ R.Builtin name :: Term SymbolC
       in FormClosure (ABT.hash r) r []
    var = Var.named "x"
    arity0 =
      [ (R.Builtin name, val value)
        | (name, value) <-
            [ ("Text.empty", T ""),
              ("Sequence.empty", Sequence mempty),
              ("Bytes.empty", Bs mempty)
            ]
      ]
    arityN =
      [ (R.Builtin name, Leaf . Val $ Lam arity (underapply name) ir)
        | (name, arity, ir) <-
            [ ("Int.+", 2, AddI (Slot 1) (Slot 0)),
              ("Int.-", 2, SubI (Slot 1) (Slot 0)),
              ("Int.*", 2, MultI (Slot 1) (Slot 0)),
              ("Int./", 2, DivI (Slot 1) (Slot 0)),
              ("Int.<", 2, LtI (Slot 1) (Slot 0)),
              ("Int.>", 2, GtI (Slot 1) (Slot 0)),
              ("Int.<=", 2, LtEqI (Slot 1) (Slot 0)),
              ("Int.>=", 2, GtEqI (Slot 1) (Slot 0)),
              ("Int.==", 2, EqI (Slot 1) (Slot 0)),
              ("Int.and", 2, BitAndI (Slot 1) (Slot 0)),
              ("Int.or", 2, BitOrI (Slot 1) (Slot 0)),
              ("Int.xor", 2, BitXorI (Slot 1) (Slot 0)),
              ("Int.complement", 1, ComplementI (Slot 0)),
              ("Int.increment", 1, AddI (Val (I 1)) (Slot 0)),
              ("Int.signum", 1, SignumI (Slot 0)),
              ("Int.negate", 1, NegateI (Slot 0)),
              ("Int.truncate0", 1, Truncate0I (Slot 0)),
              ("Int.mod", 2, ModI (Slot 1) (Slot 0)),
              ("Int.pow", 2, PowI (Slot 1) (Slot 0)),
              ("Int.shiftLeft", 2, ShiftLI (Slot 1) (Slot 0)),
              ("Int.shiftRight", 2, ShiftRI (Slot 1) (Slot 0)),
              ("Int.leadingZeros", 1, LeadZeroI (Slot 0)),
              ("Int.trailingZeros", 1, TrailZeroI (Slot 0)),
              ( "Int.isEven",
                1,
                let'
                  var
                  (ModI (Slot 0) (Val (I 2)))
                  (EqI (Val (I 0)) (Slot 0))
              ),
              ( "Int.isOdd",
                1,
                let'
                  var
                  (ModI (Slot 0) (Val (I 2)))
                  ( let'
                      var
                      (EqI (Val (I 0)) (Slot 0))
                      (Not (Slot 0))
                  )
              ),
              ("Nat.+", 2, AddN (Slot 1) (Slot 0)),
              ("Nat.drop", 2, DropN (Slot 1) (Slot 0)),
              ("Nat.sub", 2, SubN (Slot 1) (Slot 0)),
              ("Nat.*", 2, MultN (Slot 1) (Slot 0)),
              ("Nat./", 2, DivN (Slot 1) (Slot 0)),
              ("Nat.<", 2, LtN (Slot 1) (Slot 0)),
              ("Nat.>", 2, GtN (Slot 1) (Slot 0)),
              ("Nat.<=", 2, LtEqN (Slot 1) (Slot 0)),
              ("Nat.>=", 2, GtEqN (Slot 1) (Slot 0)),
              ("Nat.==", 2, EqN (Slot 1) (Slot 0)),
              ("Nat.and", 2, BitAndN (Slot 1) (Slot 0)),
              ("Nat.or", 2, BitOrN (Slot 1) (Slot 0)),
              ("Nat.xor", 2, BitXorN (Slot 1) (Slot 0)),
              ("Nat.complement", 1, ComplementN (Slot 0)),
              ("Nat.increment", 1, AddN (Val (N 1)) (Slot 0)),
              ("Nat.mod", 2, ModN (Slot 1) (Slot 0)),
              ("Nat.pow", 2, PowN (Slot 1) (Slot 0)),
              ("Nat.shiftLeft", 2, ShiftLN (Slot 1) (Slot 0)),
              ("Nat.shiftRight", 2, ShiftRN (Slot 1) (Slot 0)),
              ("Nat.leadingZeros", 1, LeadZeroN (Slot 0)),
              ("Nat.trailingZeros", 1, TrailZeroN (Slot 0)),
              ( "Nat.isEven",
                1,
                let'
                  var
                  (ModN (Slot 0) (Val (N 2)))
                  (EqN (Val (N 0)) (Slot 0))
              ),
              ( "Nat.isOdd",
                1,
                let'
                  var
                  (ModN (Slot 0) (Val (N 2)))
                  ( let'
                      var
                      (EqN (Val (N 0)) (Slot 0))
                      (Not (Slot 0))
                  )
              ),
              ("Nat.toInt", 1, ToIntN (Slot 0)),
              ("Float.+", 2, AddF (Slot 1) (Slot 0)),
              ("Float.-", 2, SubF (Slot 1) (Slot 0)),
              ("Float.*", 2, MultF (Slot 1) (Slot 0)),
              ("Float./", 2, DivF (Slot 1) (Slot 0)),
              ("Float.<", 2, LtF (Slot 1) (Slot 0)),
              ("Float.>", 2, GtF (Slot 1) (Slot 0)),
              ("Float.<=", 2, LtEqF (Slot 1) (Slot 0)),
              ("Float.>=", 2, GtEqF (Slot 1) (Slot 0)),
              ("Float.==", 2, EqF (Slot 1) (Slot 0)),
              ("Universal.==", 2, EqU (Slot 1) (Slot 0)),
              ("Universal.compare", 2, CompareU (Slot 1) (Slot 0)),
              ( "Universal.<",
                2,
                let'
                  var
                  (CompareU (Slot 1) (Slot 0))
                  (LtI (Slot 0) (Val (I 0)))
              ),
              ( "Universal.>",
                2,
                let'
                  var
                  (CompareU (Slot 1) (Slot 0))
                  (GtI (Slot 0) (Val (I 0)))
              ),
              ( "Universal.>=",
                2,
                let'
                  var
                  (CompareU (Slot 1) (Slot 0))
                  (GtEqI (Slot 0) (Val (I 0)))
              ),
              ( "Universal.<=",
                2,
                let'
                  var
                  (CompareU (Slot 1) (Slot 0))
                  (LtEqI (Slot 0) (Val (I 0)))
              ),
              ("Boolean.not", 1, Not (Slot 0)),
              ("bug", 1, Bug (Slot 0)),
              ("todo", 1, Todo (Slot 0))
            ]
      ]

-- boring instances

instance Eq SymbolC where
  SymbolC _ s == SymbolC _ s2 = s == s2

instance Ord SymbolC where
  SymbolC _ s `compare` SymbolC _ s2 = s `compare` s2

instance ABT.Var SymbolC where
  freshIn vs (SymbolC i s) =
    SymbolC i (ABT.freshIn (Set.map underlyingSymbol vs) s)

instance Var SymbolC where
  typed s = SymbolC False (Var.typed s)
  typeOf (SymbolC _ s) = Var.typeOf s
  freshId (SymbolC _ s) = Var.freshId s
  freshenId n (SymbolC i s) = SymbolC i (Var.freshenId n s)

instance (Show e, Show cont) => Show (Value e cont) where
  show (I n) = show n
  show (F n) = show n
  show (N n) = show n
  show (B b) = show b
  show (T t) = show t
  show (C c) = show c
  show (Bs bs) = show bs
  show (Lam n e ir) = "(Lam " <> show n <> " " <> show e <> " (" <> show ir <> "))"
  show (Data r cid vs) = "(Data " <> show r <> " " <> show cid <> " " <> show vs <> ")"
  show (Sequence vs) = "[" <> intercalateMap ", " show vs <> "]"
  show (Ref n s _) = "(Ref " <> show n <> " " <> show s <> ")"
  show (TermLink r) = "(TermLink " <> show r <> ")"
  show (TypeLink r) = "(TypeLink " <> show r <> ")"
  show (Pure v) = "(Pure " <> show v <> ")"
  show (Requested r) = "(Requested " <> show r <> ")"
  show (Cont ir) = "(Cont " <> show ir <> ")"
  show (UninitializedLetRecSlot b bs _body) =
    "(UninitializedLetRecSlot " <> show b <> " in " <> show (fst <$> bs) <> ")"

compilationEnv0 :: CompilationEnv e cont
compilationEnv0 = CompilationEnv builtins mempty

instance Semigroup (CompilationEnv e cont) where (<>) = mappend

instance Monoid (CompilationEnv e cont) where
  mempty = CompilationEnv mempty mempty
  mappend c1 c2 = CompilationEnv ir ctor
    where
      ir = toIR' c1 <> toIR' c2
      ctor = constructorArity' c1 <> constructorArity' c2

instance (CyclicEq e, CyclicEq cont) => CyclicEq (UnderapplyStrategy e cont) where
  cyclicEq h1 h2 (FormClosure hash1 _ vs1) (FormClosure hash2 _ vs2) =
    if hash1 == hash2
      then cyclicEq h1 h2 vs1 vs2
      else pure False
  cyclicEq h1 h2 (Specialize hash1 _ vs1) (Specialize hash2 _ vs2) =
    if hash1 == hash2
      then cyclicEq h1 h2 (snd <$> vs1) (snd <$> vs2)
      else pure False
  cyclicEq _ _ _ _ = pure False

instance (CyclicEq e, CyclicEq cont) => CyclicEq (Req e cont) where
  cyclicEq h1 h2 (Req r1 c1 vs1 k1) (Req r2 c2 vs2 k2) =
    if r1 == r2 && c1 == c2
      then do
        b <- cyclicEq h1 h2 vs1 vs2
        if b
          then cyclicEq h1 h2 k1 k2
          else pure False
      else pure False

instance (CyclicEq e, CyclicEq cont) => CyclicEq (Value e cont) where
  cyclicEq _ _ (I x) (I y) = pure (x == y)
  cyclicEq _ _ (F x) (F y) = pure (x == y)
  cyclicEq _ _ (N x) (N y) = pure (x == y)
  cyclicEq _ _ (B x) (B y) = pure (x == y)
  cyclicEq _ _ (T x) (T y) = pure (x == y)
  cyclicEq _ _ (C x) (C y) = pure (x == y)
  cyclicEq _ _ (Bs x) (Bs y) = pure (x == y)
  cyclicEq _ _ (TermLink x) (TermLink y) = pure (x == y)
  cyclicEq _ _ (TypeLink x) (TypeLink y) = pure (x == y)
  cyclicEq h1 h2 (Lam arity1 us _) (Lam arity2 us2 _) =
    if arity1 == arity2
      then cyclicEq h1 h2 us us2
      else pure False
  cyclicEq h1 h2 (Data r1 c1 vs1) (Data r2 c2 vs2) =
    if r1 == r2 && c1 == c2
      then cyclicEq h1 h2 vs1 vs2
      else pure False
  cyclicEq h1 h2 (Sequence v1) (Sequence v2) = cyclicEq h1 h2 v1 v2
  cyclicEq h1 h2 (Ref r1 _ io1) (Ref r2 _ io2) =
    if io1 == io2
      then pure True
      else do
        a <- CyT.lookup r1 h1
        b <- CyT.lookup r2 h2
        case (a, b) of
          -- We haven't encountered these refs before, descend into them and
          -- compare contents.
          (Nothing, Nothing) -> do
            CyT.insertEnd r1 h1
            CyT.insertEnd r2 h2
            r1 <- readIORef io1
            r2 <- readIORef io2
            cyclicEq h1 h2 r1 r2
          -- We've encountered these refs before, compare the positions where
          -- they were first encountered
          (Just r1, Just r2) -> pure (r1 == r2)
          _ -> pure False
  cyclicEq h1 h2 (Pure a) (Pure b) = cyclicEq h1 h2 a b
  cyclicEq h1 h2 (Requested r1) (Requested r2) = cyclicEq h1 h2 r1 r2
  cyclicEq h1 h2 (Cont k1) (Cont k2) = cyclicEq h1 h2 k1 k2
  cyclicEq _ _ _ _ = pure False

constructorId :: Value e cont -> Int
constructorId v = case v of
  I _ -> 0
  F _ -> 1
  N _ -> 2
  B _ -> 3
  T _ -> 4
  Bs _ -> 5
  Lam {} -> 6
  Data {} -> 7
  Sequence _ -> 8
  Pure _ -> 9
  Requested _ -> 10
  Ref {} -> 11
  Cont _ -> 12
  C _ -> 13
  UninitializedLetRecSlot {} -> 14
  TermLink _ -> 15
  TypeLink _ -> 16

instance (CyclicOrd e, CyclicOrd cont) => CyclicOrd (UnderapplyStrategy e cont) where
  cyclicOrd h1 h2 (FormClosure hash1 _ vs1) (FormClosure hash2 _ vs2) =
    COrd.bothOrd' h1 h2 hash1 hash2 vs1 vs2
  cyclicOrd h1 h2 (Specialize hash1 _ vs1) (Specialize hash2 _ vs2) =
    COrd.bothOrd' h1 h2 hash1 hash2 (map snd vs1) (map snd vs2)
  cyclicOrd _ _ FormClosure {} _ = pure LT
  cyclicOrd _ _ Specialize {} _ = pure GT

instance (CyclicOrd e, CyclicOrd cont) => CyclicOrd (Req e cont) where
  cyclicOrd h1 h2 (Req r1 c1 vs1 k1) (Req r2 c2 vs2 k2) = case compare r1 r2 of
    EQ -> do
      o <- COrd.bothOrd' h1 h2 c1 c2 vs1 vs2
      o <- case o of
        EQ -> cyclicOrd h1 h2 k1 k2
        _ -> pure o
      case o of
        EQ -> pure (r1 `compare` r2)
        _ -> pure o
    c -> pure c

instance (CyclicOrd e, CyclicOrd cont) => CyclicOrd (Value e cont) where
  cyclicOrd _ _ (I x) (I y) = pure (x `compare` y)
  cyclicOrd _ _ (F x) (F y) = pure (x `compare` y)
  cyclicOrd _ _ (N x) (N y) = pure (x `compare` y)
  cyclicOrd _ _ (B x) (B y) = pure (x `compare` y)
  cyclicOrd _ _ (T x) (T y) = pure (x `compare` y)
  cyclicOrd _ _ (C x) (C y) = pure (x `compare` y)
  cyclicOrd _ _ (Bs x) (Bs y) = pure (x `compare` y)
  cyclicOrd _ _ (TermLink x) (TermLink y) = pure (x `compare` y)
  cyclicOrd _ _ (TypeLink x) (TypeLink y) = pure (x `compare` y)
  cyclicOrd h1 h2 (Lam arity1 us _) (Lam arity2 us2 _) =
    COrd.bothOrd' h1 h2 arity1 arity2 us us2
  cyclicOrd h1 h2 (Data r1 c1 vs1) (Data r2 c2 vs2) =
    COrd.bothOrd' h1 h2 c1 c2 vs1 vs2 >>= \o -> case o of
      EQ -> pure (r1 `compare` r2)
      _ -> pure o
  cyclicOrd h1 h2 (Sequence v1) (Sequence v2) = cyclicOrd h1 h2 v1 v2
  cyclicOrd h1 h2 (Ref r1 _ io1) (Ref r2 _ io2) =
    if io1 == io2
      then pure EQ
      else do
        a <- CyT.lookup r1 h1
        b <- CyT.lookup r2 h2
        case (a, b) of
          -- We haven't encountered these refs before, descend into them and
          -- compare contents.
          (Nothing, Nothing) -> do
            CyT.insertEnd r1 h1
            CyT.insertEnd r2 h2
            r1 <- readIORef io1
            r2 <- readIORef io2
            cyclicOrd h1 h2 r1 r2
          -- We've encountered these refs before, compare the positions where
          -- they were first encountered
          (Just r1, Just r2) -> pure (r1 `compare` r2)
          _ -> pure $ a `compare` b
  cyclicOrd h1 h2 (Pure a) (Pure b) = cyclicOrd h1 h2 a b
  cyclicOrd h1 h2 (Requested r1) (Requested r2) = cyclicOrd h1 h2 r1 r2
  cyclicOrd h1 h2 (Cont k1) (Cont k2) = cyclicOrd h1 h2 k1 k2
  cyclicOrd _ _ v1 v2 = pure $ constructorId v1 `compare` constructorId v2
