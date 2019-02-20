{-# Language DeriveFoldable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PartialTypeSignatures #-}
{-# Language StrictData #-}
{-# Language TupleSections #-}

module Unison.Runtime.IR where

import Control.Applicative
import Data.Foldable
import Data.Functor (void)
import Data.IORef
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import Unison.Symbol (Symbol)
import Unison.Term (AnnotatedTerm)
import Unison.Util.Monoid (intercalateMap)
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Pattern as Pattern
import qualified Unison.Reference as R
import qualified Unison.Runtime.ANF as ANF
import qualified Unison.Term as Term
import qualified Unison.Var as Var

type Pos = Int
type Arity = Int
type ConstructorId = Int
type Term v = AnnotatedTerm v ()

data CompilationEnv e
  = CompilationEnv { toIR :: R.Reference -> Maybe (IR e)
                   , constructorArity :: R.Reference -> Int -> Maybe Int }

data SymbolC =
  SymbolC { isLazy :: Bool
          , underlyingSymbol :: Symbol
          } deriving Show

makeLazy :: SymbolC -> SymbolC
makeLazy s = s { isLazy = True }

toSymbolC :: Symbol -> SymbolC
toSymbolC s = SymbolC False s

-- Values, in normal form
data Value e
  = I Int64 | F Double | N Word64 | B Bool | T Text
  | Lam Arity UnderapplyStrategy (IR e)
  | Data R.Reference ConstructorId [Value e]
  | Sequence (Vector (Value e))
  | Ref Int Symbol (IORef (Value e))
  | Pure (Value e)
  | Requested (Req e)
  | Cont (IR e)
  | LetRecBomb Symbol [(Symbol, IR e)] (IR e)
  deriving (Eq)

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
data UnderapplyStrategy
  = FormClosure (Term SymbolC)
  | Specialize (Term SymbolC)
  deriving (Eq, Show)

-- Patterns - for now this follows Unison.Pattern exactly, but
-- we may switch to more efficient runtime representation of patterns
data Pattern
  = PatternI Int64 | PatternF Double | PatternN Word64 | PatternB Bool | PatternT Text
  | PatternData R.Reference ConstructorId [Pattern]
  | PatternSequence (Vector Pattern)
  | PatternPure Pattern
  | PatternBind R.Reference ConstructorId [Pattern] Pattern
  | PatternAs Pattern
  | PatternIgnore
  | PatternVar deriving (Eq,Show)

-- Leaf level instructions - these return immediately without using any stack
data Z e
  = Slot Pos
  | LazySlot Pos
  | Val (Value e)
  | External e
  deriving (Eq)

type IR e = IR' (Z e)

-- IR z
-- depth of a slot is just that slot
-- depth of a let is just depth
--
-- Computations - evaluation reduces these to values
data IR' z
  = Leaf z
  -- Ints
  | AddI z z | SubI z z | MultI z z | DivI z z
  | GtI z z | LtI z z | GtEqI z z | LtEqI z z | EqI z z
  -- Nats
  | AddN z z | DropN z z | SubN z z | MultN z z | DivN z z
  | GtN z z | LtN z z | GtEqN z z | LtEqN z z | EqN z z
  -- Floats
  | AddF z z | SubF z z | MultF z z | DivF z z
  | GtF z z | LtF z z | GtEqF z z | LtEqF z z | EqF z z
  -- Control flow
  | Let (IR' z) (IR' z)
  | LetRec [(Symbol, IR' z)] (IR' z)
  | MakeSequence [z]
  | Apply (IR' z) [z]
  | Construct R.Reference ConstructorId [z]
  | Request R.Reference ConstructorId [z]
  | Handle z (IR' z)
  | If z (IR' z) (IR' z)
  | And z (IR' z)
  | Or z (IR' z)
  | Not z
  -- pattern, optional guard, rhs
  | Match z [(Pattern, Maybe (IR' z), (IR' z))]
  deriving (Functor,Foldable,Traversable,Eq,Show)

-- Contains the effect ref and ctor id, the args, and the continuation
-- which expects the result at the top of the stack
data Req e
  = Req R.Reference ConstructorId [Value e] (IR e)
  deriving (Eq,Show)

-- Appends `k2` to the end of the `k` continuation
-- Ex: if `k` is `x -> x + 1` and `k2` is `y -> y + 4`,
-- this produces a continuation `x -> let r1 = x + 1; r1 + 4`.
appendCont :: Req e -> IR e -> Req e
appendCont (Req r cid args k) k2 = Req r cid args (Let k k2)

-- Wrap a `handle h` around the continuation inside the `Req`.
-- Ex: `k = x -> x + 1` becomes `x -> handle h in x + 1`.
wrapHandler :: Value e -> Req e -> Req e
wrapHandler h (Req r cid args k) = Req r cid args (Handle (Val h) k)

compile :: Show e => CompilationEnv e -> Term Symbol -> IR e
compile env t = compile0 env [] (Term.vmap toSymbolC t)

freeVars :: [(SymbolC,a)] -> Term SymbolC -> Set SymbolC
freeVars bound t =
  ABT.freeVars t `Set.difference` Set.fromList (fst <$> bound)

-- Main compilation function - converts an arbitrary term to an `IR`.
-- Takes a way of resolving `Reference`s and an environment of variables,
-- some of which may already be precompiled to `V`s. (This occurs when
-- recompiling a function that is being partially applied)
compile0 :: Show e => CompilationEnv e -> [(SymbolC, Maybe (Value e))] -> Term SymbolC -> IR e
compile0 env bound t =
  if Set.null fvs then
    go ((++ bound) . fmap (,Nothing) <$> ABT.annotateBound' (ANF.fromTerm' makeLazy t))
  else
    error $ "can't compile a term with free variables: " ++ show (toList fvs)
  where
  fvs = freeVars bound t
  go t = case t of
    Term.Nat' n -> Leaf . Val . N $ n
    Term.And' x y -> And (ind "and" t x) (go y)
    Term.LamsNamed' vs body -> Leaf . Val $
      Lam (length vs)
        (Specialize $ void t)
        (compile0 env (ABT.annotation body) (void body))
    Term.Or' x y -> Or (ind "or" t x) (go y)
    Term.Let1Named' _v b body -> Let (go b) (go body)
    Term.LetRecNamed' bs body ->
      LetRec ((\(v,b) -> (underlyingSymbol v, go b)) <$> bs) (go body)
    Term.Constructor' r cid -> ctorIR Construct (Term.constructor()) r cid
    Term.Request' r cid -> ctorIR Request (Term.request()) r cid
    Term.Apps' f args -> Apply (go f) (map (ind "apply-args" t) args)
    Term.Handle' h body -> Handle (ind "handle" t h) (go body)
    Term.Ann' e _ -> go e
    Term.Match' scrutinee cases -> Match (ind "match" t scrutinee) (compileCase <$> cases)
    Term.Var' _ -> Leaf $ ind "var" t t
    _ -> error $ "TODO - don't know how to compile " ++ show t
    where
      compileVar _ v [] = unknown v
      compileVar i v ((v',o):tl) =
        if v == v' then case o of
          Nothing | isLazy v  -> LazySlot i
                  | otherwise -> Slot i
          Just v -> Val v
        else if isJust o then compileVar i v tl
        else compileVar (i + 1) v tl

      ctorIR :: (R.Reference -> Int -> [Z e] -> IR e)
             -> (R.Reference -> Int -> Term SymbolC)
             -> R.Reference -> Int -> IR e
      ctorIR con src r cid = case constructorArity env r cid of
        Nothing -> error $ "the compilation env is missing info about how "
                        ++ "to compile this constructor: " ++ show (r, cid)
        Just arity -> Leaf . Val $ Lam arity (FormClosure $ src r cid) ir
          where
          -- if `arity` is 1, then `Slot 0` is the sole argument.
          -- if `arity` is 2, then `Slot 1` is the first arg, and `Slot 0`
          -- get the second arg, etc.
          -- Note: [1..10] is inclusive of both `1` and `10`
          ir = con r cid (reverse $ map Slot [0 .. (arity - 1)])

      unknown v = error $ "free variable during compilation: " ++ show v
      ind _msg t (Term.Var' v) = compileVar 0 v (ABT.annotation t)
      ind msg _t e = case go e of
        Leaf v -> v
        e -> error $ msg ++ " ANF should eliminate any non-var arguments here: " ++ show e
      compileCase (Term.MatchCase pat guard rhs) = (compilePattern pat, go <$> guard, go rhs)
      compilePattern pat = case pat of
        Pattern.Unbound -> PatternIgnore
        Pattern.Var -> PatternVar
        Pattern.Boolean b -> PatternB b
        Pattern.Int n -> PatternI n
        Pattern.Nat n -> PatternN n
        Pattern.Float n -> PatternF n
        Pattern.Constructor r cid args -> PatternData r cid (compilePattern <$> args)
        Pattern.As pat -> PatternAs (compilePattern pat)
        Pattern.EffectPure p -> PatternPure (compilePattern p)
        Pattern.EffectBind r cid args k -> PatternBind r cid (compilePattern <$> args) (compilePattern k)
        _ -> error $ "todo - compilePattern " ++ show pat

decompile :: Value e -> Maybe (Term SymbolC)
decompile v = case v of
  I n -> pure $ Term.int () n
  N n -> pure $ Term.nat () n
  F n -> pure $ Term.float () n
  B b -> pure $ Term.boolean () b
  T t -> pure $ Term.text () t
  Lam _ f _ -> pure $ case f of
    FormClosure f -> f
    Specialize f -> f
  Data r cid args -> Term.apps' <$> pure (Term.constructor() r cid) <*> traverse decompile (toList args)
  Sequence vs -> Term.vector' () <$> traverse decompile vs
  Pure _ -> Nothing
  Requested _ -> Nothing
  Cont _ -> Nothing
  Ref _ _ _ -> error "IR todo - decompile Ref"
  LetRecBomb _b _bs _body -> error "unpossible - decompile LetRecBomb"

instance Show e => Show (Z e) where
  show (LazySlot i) = "'#" ++ show i
  show (Slot i) = "#" ++ show i
  show (Val v) = show v
  show (External e) = "External:" <> show e

builtins :: Map R.Reference (IR e)
builtins = Map.fromList $ let
  -- slot = Leaf . Slot
  val = Leaf . Val
  underapply name = FormClosure (Term.ref() $ R.Builtin name)
  in [ (R.Builtin name, Leaf . Val $ Lam arity (underapply name) ir) |
       (name, arity, ir) <-
        [ ("Int.+", 2, AddI (Slot 1) (Slot 0))
        , ("Int.-", 2, SubI (Slot 1) (Slot 0))
        , ("Int.*", 2, MultI (Slot 1) (Slot 0))
        , ("Int./", 2, DivI (Slot 1) (Slot 0))
        , ("Int.<", 2, LtI (Slot 1) (Slot 0))
        , ("Int.>", 2, GtI (Slot 1) (Slot 0))
        , ("Int.<=", 2, LtEqI (Slot 1) (Slot 0))
        , ("Int.>=", 2, GtEqI (Slot 1) (Slot 0))
        , ("Int.==", 2, EqI (Slot 1) (Slot 0))
        , ("Int.increment", 1, AddI (Val (I 1)) (Slot 0))
        --, ("Int.is-even", "Int -> Boolean")
        --, ("Int.is-odd", "Int -> Boolean")
        --, ("Int.signum", "Int -> Int")
        --, ("Int.negate", "Int -> Int")

        , ("Nat.+", 2, AddN (Slot 1) (Slot 0))
        , ("Nat.drop", 2, DropN (Slot 1) (Slot 0))
        , ("Nat.sub", 2, SubN (Slot 1) (Slot 0))
        , ("Nat.*", 2, MultN (Slot 1) (Slot 0))
        , ("Nat./", 2, DivN (Slot 1) (Slot 0))
        , ("Nat.<", 2, LtN (Slot 1) (Slot 0))
        , ("Nat.>", 2, GtN (Slot 1) (Slot 0))
        , ("Nat.<=", 2, LtEqN (Slot 1) (Slot 0))
        , ("Nat.>=", 2, GtEqN (Slot 1) (Slot 0))
        , ("Nat.==", 2, EqN (Slot 1) (Slot 0))
        --, ("Nat.increment", "Nat -> Nat")
        --, ("Nat.is-even", "Nat -> Boolean")
        --, ("Nat.is-odd", "Nat -> Boolean")

        , ("Float.+", 2, AddF (Slot 1) (Slot 0))
        , ("Float.-", 2, SubF (Slot 1) (Slot 0))
        , ("Float.*", 2, MultF (Slot 1) (Slot 0))
        , ("Float./", 2, DivF (Slot 1) (Slot 0))
        , ("Float.<", 2, LtF (Slot 1) (Slot 0))
        , ("Float.>", 2, GtF (Slot 1) (Slot 0))
        , ("Float.<=", 2, LtEqF (Slot 1) (Slot 0))
        , ("Float.>=", 2, GtEqF (Slot 1) (Slot 0))
        , ("Float.==", 2, EqF (Slot 1) (Slot 0))

        , ("Boolean.not", 1, Not (Slot 0))

        , ("Text.empty", 0, val $ T "")
        -- , ("Text.++", "Text -> Text -> Text")

        --, ("Text.take", "Nat -> Text -> Text")
        --, ("Text.drop", "Nat -> Text -> Text")
        --, ("Text.size", "Text -> Nat")
        --, ("Text.==", "Text -> Text -> Boolean")
        --, ("Text.!=", "Text -> Text -> Boolean")
        --, ("Text.<=", "Text -> Text -> Boolean")
        --, ("Text.>=", "Text -> Text -> Boolean")
        --, ("Text.<", "Text -> Text -> Boolean")
        --, ("Text.>", "Text -> Text -> Boolean")

        --, ("Stream.empty", "Stream a")
        --, ("Stream.single", "a -> Stream a")
        --, ("Stream.constant", "a -> Stream a")
        --, ("Stream.from-int", "Int -> Stream Int")
        --, ("Stream.from-nat", "Nat -> Stream Nat")
        --, ("Stream.cons", "a -> Stream a -> Stream a")
        --, ("Stream.take", "Nat -> Stream a -> Stream a")
        --, ("Stream.drop", "Nat -> Stream a -> Stream a")
        --, ("Stream.take-while", "(a ->{} Boolean) -> Stream a -> Stream a")
        --, ("Stream.drop-while", "(a ->{} Boolean) -> Stream a -> Stream a")
        --, ("Stream.map", "(a ->{} b) -> Stream a -> Stream b")
        --, ("Stream.flat-map", "(a ->{} Stream b) -> Stream a -> Stream b")
        --, ("Stream.fold-left", "b -> (b ->{} a ->{} b) -> Stream a -> b")
        --, ("Stream.iterate", "a -> (a -> a) -> Stream a")
        --, ("Stream.reduce", "a -> (a ->{} a ->{} a) -> Stream a -> a")
        --, ("Stream.toSequence", "Stream a -> Sequence a")
        --, ("Stream.filter", "(a ->{} Boolean) -> Stream a -> Stream a")
        --, ("Stream.scan-left", "b -> (b ->{} a ->{} b) -> Stream a -> Stream b")
        --, ("Stream.sum-int", "Stream Int -> Int")
        --, ("Stream.sum-nat", "Stream Nat -> Nat")
        --, ("Stream.sum-float", "Stream Float -> Float")
        --, ("Stream.append", "Stream a -> Stream a -> Stream a")
        --, ("Stream.zip-with", "(a ->{} b ->{} c) -> Stream a -> Stream b -> Stream c")
        --, ("Stream.unfold", "(a ->{} Optional (b, a)) -> b -> Stream a")

        --, ("Sequence.empty", "[a]")
        --, ("Sequence.cons", "a -> [a] -> [a]")
        --, ("Sequence.snoc", "[a] -> a -> [a]")
        --, ("Sequence.take", "Nat -> [a] -> [a]")
        --, ("Sequence.drop", "Nat -> [a] -> [a]")
        --, ("Sequence.++", "[a] -> [a] -> [a]")
        --, ("Sequence.size", "[a] -> Nat")
        --, ("Sequence.at", "Nat -> [a] -> Optional a")

        -- , ("Debug.watch", "Text -> a -> a")
        ]
  ]

-- boring instances

instance Eq SymbolC where
  SymbolC _ s == SymbolC _ s2 = s == s2

instance Ord SymbolC where
  SymbolC _ s `compare` SymbolC _ s2 = s `compare` s2

instance Var SymbolC where
  named s = SymbolC False (Var.named s)
  rename t (SymbolC i s) = SymbolC i (Var.rename t s)
  name (SymbolC _ s) = Var.name s
  clear (SymbolC _ s) = SymbolC False (Var.clear s)
  qualifiedName (SymbolC _ s) = Var.qualifiedName s
  freshenId n (SymbolC i s) = SymbolC i (Var.freshenId n s)
  freshIn vs (SymbolC i s) =
    SymbolC i (Var.freshIn (Set.map underlyingSymbol vs) s)

instance Show e => Show (Value e) where
  show (I n) = show n
  show (F n) = show n
  show (N n) = show n
  show (B b) = show b
  show (T t) = show t
  show (Lam n e ir) = "(Lam " <> show n <> " " <> show e <> " " <> show ir <> ")"
  show (Data r cid vs) = "(Data " <> show r <> " " <> show cid <> " " <> show vs <> ")"
  show (Sequence vs) = "[" <> intercalateMap ", " show vs <> "]"
  show (Ref n s _) = "(Ref " <> show n <> " " <> show s <> ")"
  show (Pure v) = "(Pure " <> show v <> ")"
  show (Requested r) = "(Requested " <> show r <> ")"
  show (Cont ir) = "(Cont " <> show ir <> ")"
  show (LetRecBomb b bs _body) =
    "(LetRecBomb " <> show b <> " in " <> show (fst <$> bs)<> ")"

compilationEnv0 :: CompilationEnv e
compilationEnv0 = mempty { toIR = \r -> Map.lookup r builtins }

instance Semigroup (CompilationEnv e) where (<>) = mappend

instance Monoid (CompilationEnv e) where
  mempty = CompilationEnv (const Nothing) (\_ _ -> Nothing)
  mappend c1 c2 = CompilationEnv ir ctor where
    ir r = toIR c1 r <|> toIR c2 r
    ctor r cid = constructorArity c1 r cid <|> constructorArity c2 r cid
