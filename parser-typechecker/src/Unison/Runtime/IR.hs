{-# Language TupleSections #-}

module Unison.Runtime.IR where

import Debug.Trace
import Data.Foldable
import Data.Functor (void)
import Data.Maybe (isJust)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import Unison.Symbol (Symbol)
import Unison.Term (AnnotatedTerm)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Pattern as Pattern
import qualified Unison.Reference as R
import qualified Unison.Runtime.ANF as ANF
import qualified Unison.Term as Term

type Pos = Int
type Arity = Int
type ConstructorId = Int
type Term v = AnnotatedTerm v ()

-- Values, in normal form
data V
  = I Int64 | F Double | N Word64 | B Bool | T Text
  | Lam Arity (Either R.Reference (Term Symbol)) IR
  | Data R.Reference ConstructorId [V]
  | Sequence (Vector V)
  | Pure V
  | Requested Req
  | Cont IR
  deriving (Eq,Show)

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
data Z = Slot Pos | Val V deriving (Eq)

-- Computations - evaluation reduces these to values
data IR
  = Var Pos
  -- Ints
  | AddI Z Z | SubI Z Z | MultI Z Z | DivI Z Z
  | GtI Z Z | LtI Z Z | GtEqI Z Z | LtEqI Z Z | EqI Z Z
  -- Nats
  | AddN Z Z | DropN Z Z | SubN Z Z | MultN Z Z | DivN Z Z
  | GtN Z Z | LtN Z Z | GtEqN Z Z | LtEqN Z Z | EqN Z Z
  -- Floats
  | AddF Z Z | SubF Z Z | MultF Z Z | DivF Z Z
  | GtF Z Z | LtF Z Z | GtEqF Z Z | LtEqF Z Z | EqF Z Z
  -- Control flow
  | Let IR IR
  | LetRec [IR] IR
  | MakeSequence [Z]
  | V V
  | ApplyIR IR [Z]
  | ApplyZ Z [Z] -- call to unknown function
  | Construct R.Reference ConstructorId [Z]
  | Request R.Reference ConstructorId [Z]
  | Handle Z IR
  | If Z IR IR
  | And Z IR
  | Or Z IR
  | Not Z
  | Match Z [(Pattern, Maybe IR, IR)] -- pattern, optional guard, rhs
  -- | Watch Text (Term Symbol) IR
  deriving (Eq,Show)

-- Contains the effect ref and ctor id, the args, and the continuation
-- which expects the result at the top of the stack
data Req
  = Req R.Reference ConstructorId [V] IR
  deriving (Eq,Show)

-- Appends `k2` to the end of the `k` continuation
-- Ex: if `k` is `x -> x + 1` and `k2` is `y -> y + 4`,
-- this produces a continuation `x -> let r1 = x + 1; r1 + 4`.
appendCont :: Req -> IR -> Req
appendCont (Req r cid args k) k2 = Req r cid args (Let k k2)

-- Wrap a `handle h` around the continuation inside the `Req`.
-- Ex: `k = x -> x + 1` becomes `x -> handle h in x + 1`.
wrapHandler :: V -> Req -> Req
wrapHandler h (Req r cid args k) = Req r cid args (Handle (Val h) k)

compile :: (R.Reference -> IR) -> Term Symbol -> IR
compile env t = traceShowId $ compile0 env [] t

freeVars :: [(Symbol,a)] -> Term Symbol -> Set Symbol
freeVars bound t =
  ABT.freeVars t `Set.difference` Set.fromList (fst <$> bound)

-- Main compilation function - converts an arbitrary term to an `IR`.
-- Takes a way of resolving `Reference`s and an environment of variables,
-- some of which may already be precompiled to `V`s. (This occurs when
-- recompiling a function that is being partially applied)
compile0 :: (R.Reference -> IR) -> [(Symbol, Maybe V)] -> Term Symbol -> IR
compile0 env bound t = case freeVars bound t of
  fvs | Set.null fvs -> go ((++ bound) . fmap (,Nothing) <$> ABT.annotateBound' (ANF.fromTerm' t))
      | otherwise    -> error $ "can't compile a term with free variables: " ++ show (toList fvs)
  where
  go t = case t of
    Term.And' x y -> And (ind "and" t x) (go y)
    Term.LamsNamed' vs body ->
      V (Lam (length vs) (Right $ void t) (compile0 env (ABT.annotation body) (void body)))
    Term.Or' x y -> Or (ind "or" t x) (go y)
    Term.If' cond ifT ifF -> If (ind "cond" t cond) (go ifT) (go ifF)
    Term.Int' n -> V (I n)
    Term.Nat' n -> V (N n)
    Term.Float' n -> V (F n)
    Term.Boolean' b -> V (B b)
    Term.Text' t -> V (T t)
    Term.Ref' r -> env r
    Term.Var' v -> case compileVar 0 v (ABT.annotation t) of
      Slot i -> Var i
      Val v -> V v
    Term.Let1Named' _ b body -> Let (go b) (go body)
    Term.LetRecNamed' bs body -> LetRec (go . snd <$> bs) (go body)
    Term.Constructor' r cid -> V (Data r cid mempty)
    Term.Request' r cid -> Request r cid mempty
    Term.Apps' f args -> case f of
      Term.Ref' r -> ApplyIR (env r) (ind "apps-ref" t <$> args)
      Term.Request' r cid -> Request r cid (ind "apps-req" t <$> args)
      Term.Constructor' r cid -> Construct r cid (ind "apps-ctor" t <$> args)
      _ -> let msg = "apps-fn" ++ show args
           in ApplyZ (ind msg t f) (map (ind "apps-args" t) args) where
    Term.Handle' h body -> Handle (ind "handle" t h) (go body)
    Term.Ann' e _ -> go e
    Term.Match' scrutinee cases -> Match (ind "match" t scrutinee) (compileCase <$> cases)
    _ -> error $ "TODO - don't know how to compile " ++ show t
    where
      compileVar _ v [] = unknown v
      compileVar i v ((v',o):tl) =
        if v == v' then maybe (Slot i) Val o
        else if isJust o then compileVar i v tl
        else compileVar (i + 1) v tl
      unknown v = error $ "free variable during compilation: " ++ show v
      ind _msg t (Term.Var' v) = compileVar 0 v (ABT.annotation t)
      ind msg _t e = case go e of
        V v -> Val v
        _ -> error $ msg ++ " ANF should eliminate any non-var arguments here: " ++ show e
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

decompile :: V -> Maybe (Term Symbol)
decompile v = case v of
  I n -> pure $ Term.int () n
  N n -> pure $ Term.nat () n
  F n -> pure $ Term.float () n
  B b -> pure $ Term.boolean () b
  T t -> pure $ Term.text () t
  Lam _ f _ -> pure $ case f of Left r -> Term.ref() r; Right f -> f
  Data r cid args -> Term.apps' <$> pure (Term.constructor() r cid) <*> traverse decompile (toList args)
  Sequence vs -> Term.vector' () <$> (traverse decompile vs)
  Pure _ -> Nothing
  Requested _ -> Nothing
  Cont _ -> Nothing

instance Show Z where
  show (Slot i) = "#" ++ show i
  show (Val v) = show v

