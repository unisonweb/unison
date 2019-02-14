{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Unison.Term where

import Prelude hiding (and,or)
import qualified Control.Monad.Writer.Strict as Writer
import Data.Functor (void)
import           Data.Foldable (traverse_, toList)
import           Data.Int (Int64)
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set, union)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Word (Word64)
import           GHC.Generics
import           Prelude.Extras (Eq1(..), Show1(..))
import           Text.Show
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import qualified Unison.Hash as Hash
import           Unison.Hashable (Hashable1, accumulateToken)
import qualified Unison.Hashable as Hashable
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import           Unison.Reference (Reference, pattern Builtin)
import qualified Unison.Reference as Reference
import           Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import           Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.TypeVar as TypeVar
import qualified Unison.ConstructorType as CT
import Unison.TypeVar (TypeVar)
import           Unison.Var (Var)
import qualified Unison.Var as Var
import           Unsafe.Coerce
import Unison.Symbol (Symbol)

data MatchCase loc a = MatchCase (Pattern loc) (Maybe a) a
  deriving (Show,Eq,Foldable,Functor,Generic,Generic1,Traversable)

-- | Base functor for terms in the Unison language
-- We need `typeVar` because the term and type variables may differ.
data F typeVar typeAnn patternAnn a
  = Int Int64
  | Nat Word64
  | Float Double
  | Boolean Bool
  | Text Text
  | Blank (B.Blank typeAnn)
  | Ref Reference
  -- First argument identifies the data type,
  -- second argument identifies the constructor
  | Constructor Reference Int
  | Request Reference Int
  | Handle a a
  | App a a
  | Ann a (Type.AnnotatedType typeVar typeAnn)
  | Vector (Vector a)
  | If a a a
  | And a a
  | Or a a
  | Lam a
  -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
  -- variables as there are bindings
  | LetRec IsTop [a] a
  -- Note: first parameter is the binding, second is the expression which may refer
  -- to this let bound variable. Constructed as `Let b (abs v e)`
  | Let IsTop a a
  -- Pattern matching / eliminating data types, example:
  --  case x of
  --    Just n -> rhs1
  --    Nothing -> rhs2
  --
  -- translates to
  --
  --   Match x
  --     [ (Constructor 0 [Var], ABT.abs n rhs1)
  --     , (Constructor 1 [], rhs2) ]
  | Match a [MatchCase patternAnn a]
  deriving (Foldable,Functor,Generic,Generic1,Traversable)

type IsTop = Bool

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type AnnotatedTerm v a = AnnotatedTerm2 v a a v a
-- | Allow type variables and term variables to differ
type AnnotatedTerm' vt v a = AnnotatedTerm2 vt a a v a
-- | Allow type variables, term variables, type annotations and term annotations
-- to all differ
type AnnotatedTerm2 vt at ap v a = ABT.Term (F vt at ap) v a

-- | Terms are represented as ABTs over the base functor F, with variables in `v`
type Term v = AnnotatedTerm v ()
-- | Terms with type variables in `vt`, and term variables in `v`
type Term' vt v = AnnotatedTerm' vt v ()

bindBuiltins :: forall v a b b2. Var v
             => [(v, AnnotatedTerm2 v b a v b2)]
             -> [(v, Reference)]
             -> AnnotatedTerm2 v b a v a
             -> AnnotatedTerm2 v b a v a
bindBuiltins termBuiltins typeBuiltins t =
   f . g $ t
   where
   f :: AnnotatedTerm2 v b a v a -> AnnotatedTerm2 v b a v a
   f = typeMap (Type.bindBuiltins typeBuiltins)
   g :: AnnotatedTerm2 v b a v a -> AnnotatedTerm2 v b a v a
   g = ABT.substsInheritAnnotation termBuiltins

-- Prepare a term for type-directed name resolution by replacing
-- any remaining free variables with blanks to be resolved by TDNR
prepareTDNR :: Var v => ABT.Term (F vt b ap) v b -> ABT.Term (F vt b ap) v b
prepareTDNR t = fmap fst . ABT.visitPure f $ ABT.annotateBound t
  where f (ABT.Term _ (a, bound) (ABT.Var v)) | Set.notMember v bound =
          Just $ resolve (a, bound) a (Text.unpack $ Var.name v)
        f _ = Nothing

amap :: Ord v => (a -> a2) -> AnnotatedTerm v a -> AnnotatedTerm v a2
amap f = fmap f . patternMap (fmap f) . typeMap (fmap f)

patternMap :: (Pattern ap -> Pattern ap2) -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap2 v a
patternMap f e = go e where
  go (ABT.Term fvs a t) = ABT.Term fvs a $ case t of
    ABT.Abs v t -> ABT.Abs v (go t)
    ABT.Var v -> ABT.Var v
    ABT.Cycle t -> ABT.Cycle (go t)
    ABT.Tm (Match e cases) -> ABT.Tm (Match (go e) [
      MatchCase (f p) (go <$> g) (go a) | MatchCase p g a <- cases ])
    -- Safe since `Match` is only ctor that has embedded `Pattern ap` arg
    ABT.Tm ts -> unsafeCoerce $ ABT.Tm (fmap go ts)

vmap :: Ord v2 => (v -> v2) -> AnnotatedTerm v a -> AnnotatedTerm v2 a
vmap f = ABT.vmap f . typeMap (ABT.vmap f)

vtmap :: Ord vt2 => (vt -> vt2) -> AnnotatedTerm' vt v a -> AnnotatedTerm' vt2 v a
vtmap f = typeMap (ABT.vmap f)

typeMap :: Ord vt2 => (Type.AnnotatedType vt at -> Type.AnnotatedType vt2 at2)
                   -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt2 at2 ap v a
typeMap f t = go t where
  go (ABT.Term fvs a t) = ABT.Term fvs a $ case t of
    ABT.Abs v t -> ABT.Abs v (go t)
    ABT.Var v -> ABT.Var v
    ABT.Cycle t -> ABT.Cycle (go t)
    ABT.Tm (Ann e t) -> ABT.Tm (Ann (go e) (f t))
    -- Safe since `Ann` is only ctor that has embedded `Type v` arg
    -- otherwise we'd have to manually match on every non-`Ann` ctor
    ABT.Tm ts -> unsafeCoerce $ ABT.Tm (fmap go ts)

unTypeVar :: Ord v => AnnotatedTerm' (TypeVar b v) v a -> AnnotatedTerm v a
unTypeVar = typeMap (ABT.vmap TypeVar.underlying)

unannotate :: ∀ vt at ap v a . Ord v => AnnotatedTerm2 vt at ap v a -> Term' vt v
unannotate t = go t where
  go :: AnnotatedTerm2 vt at ap v a -> Term' vt v
  go (ABT.out -> ABT.Abs v body) = ABT.abs v (go body)
  go (ABT.out -> ABT.Cycle body) = ABT.cycle (go body)
  go (ABT.Var' v) = ABT.var v
  go (ABT.Tm' f) =
    case go <$> f of
      Ann e t -> ABT.tm (Ann e (void t))
      Match scrutinee branches ->
        let unann (MatchCase pat guard body) = MatchCase (void pat) guard body
        in ABT.tm (Match scrutinee (unann <$> branches))
      f' -> ABT.tm (unsafeCoerce f')
  go _ = error "unpossible"

wrapV :: Ord v => AnnotatedTerm v a -> AnnotatedTerm (ABT.V v) a
wrapV = vmap ABT.Bound

freeVars :: AnnotatedTerm' vt v a -> Set v
freeVars = ABT.freeVars

freeTypeVars :: Ord vt => AnnotatedTerm' vt v a -> Set vt
freeTypeVars t = go t where
  go :: Ord vt => AnnotatedTerm' vt v a -> Set vt
  go (ABT.Term _ _ t) = case t of
    ABT.Abs _ t -> go t
    ABT.Var _ -> Set.empty
    ABT.Cycle t -> go t
    ABT.Tm (Ann e t) -> Type.freeVars t `union` go e
    ABT.Tm ts -> foldMap go ts

-- nicer pattern syntax

pattern Var' v <- ABT.Var' v
pattern Cycle' xs t <- ABT.Cycle' xs t
pattern Abs' subst <- ABT.Abs' subst
pattern Int' n <- (ABT.out -> ABT.Tm (Int n))
pattern Nat' n <- (ABT.out -> ABT.Tm (Nat n))
pattern Float' n <- (ABT.out -> ABT.Tm (Float n))
pattern Boolean' b <- (ABT.out -> ABT.Tm (Boolean b))
pattern Text' s <- (ABT.out -> ABT.Tm (Text s))
pattern Blank' b <- (ABT.out -> ABT.Tm (Blank b))
pattern Ref' r <- (ABT.out -> ABT.Tm (Ref r))
pattern Builtin' r <- (ABT.out -> ABT.Tm (Ref (Builtin r)))
pattern App' f x <- (ABT.out -> ABT.Tm (App f x))
pattern Match' scrutinee branches <- (ABT.out -> ABT.Tm (Match scrutinee branches))
pattern Constructor' ref n <- (ABT.out -> ABT.Tm (Constructor ref n))
pattern Request' ref n <- (ABT.out -> ABT.Tm (Request ref n))
pattern RequestOrCtor' ref n <- (unReqOrCtor -> Just (ref, n))
pattern If' cond t f <- (ABT.out -> ABT.Tm (If cond t f))
pattern And' x y <- (ABT.out -> ABT.Tm (And x y))
pattern Or' x y <- (ABT.out -> ABT.Tm (Or x y))
pattern Handle' h body <- (ABT.out -> ABT.Tm (Handle h body))
pattern Apps' f args <- (unApps -> Just (f, args))
pattern AppsPred' f args <- (unAppsPred -> Just (f, args))
pattern BinaryApp' f arg1 arg2 <- (unBinaryApp -> Just (f, arg1, arg2))
pattern BinaryApps' apps lastArg <- (unBinaryApps -> Just (apps, lastArg))
pattern BinaryAppsPred' apps lastArg <- (unBinaryAppsPred -> Just (apps, lastArg))
pattern Ann' x t <- (ABT.out -> ABT.Tm (Ann x t))
pattern Vector' xs <- (ABT.out -> ABT.Tm (Vector xs))
pattern Tuple' xs <- (unTuple' -> Just xs)
pattern Lam' subst <- ABT.Tm' (Lam (ABT.Abs' subst))
pattern LamNamed' v body <- (ABT.out -> ABT.Tm (Lam (ABT.Term _ _ (ABT.Abs v body))))
pattern LamsNamed' vs body <- (unLams' -> Just (vs, body))
pattern LamsNamedOpt' vs body <- (unLamsOpt' -> Just (vs, body))
pattern LamsNamedPred' vs body <- (unLamsPred' -> Just (vs, body))
pattern Let1' b subst <- (unLet1 -> Just (_, b, subst))
pattern Let1Top' top b subst <- (unLet1 -> Just (top, b, subst))
pattern Let1Named' v b e <- (ABT.Tm' (Let _ b (ABT.out -> ABT.Abs v e)))
pattern Let1NamedTop' top v b e <- (ABT.Tm' (Let top b (ABT.out -> ABT.Abs v e)))
pattern Lets' bs e <- (unLet -> Just (bs, e))
pattern LetRecNamed' bs e <- (unLetRecNamed -> Just (_,bs,e))
pattern LetRec' subst <- (unLetRec -> Just (_, subst))
pattern LetRecTop' top subst <- (unLetRec -> Just (top, subst))
pattern LetRecNamedAnnotated' ann bs e <- (unLetRecNamedAnnotated -> Just (_, ann, bs,e))
pattern LetRecNamedAnnotatedTop' top ann bs e <-
          (unLetRecNamedAnnotated -> Just (top, ann, bs,e))
pattern AskInfo' arg <- (unAskInfo -> Just arg)

fresh :: Var v => Term v -> v -> v
fresh = ABT.fresh

-- some smart constructors

var :: a -> v -> AnnotatedTerm2 vt at ap v a
var = ABT.annotatedVar

var' :: Var v => Text -> Term' vt v
var' = var() . ABT.v'

ref :: Ord v => a -> Reference -> AnnotatedTerm2 vt at ap v a
ref a r = ABT.tm' a (Ref r)

builtin :: Ord v => a -> Text -> AnnotatedTerm2 vt at ap v a
builtin a n = ref a (Reference.Builtin n)

float :: Ord v => a -> Double -> AnnotatedTerm2 vt at ap v a
float a d = ABT.tm' a (Float d)

boolean :: Ord v => a -> Bool -> AnnotatedTerm2 vt at ap v a
boolean a b = ABT.tm' a (Boolean b)

int :: Ord v => a -> Int64 -> AnnotatedTerm2 vt at ap v a
int a d = ABT.tm' a (Int d)

nat :: Ord v => a -> Word64 -> AnnotatedTerm2 vt at ap v a
nat a d = ABT.tm' a (Nat d)

text :: Ord v => a -> Text -> AnnotatedTerm2 vt at ap v a
text a = ABT.tm' a . Text

unit :: Var v => a -> AnnotatedTerm v a
unit ann = constructor ann Type.unitRef 0

tupleCons :: (Ord v, Semigroup a)
          => AnnotatedTerm2 vt at ap v a
          -> AnnotatedTerm2 vt at ap v a
          -> AnnotatedTerm2 vt at ap v a
tupleCons hd tl =
  apps' (constructor (ABT.annotation hd) Type.pairRef 0) [hd, tl]

-- delayed terms are just lambdas that take a single `()` arg
-- `force` calls the function
force :: Var v => a -> a -> AnnotatedTerm v a -> AnnotatedTerm v a
force a au e = app a e (unit au)

delay :: Var v => a -> AnnotatedTerm v a -> AnnotatedTerm v a
delay a e = lam a (Var.named "()") e

watch :: (Var v, Semigroup a) => a -> String -> AnnotatedTerm v a -> AnnotatedTerm v a
watch a note e =
  apps' (builtin a "Debug.watch") [text a (Text.pack note), e]

watchMaybe :: (Var v, Semigroup a) => Maybe String -> AnnotatedTerm v a -> AnnotatedTerm v a
watchMaybe Nothing     e = e
watchMaybe (Just note) e = watch (ABT.annotation e) note e

blank :: Ord v => a -> AnnotatedTerm2 vt at ap v a
blank a = ABT.tm' a (Blank B.Blank)

placeholder :: Ord v => a -> String -> AnnotatedTerm2 vt a ap v a
placeholder a s = ABT.tm' a . Blank $ B.Recorded (B.Placeholder a s)

resolve :: Ord v => at -> ab -> String -> AnnotatedTerm2 vt ab ap v at
resolve at ab s = ABT.tm' at . Blank $ B.Recorded (B.Resolve ab s)

constructor :: Ord v => a -> Reference -> Int -> AnnotatedTerm2 vt at ap v a
constructor a ref n = ABT.tm' a (Constructor ref n)

request :: Ord v => a -> Reference -> Int -> AnnotatedTerm2 vt at ap v a
request a ref n = ABT.tm' a (Request ref n)

-- todo: delete and rename app' to app
app_ :: Ord v => Term' vt v -> Term' vt v -> Term' vt v
app_ f arg = ABT.tm (App f arg)

app :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
app a f arg = ABT.tm' a (App f arg)

match :: Ord v => a -> AnnotatedTerm2 vt at a v a -> [MatchCase a (AnnotatedTerm2 vt at a v a)] -> AnnotatedTerm2 vt at a v a
match a scrutinee branches = ABT.tm' a (Match scrutinee branches)

handle :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
handle a h block = ABT.tm' a (Handle h block)

and :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
and a x y = ABT.tm' a (And x y)

or :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
or a x y = ABT.tm' a (Or x y)

vector :: Ord v => a -> [AnnotatedTerm2 vt at ap v a] -> AnnotatedTerm2 vt at ap v a
vector a es = vector' a (Vector.fromList es)

vector' :: Ord v => a -> Vector (AnnotatedTerm2 vt at ap v a) -> AnnotatedTerm2 vt at ap v a
vector' a es = ABT.tm' a (Vector es)

apps :: Ord v => AnnotatedTerm2 vt at ap v a -> [(a, AnnotatedTerm2 vt at ap v a)] -> AnnotatedTerm2 vt at ap v a
apps f = foldl' (\f (a,t) -> app a f t) f

apps' :: (Ord v, Semigroup a)
      => AnnotatedTerm2 vt at ap v a -> [AnnotatedTerm2 vt at ap v a] -> AnnotatedTerm2 vt at ap v a
apps' f = foldl' (\f t -> app (ABT.annotation f <> ABT.annotation t) f t) f

iff :: Ord v => a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
iff a cond t f = ABT.tm' a (If cond t f)

ann_ :: Ord v => Term' vt v -> Type vt -> Term' vt v
ann_ e t = ABT.tm (Ann e t)

ann :: Ord v
    => a
    -> AnnotatedTerm2 vt at ap v a
    -> Type.AnnotatedType vt at
    -> AnnotatedTerm2 vt at ap v a
ann a e t = ABT.tm' a (Ann e t)

-- arya: are we sure we want the two annotations to be the same?
lam :: Ord v => a -> v -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
lam a v body = ABT.tm' a (Lam (ABT.abs' a v body))

lam' :: Ord v => a -> [v] -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
lam' a vs body = foldr (lam a) body vs

lam'' :: Ord v => [(a,v)] -> AnnotatedTerm2 vt at ap v a -> AnnotatedTerm2 vt at ap v a
lam'' vs body = foldr (uncurry lam) body vs

arity :: AnnotatedTerm2 vt at ap v a -> Int
arity (LamNamed' _ body) = 1 + arity body
arity (Ann' e _) = arity e
arity _ = 0

unLetRecNamedAnnotated
  :: AnnotatedTerm' vt v a
  -> Maybe
       (IsTop, a, [((a, v), AnnotatedTerm' vt v a)], AnnotatedTerm' vt v a)
unLetRecNamedAnnotated (ABT.CycleA' ann avs (ABT.Tm' (LetRec isTop bs e))) =
  Just (isTop, ann, avs `zip` bs, e)
unLetRecNamedAnnotated _ = Nothing

letRec
  :: Ord v
  => Bool
  -> a
  -> [((a, v), AnnotatedTerm' vt v a)]
  -> AnnotatedTerm' vt v a
  -> AnnotatedTerm' vt v a
letRec _ _ []       e     = e
letRec isTop a bindings e = ABT.cycle'
  a
  (foldr (uncurry ABT.abs') z (map fst bindings))
  where z = ABT.tm' a (LetRec isTop (map snd bindings) e)


-- | Smart constructor for let rec blocks. Each binding in the block may
-- reference any other binding in the block in its body (including itself),
-- and the output expression may also reference any binding in the block.
letRec_ :: Ord v => IsTop -> [(v, Term' vt v)] -> Term' vt v -> Term' vt v
letRec_ _ [] e = e
letRec_ isTop bindings e = ABT.cycle (foldr ABT.abs z (map fst bindings))
  where
    z = ABT.tm (LetRec isTop (map snd bindings) e)

-- | Smart constructor for let blocks. Each binding in the block may
-- reference only previous bindings in the block, not including itself.
-- The output expression may reference any binding in the block.
-- todo: delete me
let1_ :: Ord v => IsTop -> [(v,Term' vt v)] -> Term' vt v -> Term' vt v
let1_ isTop bindings e = foldr f e bindings
  where
    f (v,b) body = ABT.tm (Let isTop b (ABT.abs v body))

-- | annotations are applied to each nested Let expression
let1
  :: Ord v
  => IsTop
  -> [((a, v), AnnotatedTerm2 vt at ap v a)]
  -> AnnotatedTerm2 vt at ap v a
  -> AnnotatedTerm2 vt at ap v a
let1 isTop bindings e = foldr f e bindings
  where f ((ann, v), b) body = ABT.tm' ann (Let isTop b (ABT.abs' ann v body))

let1'
  :: (Semigroup a, Ord v)
  => IsTop
  -> [(v, AnnotatedTerm2 vt at ap v a)]
  -> AnnotatedTerm2 vt at ap v a
  -> AnnotatedTerm2 vt at ap v a
let1' isTop bindings e = foldr f e bindings
 where
  ann = ABT.annotation
  f (v, b) body = ABT.tm' a (Let isTop b (ABT.abs' a v body))
    where a = ann b <> ann body

-- let1' :: Var v => [(Text, Term' vt v)] -> Term' vt v -> Term' vt v
-- let1' bs e = let1 [(ABT.v' name, b) | (name,b) <- bs ] e

unLet1
  :: Var v
  => AnnotatedTerm' vt v a
  -> Maybe (IsTop, AnnotatedTerm' vt v a, ABT.Subst (F vt a a) v a)
unLet1 (ABT.Tm' (Let isTop b (ABT.Abs' subst))) = Just (isTop, b, subst)
unLet1 _ = Nothing

-- | Satisfies `unLet (let' bs e) == Just (bs, e)`
unLet
  :: AnnotatedTerm' vt v a
  -> Maybe ([(IsTop, v, AnnotatedTerm' vt v a)], AnnotatedTerm' vt v a)
unLet t = fixup (go t)
 where
  go (ABT.Tm' (Let isTop b (ABT.out -> ABT.Abs v t))) = case go t of
    (env, t) -> ((isTop, v, b) : env, t)
  go t = ([], t)
  fixup ([], _) = Nothing
  fixup bst     = Just bst

-- | Satisfies `unLetRec (letRec bs e) == Just (bs, e)`
unLetRecNamed
  :: AnnotatedTerm2 vt at ap v a
  -> Maybe
       ( IsTop
       , [(v, AnnotatedTerm2 vt at ap v a)]
       , AnnotatedTerm2 vt at ap v a
       )
unLetRecNamed (ABT.Cycle' vs (ABT.Tm' (LetRec isTop bs e)))
  | length vs == length vs = Just (isTop, zip vs bs, e)
unLetRecNamed _ = Nothing

unLetRec
  :: (Monad m, Var v)
  => AnnotatedTerm2 vt at ap v a
  -> Maybe
       (  IsTop
       ,  (v -> m v)
       -> m
            ( [(v, AnnotatedTerm2 vt at ap v a)]
            , AnnotatedTerm2 vt at ap v a
            )
       )
unLetRec (unLetRecNamed -> Just (isTop, bs, e)) =
  Just
    $ ( isTop
      , \freshen -> do
        vs <- sequence [ freshen v | (v, _) <- bs ]
        let sub = ABT.substsInheritAnnotation (map fst bs `zip` map ABT.var vs)
        pure (vs `zip` [ sub b | (_, b) <- bs ], sub e)
      )
unLetRec _ = Nothing

unApps :: AnnotatedTerm2 vt at ap v a -> Maybe (AnnotatedTerm2 vt at ap v a, [AnnotatedTerm2 vt at ap v a])
unApps t = unAppsPred (t, \_ -> True)

-- Same as unApps but taking a predicate controlling whether we match on a given function argument.
unAppsPred :: (AnnotatedTerm2 vt at ap v a, AnnotatedTerm2 vt at ap v a -> Bool) ->
                Maybe (AnnotatedTerm2 vt at ap v a, [AnnotatedTerm2 vt at ap v a])
unAppsPred (t, pred) = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (App' i o) acc | pred o = go i (o:acc)
  go _ [] = []
  go fn args = fn:args

unBinaryApp :: AnnotatedTerm2 vt at ap v a -> Maybe (AnnotatedTerm2 vt at ap v a,
                                                     AnnotatedTerm2 vt at ap v a,
                                                     AnnotatedTerm2 vt at ap v a)
unBinaryApp t = case unApps t of
  Just (f, [arg1, arg2]) -> Just (f, arg1, arg2)
  _                      -> Nothing

-- "((a1 `f1` a2) `f2` a3)" becomes "Just ([(a2, f2), (a1, f1)], a3)"
unBinaryApps :: AnnotatedTerm2 vt at ap v a -> Maybe ([(AnnotatedTerm2 vt at ap v a,
                                                        AnnotatedTerm2 vt at ap v a)],
                                                      AnnotatedTerm2 vt at ap v a)
unBinaryApps t = unBinaryAppsPred (t, \_ -> True)

-- Same as unBinaryApps but taking a predicate controlling whether we match on a given binary function.
unBinaryAppsPred :: (AnnotatedTerm2 vt at ap v a, AnnotatedTerm2 vt at ap v a -> Bool) ->
                      Maybe ([(AnnotatedTerm2 vt at ap v a,
                               AnnotatedTerm2 vt at ap v a)],
                              AnnotatedTerm2 vt at ap v a)
unBinaryAppsPred (t, pred) = case unBinaryApp t of
  Just (f, x, y) | pred f -> case unBinaryAppsPred (x, pred) of
                               Just (as, xLast) -> Just ((xLast, f) : as, y)
                               Nothing          -> Just ([(x, f)], y)
  _                       -> Nothing

unLams' :: AnnotatedTerm2 vt at ap v a -> Maybe ([v], AnnotatedTerm2 vt at ap v a)
unLams' t = unLamsPred' (t, (\_ -> True))

-- Same as unLams', but always matches.  Returns an empty [v] if the term doesn't start with a
-- lambda extraction.
unLamsOpt' :: AnnotatedTerm2 vt at ap v a -> Maybe ([v], AnnotatedTerm2 vt at ap v a)
unLamsOpt' t = case unLams' t of
  r@(Just _) -> r
  Nothing    -> Just ([], t)

-- Same as unLams' but taking a predicate controlling whether we match on a given binary function.
unLamsPred' :: (AnnotatedTerm2 vt at ap v a, v -> Bool) ->
                 Maybe ([v], AnnotatedTerm2 vt at ap v a)
unLamsPred' ((LamNamed' v body), pred) | pred v = case unLamsPred' (body, pred) of
  Nothing -> Just ([v], body)
  Just (vs, body) -> Just (v:vs, body)
unLamsPred' _ = Nothing

unTuple' :: AnnotatedTerm2 vt at ap v a -> Maybe [AnnotatedTerm2 vt at ap v a]
unTuple' t = case t of
  Apps' (Constructor' Type.PairRef 0) [fst, snd] -> (fst :) <$> unTuple' snd
  Constructor' Type.UnitRef 0 -> Just []
  _ -> Nothing

unReqOrCtor :: AnnotatedTerm2 vt at ap v a -> Maybe (Reference, Int)
unReqOrCtor (Constructor' r cid) = Just (r, cid)
unReqOrCtor (Request' r cid)     = Just (r, cid)
unReqOrCtor _                         = Nothing

unAskInfo :: Var v => AnnotatedTerm' vt v a -> Maybe (AnnotatedTerm' vt v a)
unAskInfo tm = case tm of
  App' t arg | isVarKindInfo t -> Just arg
  _ -> Nothing

isVarKindInfo :: Var v => AnnotatedTerm' vt v a -> Bool
isVarKindInfo t = case t of
  Var' v | (Var.kind v) == "info" -> True
  _ -> False

-- Dependencies including referenced data and effect decls
dependencies :: (Ord v, Ord vt) => AnnotatedTerm2 vt at ap v a -> Set Reference
dependencies t =
  dependencies' t
    <> referencedDataDeclarations t
    <> referencedEffectDeclarations t

-- Term and type dependencies, not including references to user-defined types
dependencies' :: (Ord v, Ord vt) => AnnotatedTerm2 vt at ap v a -> Set Reference
dependencies' t = Set.fromList . Writer.execWriter $ ABT.visit' f t
 where
  f t@(Ref r    ) = Writer.tell [r] *> pure t
  f t@(Ann _ typ) = Writer.tell (Set.toList (Type.dependencies typ)) *> pure t
  f t@(Nat _)     = Writer.tell [Type.natRef] *> pure t
  f t@(Int _)     = Writer.tell [Type.intRef] *> pure t
  f t@(Float _)   = Writer.tell [Type.floatRef] *> pure t
  f t@(Boolean _) = Writer.tell [Type.booleanRef] *> pure t
  f t@(Text _)    = Writer.tell [Type.textRef] *> pure t
  f t@(Vector _)  = Writer.tell [Type.vectorRef] *> pure t
  f t             = pure t

referencedDataDeclarations
  :: Ord v => AnnotatedTerm2 vt at ap v a -> Set Reference
referencedDataDeclarations t = Set.fromList . Writer.execWriter $ ABT.visit'
  f
  t
 where
  f t@(Constructor r _    ) = Writer.tell [r] *> pure t
  f t@(Match       _ cases) = traverse_ g cases *> pure t
   where
    g (MatchCase pat _ _) =
      Writer.tell (Set.toList (referencedDataDeclarationsP pat))
  f t = pure t

referencedDataDeclarationsP :: Pattern loc -> Set Reference
referencedDataDeclarationsP p = Set.fromList . Writer.execWriter $ go p
 where
  go (Pattern.As _ p) = go p
  go (Pattern.Constructor _ id _ args) = Writer.tell [id] *> traverse_ go args
  go (Pattern.EffectPure _ p) = go p
  go (Pattern.EffectBind _ _ _ args k) = traverse_ go args *> go k
  go _ = pure ()

referencedEffectDeclarations
  :: Ord v => AnnotatedTerm2 vt at ap v a -> Set Reference
referencedEffectDeclarations t = Set.fromList . Writer.execWriter $ ABT.visit'
  f
  t
 where
  f t@(Request r _    ) = Writer.tell [r] *> pure t
  f t@(Match   _ cases) = traverse_ g cases *> pure t
   where
    g (MatchCase pat _ _) =
      Writer.tell (Set.toList (referencedEffectDeclarationsP pat))
    -- todo: does this traverse the guard and body of MatchCase?
  f t = pure t

referencedEffectDeclarationsP :: Pattern loc -> Set Reference
referencedEffectDeclarationsP p = Set.fromList . Writer.execWriter $ go p
 where
  go (Pattern.As _ p                ) = go p
  go (Pattern.Constructor _ _ _ args) = traverse_ go args
  go (Pattern.EffectPure _ p        ) = go p
  go (Pattern.EffectBind _ id _ args k) =
    Writer.tell [id] *> traverse_ go args *> go k
  go _ = pure ()

updateDependencies :: Ord v => Map Reference Reference -> Term v -> Term v
updateDependencies u tm = ABT.rebuildUp go tm where
  -- todo: this function might need tweaking if we ever allow type replacements
  -- would need to look inside pattern matching and constructor calls
  go (Ref r) = Ref (Map.findWithDefault r r u)
  go f = f

-- | If the outermost term is a function application,
-- perform substitution of the argument into the body
betaReduce :: Var v => Term v -> Term v
betaReduce (App' (Lam' f) arg) = ABT.bind f arg
betaReduce e = e

hashComponents
  :: Var v => Map v (AnnotatedTerm v a) -> Map v (Reference, AnnotatedTerm v a)
hashComponents m = Reference.hashComponents (\r -> ref () r) m

-- The hash for a constructor
hashConstructor'
  :: (Reference -> Int -> Term Symbol) -> Reference -> Int -> Reference
hashConstructor' f r cid =
  let
-- this is a bit circuitous, but defining everything in terms of hashComponents
-- ensure the hashing is always done in the same way
      m = hashComponents (Map.fromList [(Var.named "_" :: Symbol, f r cid)])
  in  case toList m of
        [(r, _)] -> r
        _        -> error "unpossible"

hashConstructor :: Reference -> Int -> Reference
hashConstructor = hashConstructor' $ constructor ()

hashRequest :: Reference -> Int -> Reference
hashRequest = hashConstructor' $ request ()

-- Problem: how do we avoid needing a Req pattern here?
-- fromReferent is used by:
--    Names.bindTerm, used by UnisonFile.bindBuiltins.
--    Typechecker.substSuggestion; a referent is suggested based on a name
--      Could we get around this by going directly from name to type?

fromReferent ::
  Ord v => (Reference -> CT.ConstructorType)
        -> a -> Referent -> AnnotatedTerm2 vt at ap v a
fromReferent ct a = \case
  Referent.Ref r -> ref a r
  Referent.Con r i -> case ct r of
    CT.Data -> constructor a r i
    CT.Effect -> request a r i

anf :: ∀ vt at v a . (Semigroup a, Var v)
    => AnnotatedTerm2 vt at a v a -> AnnotatedTerm2 vt at a v a
anf t = go t
 where
  ann = ABT.annotation
  isVar (Var' _) = True
  isVar _        = False
  isClosedLam t@(LamNamed' _ _) | Set.null (ABT.freeVars t) = True
  isClosedLam _ = False
  fixAp t f args =
    let args' = Map.fromList $ toVar =<< (args `zip` [0 ..])
        toVar (b, i)
          | isVar b
          = []
          | otherwise
          = [(i, ABT.fresh t (Var.named . Text.pack $ "arg" ++ show i))]
        argsANF = map toANF (args `zip` [0 ..])
        toANF (b, i) = maybe b (var (ann b)) $ Map.lookup i args'
        addLet (b, i) body =
          maybe body (\v -> let1' False [(v, go b)] body) (Map.lookup i args')
    in  foldr addLet (apps' f argsANF) (args `zip` [(0 :: Int) ..])
  go :: AnnotatedTerm2 vt at a v a -> AnnotatedTerm2 vt at a v a
  go (Apps' f@(LamsNamed' vs body) args) | isClosedLam f = ap vs body args
   where
    ap vs       body  []           = lam' (ann f) vs body
    ap (v : vs) body  (arg : args) = let1' False [(v, arg)] $ ap vs body args
    ap []       _body _args        = error "type error"
  go t@(Apps' f args)
    | isVar f
    = fixAp t f args
    | otherwise
    = let fv' = ABT.fresh t (Var.named "f")
      in  let1' False [(fv', anf f)] (fixAp t (var (ann f) fv') args)
  go e@(Handle' h body)
    | isVar h
    = handle (ann e) h (go body)
    | otherwise
    = let h' = ABT.fresh e (Var.named "handler")
      in  let1' False [(h', go h)] (handle (ann e) (var (ann h) h') (go body))
  go e@(If' cond t f)
    | isVar cond
    = iff (ann e) cond (go t) (go f)
    | otherwise
    = let cond' = ABT.fresh e (Var.named "cond")
      in  let1' False
                [(cond', anf cond)]
                (iff (ann e) (var (ann cond) cond') t f)
  go e@(Match' scrutinee cases)
    | isVar scrutinee
    = match (ann e) scrutinee (fmap go <$> cases)
    | otherwise
    = let scrutinee' = ABT.fresh e (Var.named "scrutinee")
      in  let1' False
                [(scrutinee', go scrutinee)]
                (match (ann e) (var (ann scrutinee) scrutinee') cases)
  go e@(And' x y)
    | isVar x
    = and (ann e) x (go y)
    | otherwise
    = let x' = ABT.fresh e (Var.named "argX")
      in  let1' False [(x', anf x)] (and (ann e) (var (ann x) x') (go y))
  go e@(Or' x y)
    | isVar x
    = or (ann e) x (go y)
    | otherwise
    = let x' = ABT.fresh e (Var.named "argX")
      in  let1' False [(x', go x)] (or (ann e) (var (ann x) x') (go y))
  go e@(ABT.Tm'  f               ) = ABT.tm' (ann e) (go <$> f)
  go e@(ABT.Var' _               ) = e
  go e@(ABT.out -> ABT.Cycle body) = ABT.cycle' (ann e) (go body)
  go e@(ABT.out -> ABT.Abs v body) = ABT.abs' (ann e) v (go body)
  go e                             = e

instance Var v => Hashable1 (F v a p) where
  hash1 hashCycle hash e
    = let (tag, hashed, varint) =
            (Hashable.Tag, Hashable.Hashed, Hashable.Nat . fromIntegral)
      in
        case e of
        -- So long as `Reference.Derived` ctors are created using the same
        -- hashing function as is used here, this case ensures that references
        -- are 'transparent' wrt hash and hashing is unaffected by whether
        -- expressions are linked. So for example `x = 1 + 1` and `y = x` hash
        -- the same.
          Ref (Reference.Derived h 0 1) -> Hashable.fromBytes (Hash.toBytes h)
          Ref (Reference.Derived h i n) -> Hashable.accumulate
            [ tag 1
            , hashed $ Hashable.fromBytes (Hash.toBytes h)
            , Hashable.Nat i
            , Hashable.Nat n
            ]
          -- Note: start each layer with leading `1` byte, to avoid collisions
          -- with types, which start each layer with leading `0`.
          -- See `Hashable1 Type.F`
          _ ->
            Hashable.accumulate
              $ tag 1
              : case e of
                  Nat     i -> [tag 64, accumulateToken i]
                  Int     i -> [tag 65, accumulateToken i]
                  Float   n -> [tag 66, Hashable.Double n]
                  Boolean b -> [tag 67, accumulateToken b]
                  Text    t -> [tag 68, accumulateToken t]
                  Blank   b -> tag 1 : case b of
                    B.Blank -> [tag 0]
                    B.Recorded (B.Placeholder _ s) ->
                      [tag 1, Hashable.Text (Text.pack s)]
                    B.Recorded (B.Resolve _ s) ->
                      [tag 2, Hashable.Text (Text.pack s)]
                  Ref (Reference.Builtin name) -> [tag 2, accumulateToken name]
                  Ref (Reference.Derived _ _ _) ->
                    error "handled above, but GHC can't figure this out"
                  App a a2  -> [tag 3, hashed (hash a), hashed (hash a2)]
                  Ann a t   -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
                  Vector as -> tag 5 : varint (Vector.length as) : map
                    (hashed . hash)
                    (Vector.toList as)
                  Lam a         -> [tag 6, hashed (hash a)]
                  -- note: we use `hashCycle` to ensure result is independent of
                  -- let binding order
                  LetRec _ as a -> case hashCycle as of
                    (hs, hash) -> tag 7 : hashed (hash a) : map hashed hs
                  -- here, order is significant, so don't use hashCycle
                  Let _ b a -> [tag 8, hashed $ hash b, hashed $ hash a]
                  If b t f ->
                    [tag 9, hashed $ hash b, hashed $ hash t, hashed $ hash f]
                  Request     r n -> [tag 10, accumulateToken r, varint n]
                  Constructor r n -> [tag 12, accumulateToken r, varint n]
                  Match e branches ->
                    tag 13 : hashed (hash e) : concatMap h branches
                   where
                    h (MatchCase pat guard branch) = concat
                      [ [accumulateToken pat]
                      , toList (hashed . hash <$> guard)
                      , [hashed (hash branch)]
                      ]
                  Handle h b -> [tag 15, hashed $ hash h, hashed $ hash b]
                  And    x y -> [tag 16, hashed $ hash x, hashed $ hash y]
                  Or     x y -> [tag 17, hashed $ hash x, hashed $ hash y]
                  _ ->
                    error $ "unhandled case in show: " <> show (const () <$> e)

-- mostly boring serialization code below ...

instance (Eq a, Var v) => Eq1 (F v a p) where (==#) = (==)
instance (Var v) => Show1 (F v a p) where showsPrec1 = showsPrec

instance (Var vt, Eq at, Eq a) => Eq (F vt at p a) where
  Int x == Int y = x == y
  Nat x == Nat y = x == y
  Float x == Float y = x == y
  Boolean x == Boolean y = x == y
  Text x == Text y = x == y
  Blank b == Blank q = b == q
  Ref x == Ref y = x == y
  Constructor r cid == Constructor r2 cid2 = r == r2 && cid == cid2
  Request r cid == Request r2 cid2 = r == r2 && cid == cid2
  Handle h b == Handle h2 b2 = h == h2 && b == b2
  App f a == App f2 a2 = f == f2 && a == a2
  Ann e t == Ann e2 t2 = e == e2 && t == t2
  Vector v == Vector v2 = v == v2
  If a b c == If a2 b2 c2 = a == a2 && b == b2 && c == c2
  And a b == And a2 b2 = a == a2 && b == b2
  Or a b == Or a2 b2 = a == a2 && b == b2
  Lam a == Lam b = a == b
  LetRec _ bs body == LetRec _ bs2 body2 = bs == bs2 && body == body2
  Let _ binding body == Let _ binding2 body2 =
    binding == binding2 && body == body2
  Match scrutinee cases == Match s2 cs2 = scrutinee == s2 && cases == cs2
  _ == _ = False


instance (Var v, Show a) => Show (F v a0 p a) where
  showsPrec p fa = go p fa
   where
    showConstructor r n = showsPrec 0 r <> s "#" <> showsPrec 0 n
    go _ (Int     n    ) = (if n >= 0 then s "+" else s "") <> showsPrec 0 n
    go _ (Nat     n    ) = showsPrec 0 n
    go _ (Float   n    ) = showsPrec 0 n
    go _ (Boolean True ) = s "true"
    go _ (Boolean False) = s "false"
    go p (Ann t k) = showParen (p > 1) $ showsPrec 0 t <> s ":" <> showsPrec 0 k
    go p (App f x) = showParen (p > 9) $ showsPrec 9 f <> s " " <> showsPrec 10 x
    go _ (Lam    body  ) = showParen True (s "λ " <> showsPrec 0 body)
    go _ (Vector vs    ) = showListWith (showsPrec 0) (Vector.toList vs)
    go _ (Blank  b     ) = case b of
      B.Blank                        -> s "_"
      B.Recorded (B.Placeholder _ r) -> s ("_" ++ r)
      B.Recorded (B.Resolve     _ r) -> s r
    go _ (Ref r) = s "Ref(" <> showsPrec 0 r <> s ")"
    go _ (Let _ b body) =
      showParen True (s "let " <> showsPrec 0 b <> s " in " <> showsPrec 0 body)
    go _ (LetRec _ bs body) = showParen
      True
      (s "let rec" <> showsPrec 0 bs <> s " in " <> showsPrec 0 body)
    go _ (Handle b body) = showParen
      True
      (s "handle " <> showsPrec 0 b <> s " in " <> showsPrec 0 body)
    go _ (Constructor r         n    ) = showConstructor r n
    go _ (Match       scrutinee cases) = showParen
      True
      (s "case " <> showsPrec 0 scrutinee <> s " of " <> showsPrec 0 cases)
    go _ (Text s     ) = showsPrec 0 s
    go _ (Request r n) = showConstructor r n
    go p (If c t f) =
      showParen (p > 0)
        $  s "if "
        <> showsPrec 0 c
        <> s " then "
        <> showsPrec 0 t
        <> s " else "
        <> showsPrec 0 f
    go p (And x y) =
      showParen (p > 0) $ s "and " <> showsPrec 0 x <> s " " <> showsPrec 0 y
    go p (Or x y) =
      showParen (p > 0) $ s "or " <> showsPrec 0 x <> s " " <> showsPrec 0 y
    (<>) = (.)
    s    = showString
