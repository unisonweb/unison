{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V1.Term (
  Term,
  F(..),
  MatchCase(MatchCase),
  hashClosedTerm,
  hashComponents,
) where

import Unison.Prelude
import Prelude hiding (and,or)

import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Prelude.Extras (Eq1 (..), Show1 (..))
import Text.Show
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import qualified Unison.Hash as Hash
import Unison.Hashable (Hashable1, accumulateToken)
import qualified Unison.Hashable as Hashable
import qualified Unison.Hashing.V1.ABT as ABT
import Unison.Hashing.V1.Pattern (Pattern)
import qualified Unison.Hashing.V1.Pattern as Pattern
import Unison.Hashing.V1.Reference (Reference)
import qualified Unison.Hashing.V1.Reference as Reference
import qualified Unison.Hashing.V1.Reference.Util as ReferenceUtil
import Unison.Hashing.V1.Referent (Referent)
import Unison.Hashing.V1.Type (Type)
import Unison.Var (Var)

-- This gets reexported; should maybe live somewhere other than Pattern, though.
type ConstructorId = Pattern.ConstructorId

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
  | Char Char
  | Blank (B.Blank typeAnn)
  | Ref Reference
  -- First argument identifies the data type,
  -- second argument identifies the constructor
  | Constructor Reference ConstructorId
  | Request Reference ConstructorId
  | Handle a a
  | App a a
  | Ann a (Type typeVar typeAnn)
  | List (Seq a)
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
  | TermLink Referent
  | TypeLink Reference
  deriving (Foldable,Functor,Generic,Generic1,Traversable)

type IsTop = Bool

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type Term v a = Term2 v a a v a
-- | Allow type variables, term variables, type annotations and term annotations
-- to all differ
type Term2 vt at ap v a = ABT.Term (F vt at ap) v a

ref :: Ord v => a -> Reference -> Term2 vt at ap v a
ref a r = ABT.tm' a (Ref r)

refId :: Ord v => a -> Reference.Id -> Term2 vt at ap v a
refId a = ref a . Reference.DerivedId

hashComponents
  :: Var v => Map v (Term v a) -> Map v (Reference.Id, Term v a)
hashComponents = ReferenceUtil.hashComponents $ refId ()

hashClosedTerm :: Var v => Term v a -> Reference.Id
hashClosedTerm tm = Reference.Id (ABT.hash tm) 0 1

-- The hash for a constructor

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
                  Char    c -> [tag 69, accumulateToken c]
                  Blank   b -> tag 1 : case b of
                    B.Blank -> [tag 0]
                    B.Recorded (B.Placeholder _ s) ->
                      [tag 1, Hashable.Text (Text.pack s)]
                    B.Recorded (B.Resolve _ s) ->
                      [tag 2, Hashable.Text (Text.pack s)]
                  Ref (Reference.Builtin name) -> [tag 2, accumulateToken name]
                  Ref Reference.Derived {} ->
                    error "handled above, but GHC can't figure this out"
                  App a a2  -> [tag 3, hashed (hash a), hashed (hash a2)]
                  Ann a t   -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
                  List as -> tag 5 : varint (Sequence.length as) : map
                    (hashed . hash)
                    (toList as)
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
                  TermLink r -> [tag 18, accumulateToken r]
                  TypeLink r -> [tag 19, accumulateToken r]

-- mostly boring serialization code below ...

instance (Eq a, ABT.Var v) => Eq1 (F v a p) where (==#) = (==)
instance (Show v) => Show1 (F v a p) where showsPrec1 = showsPrec

instance (ABT.Var vt, Eq at, Eq a) => Eq (F vt at p a) where
  Int x == Int y = x == y
  Nat x == Nat y = x == y
  Float x == Float y = x == y
  Boolean x == Boolean y = x == y
  Text x == Text y = x == y
  Char x == Char y = x == y
  Blank b == Blank q = b == q
  Ref x == Ref y = x == y
  TermLink x == TermLink y = x == y
  TypeLink x == TypeLink y = x == y
  Constructor r cid == Constructor r2 cid2 = r == r2 && cid == cid2
  Request r cid == Request r2 cid2 = r == r2 && cid == cid2
  Handle h b == Handle h2 b2 = h == h2 && b == b2
  App f a == App f2 a2 = f == f2 && a == a2
  Ann e t == Ann e2 t2 = e == e2 && t == t2
  List v == List v2 = v == v2
  If a b c == If a2 b2 c2 = a == a2 && b == b2 && c == c2
  And a b == And a2 b2 = a == a2 && b == b2
  Or a b == Or a2 b2 = a == a2 && b == b2
  Lam a == Lam b = a == b
  LetRec _ bs body == LetRec _ bs2 body2 = bs == bs2 && body == body2
  Let _ binding body == Let _ binding2 body2 =
    binding == binding2 && body == body2
  Match scrutinee cases == Match s2 cs2 = scrutinee == s2 && cases == cs2
  _ == _ = False


instance (Show v, Show a) => Show (F v a0 p a) where
  showsPrec = go
   where
    go _ (Int     n    ) = (if n >= 0 then s "+" else s "") <> shows n
    go _ (Nat     n    ) = shows n
    go _ (Float   n    ) = shows n
    go _ (Boolean True ) = s "true"
    go _ (Boolean False) = s "false"
    go p (Ann t k) = showParen (p > 1) $ shows t <> s ":" <> shows k
    go p (App f x) = showParen (p > 9) $ showsPrec 9 f <> s " " <> showsPrec 10 x
    go _ (Lam    body  ) = showParen True (s "λ " <> shows body)
    go _ (List vs    ) = showListWith shows (toList vs)
    go _ (Blank  b     ) = case b of
      B.Blank                        -> s "_"
      B.Recorded (B.Placeholder _ r) -> s ("_" ++ r)
      B.Recorded (B.Resolve     _ r) -> s r
    go _ (Ref r) = s "Ref(" <> shows r <> s ")"
    go _ (TermLink r) = s "TermLink(" <> shows r <> s ")"
    go _ (TypeLink r) = s "TypeLink(" <> shows r <> s ")"
    go _ (Let _ b body) =
      showParen True (s "let " <> shows b <> s " in " <> shows body)
    go _ (LetRec _ bs body) = showParen
      True
      (s "let rec" <> shows bs <> s " in " <> shows body)
    go _ (Handle b body) = showParen
      True
      (s "handle " <> shows b <> s " in " <> shows body)
    go _ (Constructor r         n    ) = s "Con" <> shows r <> s "#" <> shows n
    go _ (Match       scrutinee cases) = showParen
      True
      (s "case " <> shows scrutinee <> s " of " <> shows cases)
    go _ (Text s     ) = shows s
    go _ (Char c     ) = shows c
    go _ (Request r n) = s "Req" <> shows r <> s "#" <> shows n
    go p (If c t f) =
      showParen (p > 0)
        $  s "if "
        <> shows c
        <> s " then "
        <> shows t
        <> s " else "
        <> shows f
    go p (And x y) =
      showParen (p > 0) $ s "and " <> shows x <> s " " <> shows y
    go p (Or x y) =
      showParen (p > 0) $ s "or " <> shows x <> s " " <> shows y
    (<>) = (.)
    s    = showString