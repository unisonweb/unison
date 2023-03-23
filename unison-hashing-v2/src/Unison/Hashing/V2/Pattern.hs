module Unison.Hashing.V2.Pattern
  ( Pattern (..),
    SeqOp (..),
  )
where

import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hashing.V2.Reference (Reference)
import qualified Unison.Hashing.V2.Tokenizable as H
import Unison.Prelude

data Pattern loc
  = PatternUnbound loc
  | PatternVar loc
  | PatternBoolean loc !Bool
  | PatternInt loc !Int64
  | PatternNat loc !Word64
  | PatternFloat loc !Double
  | PatternText loc !Text
  | PatternChar loc !Char
  | PatternConstructor loc !Reference !ConstructorId [Pattern loc]
  | PatternAs loc (Pattern loc)
  | PatternEffectPure loc (Pattern loc)
  | PatternEffectBind loc !Reference !ConstructorId [Pattern loc] (Pattern loc)
  | PatternSequenceLiteral loc [Pattern loc]
  | PatternSequenceOp loc (Pattern loc) !SeqOp (Pattern loc)
  deriving stock (Foldable, Functor, Generic, Ord, Show, Traversable)

data SeqOp
  = Cons
  | Snoc
  | Concat
  deriving (Eq, Show, Ord, Generic)

instance H.Tokenizable SeqOp where
  tokens Cons = [H.Tag 0]
  tokens Snoc = [H.Tag 1]
  tokens Concat = [H.Tag 2]

instance H.Tokenizable (Pattern p) where
  tokens (PatternUnbound _) = [H.Tag 0]
  tokens (PatternVar _) = [H.Tag 1]
  tokens (PatternBoolean _ b) = H.Tag 2 : [H.Tag $ if b then 1 else 0]
  tokens (PatternInt _ n) = H.Tag 3 : [H.Int n]
  tokens (PatternNat _ n) = H.Tag 4 : [H.Nat n]
  tokens (PatternFloat _ f) = H.Tag 5 : H.tokens f
  tokens (PatternConstructor _ r n args) =
    [H.Tag 6, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args]
  tokens (PatternEffectPure _ p) = H.Tag 7 : H.tokens p
  tokens (PatternEffectBind _ r n args k) =
    [H.Tag 8, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args, H.accumulateToken k]
  tokens (PatternAs _ p) = H.Tag 9 : H.tokens p
  tokens (PatternText _ t) = H.Tag 10 : H.tokens t
  tokens (PatternSequenceLiteral _ ps) = H.Tag 11 : concatMap H.tokens ps
  tokens (PatternSequenceOp _ l op r) = H.Tag 12 : H.tokens op ++ H.tokens l ++ H.tokens r
  tokens (PatternChar _ c) = H.Tag 13 : H.tokens c

instance Eq (Pattern loc) where
  PatternUnbound _ == PatternUnbound _ = True
  PatternVar _ == PatternVar _ = True
  PatternBoolean _ b == PatternBoolean _ b2 = b == b2
  PatternInt _ n == PatternInt _ m = n == m
  PatternNat _ n == PatternNat _ m = n == m
  PatternFloat _ f == PatternFloat _ g = f == g
  PatternConstructor _ r n args == PatternConstructor _ s m brgs = r == s && n == m && args == brgs
  PatternEffectPure _ p == PatternEffectPure _ q = p == q
  PatternEffectBind _ r ctor ps k == PatternEffectBind _ r2 ctor2 ps2 k2 = r == r2 && ctor == ctor2 && ps == ps2 && k == k2
  PatternAs _ p == PatternAs _ q = p == q
  PatternText _ t == PatternText _ t2 = t == t2
  PatternSequenceLiteral _ ps == PatternSequenceLiteral _ ps2 = ps == ps2
  PatternSequenceOp _ ph op pt == PatternSequenceOp _ ph2 op2 pt2 = ph == ph2 && op == op2 && pt == pt2
  _ == _ = False
