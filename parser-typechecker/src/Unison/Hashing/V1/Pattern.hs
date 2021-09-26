{-# Language DeriveTraversable, DeriveGeneric, PatternSynonyms,  OverloadedStrings #-}

module Unison.Hashing.V1.Pattern (Pattern(..), ConstructorId, SeqOp(..)) where

import Unison.Prelude

import Data.List (intercalate)
import qualified Unison.Hashable as H
import Unison.Hashing.V1.Reference (Reference)

type ConstructorId = Int

data Pattern loc
  = Unbound loc
  | Var loc
  | Boolean loc !Bool
  | Int loc !Int64
  | Nat loc !Word64
  | Float loc !Double
  | Text loc !Text
  | Char loc !Char
  | Constructor loc !Reference !Int [Pattern loc]
  | As loc (Pattern loc)
  | EffectPure loc (Pattern loc)
  | EffectBind loc !Reference !Int [Pattern loc] (Pattern loc)
  | SequenceLiteral loc [Pattern loc]
  | SequenceOp loc (Pattern loc) !SeqOp (Pattern loc)
    deriving (Ord,Generic,Functor,Foldable,Traversable)

data SeqOp = Cons
           | Snoc
           | Concat
           deriving (Eq, Show, Ord, Generic)

instance H.Hashable SeqOp where
  tokens Cons = [H.Tag 0]
  tokens Snoc = [H.Tag 1]
  tokens Concat = [H.Tag 2]

instance Show (Pattern loc) where
  show (Unbound _  ) = "Unbound"
  show (Var     _  ) = "Var"
  show (Boolean _ x) = "Boolean " <> show x
  show (Int   _ x) = "Int " <> show x
  show (Nat  _ x) = "Nat " <> show x
  show (Float   _ x) = "Float " <> show x
  show (Text   _ t) = "Text " <> show t
  show (Char   _ c) = "Char " <> show c
  show (Constructor _ r i ps) = "Constructor " <> unwords [show r, show i, show ps]
  show (As         _ p) = "As " <> show p
  show (EffectPure _ k) = "EffectPure " <> show k
  show (EffectBind _ r i ps k) = "EffectBind " <> unwords [show r, show i, show ps, show k]
  show (SequenceLiteral _ ps) = "Sequence " <> intercalate ", " (fmap show ps)
  show (SequenceOp _ ph op pt) = "Sequence " <> show ph <> " " <> show op <> " " <> show pt

instance H.Hashable (Pattern p) where
  tokens (Unbound _) = [H.Tag 0]
  tokens (Var _) = [H.Tag 1]
  tokens (Boolean _ b) = H.Tag 2 : [H.Tag $ if b then 1 else 0]
  tokens (Int _ n) = H.Tag 3 : [H.Int n]
  tokens (Nat _ n) = H.Tag 4 : [H.Nat n]
  tokens (Float _ f) = H.Tag 5 : H.tokens f
  tokens (Constructor _ r n args) =
    [H.Tag 6, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args]
  tokens (EffectPure _ p) = H.Tag 7 : H.tokens p
  tokens (EffectBind _ r n args k) =
    [H.Tag 8, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args, H.accumulateToken k]
  tokens (As _ p) = H.Tag 9 : H.tokens p
  tokens (Text _ t) = H.Tag 10 : H.tokens t
  tokens (SequenceLiteral _ ps) = H.Tag 11 : concatMap H.tokens ps
  tokens (SequenceOp _ l op r) = H.Tag 12 : H.tokens op ++ H.tokens l ++ H.tokens r
  tokens (Char _ c) = H.Tag 13 : H.tokens c

instance Eq (Pattern loc) where
  Unbound _ == Unbound _ = True
  Var _ == Var _ = True
  Boolean _ b == Boolean _ b2 = b == b2
  Int _ n == Int _ m = n == m
  Nat _ n == Nat _ m = n == m
  Float _ f == Float _ g = f == g
  Constructor _ r n args == Constructor _ s m brgs = r == s && n == m && args == brgs
  EffectPure _ p == EffectPure _ q = p == q
  EffectBind _ r ctor ps k == EffectBind _ r2 ctor2 ps2 k2 = r == r2 && ctor == ctor2 && ps == ps2 && k == k2
  As _ p == As _ q = p == q
  Text _ t == Text _ t2 = t == t2
  SequenceLiteral _ ps == SequenceLiteral _ ps2 = ps == ps2
  SequenceOp _ ph op pt == SequenceOp _ ph2 op2 pt2 = ph == ph2 && op == op2 && pt == pt2
  _ == _ = False
