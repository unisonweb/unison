{-# Language DeriveFunctor, DeriveTraversable, DeriveGeneric, PatternSynonyms, ViewPatterns, OverloadedStrings #-}

module Unison.Pattern where

import Data.List (intercalate)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import Data.Foldable as Foldable
import GHC.Generics
import Unison.Reference (Reference)
import qualified Unison.Hashable as H

type Pattern = PatternP ()

-- Pattern -> Pattern loc
--   Or, `data Pattern` becomes `data PatternP loc`,
--       and introduce `type Pattern = PatternP ()`
-- To have this refactoring break a minimum of stuff:
--
-- Need backwards compat Pattern type
-- Need backwards compat patterns (ignore the `loc` parameter)
-- Need backwards compat constructors (that specialize `loc` to `()`)
-- For the new typechecker and parser, they should import the module PatternP, which
--   will go away after refactoring but which will have the alias:
  --   type Pattern loc = Pattern.PatternP loc
  --   pattern Var loc = VarP loc
  --   etc

-- pattern Var = VarP ()

data PatternP loc
  = UnboundP loc
  | VarP loc
  | BooleanP loc !Bool
  | IntP loc !Int64
  | NatP loc !Word64
  | FloatP loc !Double
  | TextP loc !Text
  | ConstructorP loc !Reference !Int [PatternP loc]
  | AsP loc (PatternP loc)
  | EffectPureP loc (PatternP loc)
  | EffectBindP loc !Reference !Int [PatternP loc] (PatternP loc)
  | SequenceLiteralP loc [PatternP loc]
  | SequenceOpP loc (PatternP loc) !SeqOp (PatternP loc)
    deriving (Generic,Functor,Foldable,Traversable)

data SeqOp = Cons
           | Snoc
           | Concat
           deriving (Eq, Show)

instance H.Hashable SeqOp where
  tokens Cons = [H.Tag 0]
  tokens Snoc = [H.Tag 1]
  tokens Concat = [H.Tag 2]

instance Show (PatternP loc) where
  show (UnboundP _  ) = "Unbound"
  show (VarP     _  ) = "Var"
  show (BooleanP _ x) = "Boolean " <> show x
  show (IntP   _ x) = "Int " <> show x
  show (NatP  _ x) = "Nat " <> show x
  show (FloatP   _ x) = "Float " <> show x
  show (TextP   _ t) = "Text " <> show t
  show (ConstructorP _ r i ps) =
    "Constructor " <> intercalate " " [show r, show i, show ps]
  show (AsP         _ p) = "As " <> show p
  show (EffectPureP _ k) = "EffectPure " <> show k
  show (EffectBindP _ r i ps k) =
    "EffectBind " <> intercalate " " [show r, show i, show ps, show k]
  show (SequenceLiteralP _ ps) = "Sequence " <> intercalate ", " (fmap show ps)
  show (SequenceOpP _ ph op pt) = "Sequence " <> show ph <> " " <> show op <> " " <> show pt

loc :: PatternP loc -> loc
loc p = head $ Foldable.toList p

setLoc :: PatternP loc -> loc -> PatternP loc
setLoc p loc = case p of
  EffectBindP _ a b c d -> EffectBindP loc a b c d
  EffectPureP _ a -> EffectPureP loc a
  AsP _ a -> AsP loc a
  ConstructorP _ a b c -> ConstructorP loc a b c
  SequenceLiteralP _ ps -> SequenceLiteralP loc ps
  SequenceOpP _ ph op pt -> SequenceOpP loc ph op pt
  x -> fmap (const loc) x

pattern Unbound = UnboundP ()
pattern Var = VarP ()
pattern Boolean b = BooleanP () b
pattern Int n = IntP () n
pattern Nat n = NatP () n
pattern Float n = FloatP () n
pattern Text t = TextP () t
pattern Constructor r cid ps = ConstructorP () r cid ps
pattern As p = AsP () p
pattern EffectPure p = EffectPureP () p
pattern EffectBind r cid ps k = EffectBindP () r cid ps k
pattern SequenceLiteral ps = SequenceLiteralP () ps
pattern SequenceOp ph op pt = SequenceOpP () ph op pt

instance H.Hashable (PatternP p) where
  tokens (UnboundP _) = [H.Tag 0]
  tokens (VarP _) = [H.Tag 1]
  tokens (BooleanP _ b) = H.Tag 2 : [H.Tag $ if b then 1 else 0]
  tokens (IntP _ n) = H.Tag 3 : [H.Int n]
  tokens (NatP _ n) = H.Tag 4 : [H.Nat n]
  tokens (FloatP _ f) = H.Tag 5 : H.tokens f
  tokens (ConstructorP _ r n args) =
    [H.Tag 6, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args]
  tokens (EffectPureP _ p) = H.Tag 7 : H.tokens p
  tokens (EffectBindP _ r n args k) =
    [H.Tag 8, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args, H.accumulateToken k]
  tokens (AsP _ p) = H.Tag 9 : H.tokens p
  tokens (TextP _ t) = H.Tag 10 : H.tokens t
  tokens (SequenceLiteralP _ ps) = H.Tag 11 : concatMap H.tokens ps
  tokens (SequenceOpP _ l op r) = H.Tag 12 : H.tokens op ++ H.tokens l ++ H.tokens r

instance Eq (PatternP loc) where
  UnboundP _ == UnboundP _ = True
  VarP _ == VarP _ = True
  BooleanP _ b == BooleanP _ b2 = b == b2
  IntP _ n == IntP _ m = n == m
  NatP _ n == NatP _ m = n == m
  FloatP _ f == FloatP _ g = f == g
  ConstructorP _ r n args == ConstructorP _ s m brgs = r == s && n == m && args == brgs
  EffectPureP _ p == EffectPureP _ q = p == q
  EffectBindP _ r ctor ps k == EffectBindP _ r2 ctor2 ps2 k2 = r == r2 && ctor == ctor2 && ps == ps2 && k == k2
  AsP _ p == AsP _ q = p == q
  TextP _ t == TextP _ t2 = t == t2
  SequenceLiteralP _ ps == SequenceLiteralP _ ps2 = ps == ps2
  SequenceOpP _ ph op pt == SequenceOpP _ ph2 op2 pt2 = ph == ph2 && op == op2 && pt == pt2
  _ == _ = False



-- idea: rename PatternP to PatternP0
-- newtype PatternP loc = PatternP (PatternP0 loc)
-- instance Eq (PatternP loc) where
--   (PatternP p) == (PatternP p2) = void p == void p2
