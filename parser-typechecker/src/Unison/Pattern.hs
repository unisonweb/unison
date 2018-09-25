{-# Language DeriveFunctor, DeriveTraversable, DeriveGeneric, PatternSynonyms #-}

module Unison.Pattern where

import Data.List (intercalate)
import Data.Int (Int64)
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
  | ConstructorP loc !Reference !Int [PatternP loc]
  | AsP loc (PatternP loc)
  | EffectPureP loc (PatternP loc)
  | EffectBindP loc !Reference !Int [PatternP loc] (PatternP loc)
    deriving (Generic,Functor,Foldable,Traversable)

instance Show (PatternP loc) where
  show (UnboundP _  ) = "Unbound"
  show (VarP     _  ) = "Var"
  show (BooleanP _ x) = "Boolean " <> show x
  show (IntP   _ x) = "Int " <> show x
  show (NatP  _ x) = "Nat " <> show x
  show (FloatP   _ x) = "Float " <> show x
  show (ConstructorP _ r i ps) =
    "Constructor " <> intercalate " " [show r, show i, show ps]
  show (AsP         _ p) = "As " <> show p
  show (EffectPureP _ k) = "EffectPure " <> show k
  show (EffectBindP _ r i ps k) =
    "EffectBind " <> intercalate " " [show r, show i, show ps, show k]

loc :: PatternP loc -> loc
loc p = head $ Foldable.toList p

setLoc :: PatternP loc -> loc -> PatternP loc
setLoc p loc = case p of
  EffectBindP _ a b c d -> EffectBindP loc a b c d
  EffectPureP _ a -> EffectPureP loc a
  AsP _ a -> AsP loc a
  ConstructorP _ a b c -> ConstructorP loc a b c
  x -> fmap (const loc) x

pattern Unbound = UnboundP ()
pattern Var = VarP ()
pattern Boolean b = BooleanP () b
pattern Int n = IntP () n
pattern Nat n = NatP () n
pattern Float n = FloatP () n
pattern Constructor r cid ps = ConstructorP () r cid ps
pattern As p = AsP () p
pattern EffectPure p = EffectPureP () p
pattern EffectBind r cid ps k = EffectBindP () r cid ps k

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
  tokens (EffectBindP _ _r _ctor _ps _k) =
    H.Tag 8 : error "need fo figure out hashable"
  tokens (AsP _ p) = H.Tag 9 : H.tokens p

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
  _ == _ = False



-- idea: rename PatternP to PatternP0
-- newtype PatternP loc = PatternP (PatternP0 loc)
-- instance Eq (PatternP loc) where
--   (PatternP p) == (PatternP p2) = void p == void p2
