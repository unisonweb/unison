{-# Language DeriveFunctor, DeriveTraversable, DeriveGeneric, PatternSynonyms #-}

module Unison.Pattern where

import Data.Int (Int64)
import Data.Word (Word64)
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
  | Int64P loc !Int64
  | UInt64P loc !Word64
  | FloatP loc !Double
  | ConstructorP loc !Reference !Int [PatternP loc]
  | AsP loc (PatternP loc)
  | EffectPureP loc (PatternP loc)
  | EffectBindP loc !Reference !Int [PatternP loc] (PatternP loc)
    deriving (Generic,Eq,Show,Functor,Foldable,Traversable)

pattern Unbound = UnboundP ()
pattern Var = VarP ()
pattern Boolean b = BooleanP () b
pattern Int64 n = Int64P () n
pattern UInt64 n = UInt64P () n
pattern Float n = FloatP () n
pattern Constructor r cid ps = ConstructorP () r cid ps
pattern As p = AsP () p
pattern EffectPure p = EffectPureP () p
pattern EffectBind r cid ps k = EffectBindP () r cid ps k

instance H.Hashable (PatternP p) where
  tokens (UnboundP _) = [H.Tag 0]
  tokens (VarP _) = [H.Tag 1]
  tokens (BooleanP _ b) = H.Tag 2 : [H.Tag $ if b then 1 else 0]
  tokens (Int64P _ n) = H.Tag 3 : [H.Int64 n]
  tokens (UInt64P _ n) = H.Tag 4 : [H.UInt64 n]
  tokens (FloatP _ f) = H.Tag 5 : H.tokens f
  tokens (ConstructorP _ r n args) =
    [H.Tag 6, H.accumulateToken r, H.UInt64 $ fromIntegral n, H.accumulateToken args]
  tokens (EffectPureP _ p) = H.Tag 7 : H.tokens p
  tokens (EffectBindP _ _r _ctor _ps _k) =
    H.Tag 8 : error "need fo figure out hashable"
  tokens (AsP _ p) = H.Tag 9 : H.tokens p
