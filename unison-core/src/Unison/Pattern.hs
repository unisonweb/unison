{-# Language DeriveFunctor, DeriveTraversable, DeriveGeneric, PatternSynonyms,  OverloadedStrings #-}

module Unison.Pattern where

import Unison.Prelude

import Data.List (intercalate)
import Data.Foldable as Foldable hiding (foldMap')
import Unison.Reference (Reference)
import qualified Unison.Hashable as H
import qualified Unison.Type as Type
import qualified Data.Set as Set
import qualified Unison.LabeledDependency as LD
import Unison.LabeledDependency (LabeledDependency)

type Pattern = PatternP ()

type ConstructorId = Int

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
  | CharP loc !Char
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
  show (CharP   _ c) = "Char " <> show c
  show (ConstructorP _ r i ps) =
    "Constructor " <> unwords [show r, show i, show ps]
  show (AsP         _ p) = "As " <> show p
  show (EffectPureP _ k) = "EffectPure " <> show k
  show (EffectBindP _ r i ps k) =
    "EffectBind " <> unwords [show r, show i, show ps, show k]
  show (SequenceLiteralP _ ps) = "Sequence " <> intercalate ", " (fmap show ps)
  show (SequenceOpP _ ph op pt) = "Sequence " <> show ph <> " " <> show op <> " " <> show pt

application :: PatternP loc -> Bool
application (ConstructorP _ _ _ (_ : _)) = True
application _ = False

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
pattern Char c = CharP () c
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
  tokens (CharP _ c) = H.Tag 13 : H.tokens c

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

foldMap' :: Monoid m => (PatternP loc -> m) -> PatternP loc -> m
foldMap' f p = case p of
    UnboundP _              -> f p
    VarP _                  -> f p
    BooleanP _ _            -> f p
    IntP _ _                -> f p
    NatP _ _                -> f p
    FloatP _ _              -> f p
    TextP _ _               -> f p
    CharP _ _               -> f p
    ConstructorP _ _ _ ps   -> f p <> foldMap (foldMap' f) ps
    AsP _ p'                -> f p <> foldMap' f p'
    EffectPureP _ p'        -> f p <> foldMap' f p'
    EffectBindP _ _ _ ps p' -> f p <> foldMap (foldMap' f) ps <> foldMap' f p'
    SequenceLiteralP _ ps   -> f p <> foldMap (foldMap' f) ps
    SequenceOpP _ p1 _ p2   -> f p <> foldMap' f p1 <> foldMap' f p2

-- idea: rename PatternP to PatternP0
-- newtype PatternP loc = PatternP (PatternP0 loc)
-- instance Eq (PatternP loc) where
--   (PatternP p) == (PatternP p2) = void p == void p2

generalizedDependencies
  :: Ord r
  => (Reference -> r)
  -> (Reference -> ConstructorId -> r)
  -> (Reference -> r)
  -> (Reference -> ConstructorId -> r)
  -> (Reference -> r)
  -> PatternP loc
  -> Set r
generalizedDependencies literalType dataConstructor dataType effectConstructor effectType
  = Set.fromList . foldMap'
    (\case
      UnboundP _             -> mempty
      VarP     _             -> mempty
      AsP _ _                -> mempty
      ConstructorP _ r cid _ -> [dataType r, dataConstructor r cid]
      EffectPureP _ _        -> [effectType Type.effectRef]
      EffectBindP _ r cid _ _ ->
        [effectType Type.effectRef, effectType r, effectConstructor r cid]
      SequenceLiteralP _ _ -> [literalType Type.vectorRef]
      SequenceOpP{}        -> [literalType Type.vectorRef]
      BooleanP _ _         -> [literalType Type.booleanRef]
      IntP     _ _         -> [literalType Type.intRef]
      NatP     _ _         -> [literalType Type.natRef]
      FloatP   _ _         -> [literalType Type.floatRef]
      TextP    _ _         -> [literalType Type.textRef]
      CharP    _ _         -> [literalType Type.charRef]
    )

labeledDependencies :: PatternP loc -> Set LabeledDependency
labeledDependencies = generalizedDependencies LD.typeRef
                                              LD.dataConstructor
                                              LD.typeRef
                                              LD.effectConstructor
                                              LD.typeRef
