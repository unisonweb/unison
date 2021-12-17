{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Hashing.V2.Pattern where

import Data.Foldable as Foldable hiding (foldMap')
import Data.List (intercalate)
import qualified Data.Set as Set
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import qualified Unison.Hashing.V2.Tokenizable as H
import Unison.Hashing.V2.Reference (Reference)
import qualified Unison.Hashing.V2.Type as Type
import Unison.Prelude

data Pattern loc
  = Unbound loc
  | Var loc
  | Boolean loc !Bool
  | Int loc !Int64
  | Nat loc !Word64
  | Float loc !Double
  | Text loc !Text
  | Char loc !Char
  | Constructor loc !Reference !ConstructorId [Pattern loc]
  | As loc (Pattern loc)
  | EffectPure loc (Pattern loc)
  | EffectBind loc !Reference !ConstructorId [Pattern loc] (Pattern loc)
  | SequenceLiteral loc [Pattern loc]
  | SequenceOp loc (Pattern loc) !SeqOp (Pattern loc)
  deriving (Ord, Generic, Functor, Foldable, Traversable)

data SeqOp
  = Cons
  | Snoc
  | Concat
  deriving (Eq, Show, Ord, Generic)

instance H.Tokenizable SeqOp where
  tokens Cons = [H.Tag 0]
  tokens Snoc = [H.Tag 1]
  tokens Concat = [H.Tag 2]

instance Show (Pattern loc) where
  show (Unbound _) = "Unbound"
  show (Var _) = "Var"
  show (Boolean _ x) = "Boolean " <> show x
  show (Int _ x) = "Int " <> show x
  show (Nat _ x) = "Nat " <> show x
  show (Float _ x) = "Float " <> show x
  show (Text _ t) = "Text " <> show t
  show (Char _ c) = "Char " <> show c
  show (Constructor _ r i ps) =
    "Constructor " <> unwords [show r, show i, show ps]
  show (As _ p) = "As " <> show p
  show (EffectPure _ k) = "EffectPure " <> show k
  show (EffectBind _ r i ps k) =
    "EffectBind " <> unwords [show r, show i, show ps, show k]
  show (SequenceLiteral _ ps) = "Sequence " <> intercalate ", " (fmap show ps)
  show (SequenceOp _ ph op pt) = "Sequence " <> show ph <> " " <> show op <> " " <> show pt

application :: Pattern loc -> Bool
application (Constructor _ _ _ (_ : _)) = True
application _ = False

loc :: Pattern loc -> loc
loc p = head $ Foldable.toList p

setLoc :: Pattern loc -> loc -> Pattern loc
setLoc p loc = case p of
  EffectBind _ a b c d -> EffectBind loc a b c d
  EffectPure _ a -> EffectPure loc a
  As _ a -> As loc a
  Constructor _ a b c -> Constructor loc a b c
  SequenceLiteral _ ps -> SequenceLiteral loc ps
  SequenceOp _ ph op pt -> SequenceOp loc ph op pt
  x -> fmap (const loc) x

instance H.Tokenizable (Pattern p) where
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

foldMap' :: Monoid m => (Pattern loc -> m) -> Pattern loc -> m
foldMap' f p = case p of
  Unbound _ -> f p
  Var _ -> f p
  Boolean _ _ -> f p
  Int _ _ -> f p
  Nat _ _ -> f p
  Float _ _ -> f p
  Text _ _ -> f p
  Char _ _ -> f p
  Constructor _ _ _ ps -> f p <> foldMap (foldMap' f) ps
  As _ p' -> f p <> foldMap' f p'
  EffectPure _ p' -> f p <> foldMap' f p'
  EffectBind _ _ _ ps p' -> f p <> foldMap (foldMap' f) ps <> foldMap' f p'
  SequenceLiteral _ ps -> f p <> foldMap (foldMap' f) ps
  SequenceOp _ p1 _ p2 -> f p <> foldMap' f p1 <> foldMap' f p2

generalizedDependencies ::
  Ord r =>
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  Pattern loc ->
  Set r
generalizedDependencies literalType dataConstructor dataType effectConstructor effectType =
  Set.fromList
    . foldMap'
      ( \case
          Unbound _ -> mempty
          Var _ -> mempty
          As _ _ -> mempty
          Constructor _ r cid _ -> [dataType r, dataConstructor r cid]
          EffectPure _ _ -> [effectType Type.effectRef]
          EffectBind _ r cid _ _ ->
            [effectType Type.effectRef, effectType r, effectConstructor r cid]
          SequenceLiteral _ _ -> [literalType Type.listRef]
          SequenceOp {} -> [literalType Type.listRef]
          Boolean _ _ -> [literalType Type.booleanRef]
          Int _ _ -> [literalType Type.intRef]
          Nat _ _ -> [literalType Type.natRef]
          Float _ _ -> [literalType Type.floatRef]
          Text _ _ -> [literalType Type.textRef]
          Char _ _ -> [literalType Type.charRef]
      )
