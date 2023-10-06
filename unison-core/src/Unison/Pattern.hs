{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Pattern where

import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.Core.ConstructorId (ConstructorId)
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Type qualified as Type

data Pattern loc
  = Unbound loc
  | Var loc
  | Boolean loc !Bool
  | Int loc !Int64
  | Nat loc !Word64
  | Float loc !Double
  | Text loc !Text
  | Char loc !Char
  | Constructor loc !ConstructorReference [Pattern loc]
  | As loc (Pattern loc)
  | EffectPure loc (Pattern loc)
  | EffectBind loc !ConstructorReference [Pattern loc] (Pattern loc)
  | SequenceLiteral loc [Pattern loc]
  | SequenceOp loc (Pattern loc) !SeqOp (Pattern loc)
  deriving (Ord, Generic, Functor, Foldable, Traversable)

data SeqOp
  = Cons
  | Snoc
  | Concat
  deriving (Eq, Show, Ord, Generic)

updateDependencies :: Map Referent Referent -> Pattern loc -> Pattern loc
updateDependencies tms p = case p of
  Unbound {} -> p
  Var {} -> p
  Boolean {} -> p
  Int {} -> p
  Nat {} -> p
  Float {} -> p
  Text {} -> p
  Char {} -> p
  Constructor loc r ps -> case Map.lookup (Referent.Con r CT.Data) tms of
    Just (Referent.Con r CT.Data) -> Constructor loc r (updateDependencies tms <$> ps)
    _ -> Constructor loc r (updateDependencies tms <$> ps)
  As loc p -> As loc (updateDependencies tms p)
  EffectPure loc p -> EffectPure loc (updateDependencies tms p)
  EffectBind loc r pats k -> case Map.lookup (Referent.Con r CT.Effect) tms of
    Just (Referent.Con r CT.Effect) ->
      EffectBind loc r (updateDependencies tms <$> pats) (updateDependencies tms k)
    _ ->
      EffectBind loc r (updateDependencies tms <$> pats) (updateDependencies tms k)
  SequenceLiteral loc ps -> SequenceLiteral loc (updateDependencies tms <$> ps)
  SequenceOp loc lhs op rhs ->
    SequenceOp loc (updateDependencies tms lhs) op (updateDependencies tms rhs)

hasSubpattern :: Pattern loc -> Pattern loc -> Bool
hasSubpattern (Unbound {}) _ = True
hasSubpattern (Var {}) _ = True
hasSubpattern needle haystack = needle == haystack || go haystack
  where
    go Unbound {} = False
    go Var {} = False
    go Int {} = False
    go Nat {} = False
    go Float {} = False
    go Boolean {} = False
    go Text {} = False
    go Char {} = False
    go (Constructor _ _ ps) = any (hasSubpattern needle) ps
    go (As _ p) = hasSubpattern needle p
    go (EffectPure _ p) = hasSubpattern needle p
    go (EffectBind _ _ ps p) = any (hasSubpattern needle) ps || hasSubpattern needle p
    go (SequenceLiteral _ ps) = any (hasSubpattern needle) ps
    go (SequenceOp _ ph _ pt) = hasSubpattern needle ph || hasSubpattern needle pt

instance Show (Pattern loc) where
  show (Unbound _) = "Unbound"
  show (Var _) = "Var"
  show (Boolean _ x) = "Boolean " <> show x
  show (Int _ x) = "Int " <> show x
  show (Nat _ x) = "Nat " <> show x
  show (Float _ x) = "Float " <> show x
  show (Text _ t) = "Text " <> show t
  show (Char _ c) = "Char " <> show c
  show (Constructor _ (ConstructorReference r i) ps) =
    "Constructor " <> unwords [show r, show i, show ps]
  show (As _ p) = "As " <> show p
  show (EffectPure _ k) = "EffectPure " <> show k
  show (EffectBind _ (ConstructorReference r i) ps k) =
    "EffectBind " <> unwords [show r, show i, show ps, show k]
  show (SequenceLiteral _ ps) = "Sequence " <> intercalate ", " (fmap show ps)
  show (SequenceOp _ ph op pt) = "Sequence " <> show ph <> " " <> show op <> " " <> show pt

application :: Pattern loc -> Bool
application (Constructor _ _ (_ : _)) = True
application _ = False

loc :: Pattern loc -> loc
loc = \case
  Unbound loc -> loc
  Var loc -> loc
  Boolean loc _ -> loc
  Int loc _ -> loc
  Nat loc _ -> loc
  Float loc _ -> loc
  Text loc _ -> loc
  Char loc _ -> loc
  Constructor loc _ _ -> loc
  As loc _ -> loc
  EffectPure loc _ -> loc
  EffectBind loc _ _ _ -> loc
  SequenceLiteral loc _ -> loc
  SequenceOp loc _ _ _ -> loc

setLoc :: Pattern loc -> loc -> Pattern loc
setLoc p loc = case p of
  EffectBind _ a b c -> EffectBind loc a b c
  EffectPure _ a -> EffectPure loc a
  As _ a -> As loc a
  Constructor _ a b -> Constructor loc a b
  SequenceLiteral _ ps -> SequenceLiteral loc ps
  SequenceOp _ ph op pt -> SequenceOp loc ph op pt
  x -> fmap (const loc) x

instance Eq (Pattern loc) where
  Unbound _ == Unbound _ = True
  Var _ == Var _ = True
  Char _ c == Char _ d = c == d
  Boolean _ b == Boolean _ b2 = b == b2
  Int _ n == Int _ m = n == m
  Nat _ n == Nat _ m = n == m
  Float _ f == Float _ g = f == g
  Constructor _ r args == Constructor _ s brgs = r == s && args == brgs
  EffectPure _ p == EffectPure _ q = p == q
  EffectBind _ r ps k == EffectBind _ r2 ps2 k2 = r == r2 && ps == ps2 && k == k2
  As _ p == As _ q = p == q
  Text _ t == Text _ t2 = t == t2
  SequenceLiteral _ ps == SequenceLiteral _ ps2 = ps == ps2
  SequenceOp _ ph op pt == SequenceOp _ ph2 op2 pt2 = ph == ph2 && op == op2 && pt == pt2
  _ == _ = False

foldMap' :: (Monoid m) => (Pattern loc -> m) -> Pattern loc -> m
foldMap' f p = case p of
  Unbound _ -> f p
  Var _ -> f p
  Boolean _ _ -> f p
  Int _ _ -> f p
  Nat _ _ -> f p
  Float _ _ -> f p
  Text _ _ -> f p
  Char _ _ -> f p
  Constructor _ _ ps -> f p <> foldMap (foldMap' f) ps
  As _ p' -> f p <> foldMap' f p'
  EffectPure _ p' -> f p <> foldMap' f p'
  EffectBind _ _ ps p' -> f p <> foldMap (foldMap' f) ps <> foldMap' f p'
  SequenceLiteral _ ps -> f p <> foldMap (foldMap' f) ps
  SequenceOp _ p1 _ p2 -> f p <> foldMap' f p1 <> foldMap' f p2

data GdHandler r = GdHandler {
  gdLiteralType :: TypeReference -> r,
  gdDataCtor :: TypeReference -> ConstructorId -> r,
  gdDataCtorType :: TypeReference -> r,
  gdEffectCtor :: TypeReference -> ConstructorId -> r,
  gdEffectCtorType :: TypeReference -> r
}
generalizedDependencies ::
  (Ord r) =>
  GdHandler r ->
  Pattern loc ->
  Set r
generalizedDependencies GdHandler{..}=
  Set.fromList
    . foldMap'
      ( \case
          Unbound _ -> mempty
          Var _ -> mempty
          As _ _ -> mempty
          Constructor _ (ConstructorReference r cid) _ ->
            [gdDataCtorType r, gdDataCtor r cid]
          EffectPure _ _ -> []
          EffectBind _ (ConstructorReference r cid) _ _ ->
            [gdEffectCtorType r, gdEffectCtor r cid]
          SequenceLiteral _ _ -> [gdLiteralType Type.listRef]
          SequenceOp {} -> [gdLiteralType Type.listRef]
          Boolean _ _ -> [gdLiteralType Type.booleanRef]
          Int _ _ -> [gdLiteralType Type.intRef]
          Nat _ _ -> [gdLiteralType Type.natRef]
          Float _ _ -> [gdLiteralType Type.floatRef]
          Text _ _ -> [gdLiteralType Type.textRef]
          Char _ _ -> [gdLiteralType Type.charRef]
      )

labeledDependencies :: Pattern loc -> Set LabeledDependency
labeledDependencies =
  generalizedDependencies GdHandler {
    gdLiteralType = LD.typeRef,
    gdDataCtor = \r i -> LD.dataConstructor (ConstructorReference r i),
    gdDataCtorType = LD.typeRef,
    gdEffectCtor = \r i -> LD.effectConstructor (ConstructorReference r i),
    gdEffectCtorType = LD.typeRef
  }
