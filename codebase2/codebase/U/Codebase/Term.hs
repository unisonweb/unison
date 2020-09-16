{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Term where

import Data.Int (Int64)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic, Generic1)
import U.Codebase.Reference (Reference, Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Type (TypeR)
import U.Util.Hash (Hash)
import qualified U.Core.ABT as ABT
import qualified U.Util.Hashable as H

type ConstructorId = Word64

type Term v = ABT.Term (F v) v ()

-- | Base functor for terms in the Unison codebase
type F vt =
  F'
    Text -- text
    (Reference' Text (Maybe Hash)) -- termRef
    Reference -- typeRef
    (Referent' (Reference' Text (Maybe Hash)) (Reference' Text Hash)) -- termLink
    Reference -- typeLink
    vt

-- | Generalized version.  We could generalize further to allow sharing within
--  terms.
data F' text termRef typeRef termLink typeLink vt a
  = Int Int64
  | Nat Word64
  | Float Double
  | Boolean Bool
  | Text text
  | Char Char
  | Ref termRef
  | -- First argument identifies the data type,
    -- second argument identifies the constructor
    Constructor typeRef ConstructorId
  | Request typeRef ConstructorId
  | Handle a a
  | App a a
  | Ann a (TypeR typeRef vt)
  | Sequence (Seq a)
  | If a a a
  | And a a
  | Or a a
  | Lam a
  | -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
    -- variables as there are bindings
    LetRec [a] a
  | -- Note: first parameter is the binding, second is the expression which may refer
    -- to this let bound variable. Constructed as `Let b (abs v e)`
    Let a a
  | -- Pattern matching / eliminating data types, example:
    --  case x of
    --    Just n -> rhs1
    --    Nothing -> rhs2
    --
    -- translates to
    --
    --   Match x
    --     [ (Constructor 0 [Var], ABT.abs n rhs1)
    --     , (Constructor 1 [], rhs2) ]
    Match a [MatchCase text typeRef a]
  | TermLink termLink
  | TypeLink typeLink
  deriving (Foldable, Functor, Traversable)

data MatchCase t r a = MatchCase (Pattern t r) (Maybe a) a
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

data Pattern t r
  = PUnbound
  | PVar
  | PBoolean !Bool
  | PInt !Int64
  | PNat !Word64
  | PFloat !Double
  | PText !t
  | PChar !Char
  | PConstructor !r !Int [Pattern t r]
  | PAs (Pattern t r)
  | PEffectPure (Pattern t r)
  | PEffectBind !r !Int [Pattern t r] (Pattern t r)
  | PSequenceLiteral [Pattern t r]
  | PSequenceOp (Pattern t r) !SeqOp (Pattern t r)
  deriving (Generic, Functor, Foldable, Traversable)

data SeqOp
  = PCons
  | PSnoc
  | PConcat
  deriving (Eq, Show)

-- rmap ::
--   (termRef -> termRef') ->
--   (typeRef -> typeRef') ->
--   (termLink -> termLink') ->
--   TermR termRef typeRef termLink typeRef (TypeR typeRef vt at) blankRepr ap v a ->
--   TermR termRef' typeRef' termLink' typeRef' (TypeR typeRef' vt at) blankRepr ap v a
-- rmap fTermRef fTypeRef fTermLink t =
--   extraMap fTermRef fTypeRef fTermLink fTypeRef (Type.rmap fTypeRef) undefined id t

-- rmapPattern :: (r -> r') -> Pattern r loc -> Pattern r' loc
-- rmapPattern f = \case
--   PConstructor loc r i ps -> PConstructor loc (f r) i (rmap f <$> ps)
--   PAs loc p -> PAs loc (rmap f p)
--   PEffectPure loc p -> PEffectPure loc (rmap f p)
--   PEffectBind loc r i ps p -> PEffectBind loc (f r) i (rmap f <$> ps) (rmap f p)
--   PSequenceLiteral loc ps -> PSequenceLiteral loc (rmap f <$> ps)
--   PSequenceOp loc p1 op p2 -> PSequenceOp loc (rmap f p1) op (rmap f p2)
--   -- cover all cases having references or subpatterns above; the rest are fine
--   x -> unsafeCoerce x

instance H.Hashable SeqOp where
  tokens PCons = [H.Tag 0]
  tokens PSnoc = [H.Tag 1]
  tokens PConcat = [H.Tag 2]