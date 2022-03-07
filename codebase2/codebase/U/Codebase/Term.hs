{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Term where

import qualified Control.Monad.Writer as Writer
import qualified Data.Foldable as Foldable
import Data.Int (Int64)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic, Generic1)
import U.Codebase.Reference (Reference, Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Type (TypeR)
import qualified U.Codebase.Type as Type
import qualified U.Core.ABT as ABT
import U.Util.Hash (Hash)

type ConstructorId = Word64

type Term v = ABT.Term (F v) v ()

type Type v = TypeR TypeRef v

type TermRef = Reference' Text (Maybe Hash)

type TypeRef = Reference

type TermLink = Referent' (Reference' Text (Maybe Hash)) (Reference' Text Hash)

type TypeLink = Reference

-- | Base functor for terms in the Unison codebase
type F vt =
  F'
    Text
    TermRef
    TypeRef
    TermLink
    TypeLink
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
  | List (Seq a)
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
  deriving (Foldable, Functor, Traversable, Show)

data MatchCase t r a = MatchCase (Pattern t r) (Maybe a) a
  deriving (Foldable, Functor, Generic, Generic1, Traversable, Show)

data Pattern t r
  = PUnbound
  | PVar
  | PBoolean !Bool
  | PInt !Int64
  | PNat !Word64
  | PFloat !Double
  | PText !t
  | PChar !Char
  | PConstructor !r !ConstructorId [Pattern t r]
  | PAs (Pattern t r)
  | PEffectPure (Pattern t r)
  | PEffectBind !r !ConstructorId [Pattern t r] (Pattern t r)
  | PSequenceLiteral [Pattern t r]
  | PSequenceOp (Pattern t r) !SeqOp (Pattern t r)
  deriving (Generic, Functor, Foldable, Traversable, Show)

data SeqOp
  = PCons
  | PSnoc
  | PConcat
  deriving (Eq, Show)

extraMap ::
  forall
    text
    termRef
    typeRef
    termLink
    typeLink
    vt
    text'
    termRef'
    typeRef'
    termLink'
    typeLink'
    vt'
    v
    a.
  (Ord v, Ord vt') =>
  (text -> text') ->
  (termRef -> termRef') ->
  (typeRef -> typeRef') ->
  (termLink -> termLink') ->
  (typeLink -> typeLink') ->
  (vt -> vt') ->
  ABT.Term (F' text termRef typeRef termLink typeLink vt) v a ->
  ABT.Term (F' text' termRef' typeRef' termLink' typeLink' vt') v a
extraMap ftext ftermRef ftypeRef ftermLink ftypeLink fvt = go'
  where
    go' = ABT.transform go
    go :: forall x. F' text termRef typeRef termLink typeLink vt x -> F' text' termRef' typeRef' termLink' typeLink' vt' x
    go = \case
      Int i -> Int i
      Nat n -> Nat n
      Float d -> Float d
      Boolean b -> Boolean b
      Text t -> Text (ftext t)
      Char c -> Char c
      Ref r -> Ref (ftermRef r)
      Constructor r cid -> Constructor (ftypeRef r) cid
      Request r cid -> Request (ftypeRef r) cid
      Handle e h -> Handle e h
      App f a -> App f a
      Ann a typ -> Ann a (Type.rmap ftypeRef $ ABT.vmap fvt typ)
      List s -> List s
      If c t f -> If c t f
      And p q -> And p q
      Or p q -> Or p q
      Lam b -> Lam b
      LetRec bs b -> LetRec bs b
      Let a b -> Let a b
      Match s cs -> Match s (goCase <$> cs)
      TermLink r -> TermLink (ftermLink r)
      TypeLink r -> TypeLink (ftypeLink r)
    goCase :: MatchCase text typeRef x -> MatchCase text' typeRef' x
    goCase (MatchCase p g b) = MatchCase (goPat p) g b
    goPat = rmapPattern ftext ftypeRef

rmapPattern :: (t -> t') -> (r -> r') -> Pattern t r -> Pattern t' r'
rmapPattern ft fr = go
  where
    go = \case
      PUnbound -> PUnbound
      PVar -> PVar
      PBoolean b -> PBoolean b
      PInt i -> PInt i
      PNat n -> PNat n
      PFloat d -> PFloat d
      PText t -> PText (ft t)
      PChar c -> PChar c
      PConstructor r i ps -> PConstructor (fr r) i (go <$> ps)
      PAs p -> PAs (go p)
      PEffectPure p -> PEffectPure (go p)
      PEffectBind r i ps p -> PEffectBind (fr r) i (go <$> ps) (go p)
      PSequenceLiteral ps -> PSequenceLiteral (go <$> ps)
      PSequenceOp p1 op p2 -> PSequenceOp (go p1) op (go p2)

dependencies ::
  (Ord termRef, Ord typeRef, Ord termLink, Ord typeLink, Ord v) =>
  ABT.Term (F' text termRef typeRef termLink typeLink vt) v a ->
  (Set termRef, Set typeRef, Set termLink, Set typeLink)
dependencies =
  Writer.execWriter . ABT.visit_ \case
    Ref r -> termRef r
    Constructor r _ -> typeRef r
    Request r _ -> typeRef r
    Match _ cases -> Foldable.for_ cases \case
      MatchCase pat _guard _body -> go pat
        where
          go = \case
            PConstructor r _i args -> typeRef r *> Foldable.traverse_ go args
            PAs pat -> go pat
            PEffectPure pat -> go pat
            PEffectBind r _i args k -> typeRef r *> Foldable.traverse_ go args *> go k
            PSequenceLiteral pats -> Foldable.traverse_ go pats
            PSequenceOp l _op r -> go l *> go r
            _ -> pure ()
    TermLink r -> termLink r
    TypeLink r -> typeLink r
    _ -> pure ()
  where
    termRef r = Writer.tell (Set.singleton r, mempty, mempty, mempty)
    typeRef r = Writer.tell (mempty, Set.singleton r, mempty, mempty)
    termLink r = Writer.tell (mempty, mempty, Set.singleton r, mempty)
    typeLink r = Writer.tell (mempty, mempty, mempty, Set.singleton r)
