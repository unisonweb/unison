{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.V1.Term where

import qualified Control.Monad.Writer.Strict as Writer
import Data.Foldable (Foldable (toList), traverse_)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Unison.Codebase.V1.ABT as ABT
import qualified Unison.Codebase.V1.ConstructorType as CT
import qualified Unison.Codebase.V1.LabeledDependency as LD
import Unison.Codebase.V1.LabeledDependency (LabeledDependency)
import Unison.Codebase.V1.Reference (Reference)
import Unison.Codebase.V1.Referent (Referent)
import qualified Unison.Codebase.V1.Referent as Referent
import Unison.Codebase.V1.Term.Pattern (Pattern)
import qualified Unison.Codebase.V1.Term.Pattern as Pattern
import Unison.Codebase.V1.Type (Type)
import qualified Unison.Codebase.V1.Type as Type

-- This gets reexported; should maybe live somewhere other than Pattern, though.
type ConstructorId = Pattern.ConstructorId

data MatchCase a = MatchCase Pattern (Maybe a) a
  deriving (Foldable, Functor, Traversable)

-- | Base functor for terms in the Unison language
-- We need `typeVar` because the term and type variables may differ.
data F typeVar typeAnn a
  = Int Int64
  | Nat Word64
  | Float Double
  | Boolean Bool
  | Text Text
  | Char Char
  | Ref Reference
  | -- First argument identifies the data type,
    -- second argument identifies the constructor
    Constructor Reference ConstructorId
  | Request Reference ConstructorId
  | Handle a a
  | App a a
  | Ann a (Type typeVar typeAnn)
  | List (Seq a)
  | If a a a
  | And a a
  | Or a a
  | Lam a
  | -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
    -- variables as there are bindings
    LetRec IsTop [a] a
  | -- Note: first parameter is the binding, second is the expression which may refer
    -- to this let bound variable. Constructed as `Let b (abs v e)`
    Let IsTop a a
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
    Match a [MatchCase a]
  | TermLink Referent
  | TypeLink Reference
  deriving (Foldable, Functor, Traversable)

type IsTop = Bool

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type Term v a = ABT.Term (F v a) v a

-- Dependencies including referenced data and effect decls
dependencies :: Ord v => Term v a -> Set Reference
dependencies t = Set.map (LD.fold id Referent.toReference) (labeledDependencies t)

typeDependencies :: Ord v => Term v a -> Set Reference
typeDependencies =
  Set.fromList . mapMaybe (LD.fold Just (const Nothing)) . toList . labeledDependencies

-- Gets the types to which this term contains references via patterns and
-- data constructors.
constructorDependencies ::
  Ord v => Term v a -> Set Reference
constructorDependencies =
  Set.unions
    . generalizedDependencies
      (const mempty)
      (const mempty)
      Set.singleton
      (const . Set.singleton)
      Set.singleton
      (const . Set.singleton)
      Set.singleton

generalizedDependencies ::
  (Ord v, Ord r) =>
  (Reference -> r) ->
  (Reference -> r) ->
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  Term v a ->
  Set r
generalizedDependencies termRef typeRef literalType dataConstructor dataType effectConstructor effectType =
  Set.fromList . Writer.execWriter . ABT.visit' f
  where
    f t@(Ref r) = Writer.tell [termRef r] $> t
    f t@(TermLink r) = case r of
      Referent.Ref r -> Writer.tell [termRef r] $> t
      Referent.Con r id CT.Data -> Writer.tell [dataConstructor r id] $> t
      Referent.Con r id CT.Effect -> Writer.tell [effectConstructor r id] $> t
    f t@(TypeLink r) = Writer.tell [typeRef r] $> t
    f t@(Ann _ typ) =
      Writer.tell (map typeRef . toList $ Type.dependencies typ) $> t
    f t@(Nat _) = Writer.tell [literalType Type.natRef] $> t
    f t@(Int _) = Writer.tell [literalType Type.intRef] $> t
    f t@(Float _) = Writer.tell [literalType Type.floatRef] $> t
    f t@(Boolean _) = Writer.tell [literalType Type.booleanRef] $> t
    f t@(Text _) = Writer.tell [literalType Type.textRef] $> t
    f t@(List _) = Writer.tell [literalType Type.listRef] $> t
    f t@(Constructor r cid) =
      Writer.tell [dataType r, dataConstructor r cid] $> t
    f t@(Request r cid) =
      Writer.tell [effectType r, effectConstructor r cid] $> t
    f t@(Match _ cases) = traverse_ goPat cases $> t
    f t = pure t
    goPat (MatchCase pat _ _) =
      Writer.tell . toList $
        Pattern.generalizedDependencies
          literalType
          dataConstructor
          dataType
          effectConstructor
          effectType
          pat

labeledDependencies ::
  Ord v => Term v a -> Set LabeledDependency
labeledDependencies =
  generalizedDependencies
    LD.termRef
    LD.typeRef
    LD.typeRef
    LD.dataConstructor
    LD.typeRef
    LD.effectConstructor
    LD.typeRef
