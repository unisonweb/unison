{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.V1.Term.Pattern where

import Data.Int (Int64)
import qualified Data.Set as Set
import qualified Unison.Codebase.V1.LabeledDependency as LD
import Unison.Codebase.V1.LabeledDependency (LabeledDependency)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Unison.Codebase.V1.Reference (Reference)
import qualified Unison.Codebase.V1.Type as Type

type ConstructorId = Int

data Pattern
  = Unbound
  | Var
  | Boolean !Bool
  | Int !Int64
  | Nat !Word64
  | Float !Double
  | Text !Text
  | Char !Char
  | Constructor !Reference !Int [Pattern]
  | As Pattern
  | EffectPure Pattern
  | EffectBind !Reference !Int [Pattern] Pattern
  | SequenceLiteral [Pattern]
  | SequenceOp Pattern !SeqOp Pattern
  deriving (Eq, Ord, Show)

data SeqOp
  = Cons
  | Snoc
  | Concat
  deriving (Eq, Ord, Show)

application :: Pattern -> Bool
application (Constructor _ _ (_ : _)) = True
application _ = False

foldMap' :: Monoid m => (Pattern -> m) -> Pattern -> m
foldMap' f p = case p of
  Unbound -> f p
  Var -> f p
  Boolean _ -> f p
  Int _ -> f p
  Nat _ -> f p
  Float _ -> f p
  Text _ -> f p
  Char _ -> f p
  Constructor _ _ ps -> f p <> foldMap (foldMap' f) ps
  As p' -> f p <> foldMap' f p'
  EffectPure p' -> f p <> foldMap' f p'
  EffectBind _ _ ps p' -> f p <> foldMap (foldMap' f) ps <> foldMap' f p'
  SequenceLiteral ps -> f p <> foldMap (foldMap' f) ps
  SequenceOp p1 _ p2 -> f p <> foldMap' f p1 <> foldMap' f p2

generalizedDependencies ::
  Ord r =>
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  Pattern ->
  Set r
generalizedDependencies literalType dataConstructor dataType effectConstructor effectType =
  Set.fromList
    . foldMap'
      ( \case
          Unbound -> mempty
          Var -> mempty
          As _ -> mempty
          Constructor r cid _ -> [dataType r, dataConstructor r cid]
          EffectPure _ -> [effectType Type.effectRef]
          EffectBind r cid _ _ ->
            [effectType Type.effectRef, effectType r, effectConstructor r cid]
          SequenceLiteral _ -> [literalType Type.listRef]
          SequenceOp {} -> [literalType Type.listRef]
          Boolean _ -> [literalType Type.booleanRef]
          Int _ -> [literalType Type.intRef]
          Nat _ -> [literalType Type.natRef]
          Float _ -> [literalType Type.floatRef]
          Text _ -> [literalType Type.textRef]
          Char _ -> [literalType Type.charRef]
      )

labeledDependencies :: Pattern -> Set LabeledDependency
labeledDependencies =
  generalizedDependencies
    LD.typeRef
    LD.dataConstructor
    LD.typeRef
    LD.effectConstructor
    LD.typeRef
