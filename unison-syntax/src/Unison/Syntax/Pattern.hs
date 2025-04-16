module Unison.Syntax.Pattern
  ( Pattern (..),
    setPos,
    SeqOp (..),
  )
where

import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Syntax.Lexer.Token (Token)
import Unison.Syntax.Parser (Annotated (..))

data Pattern v
  = As Ann (Token v) (Pattern v)
  | Boolean Ann !Bool
  | Char Ann !Char
  | Constructor Ann !(Token (HashQualified Name)) [Pattern v]
  | EffectBind Ann !(Token (HashQualified Name)) [Pattern v] (Pattern v)
  | EffectPure Ann (Pattern v)
  | Float Ann !Double
  | Int Ann !Int64
  | Nat Ann !Word64
  | Pair Ann (Pattern v) (Pattern v)
  | SequenceLiteral Ann [Pattern v]
  | SequenceOp Ann (Pattern v) !SeqOp (Pattern v)
  | Text Ann !Text
  | Unbound Ann
  | Unit Ann
  | -- There's unfortunately no syntactic difference between nullary constructors and variables,
    -- so we can't commit to one or the other yet.
    VarOrNullaryConstructor Ann !(Token Name)
  deriving stock (Show)

instance Annotated (Pattern v) where
  ann = \case
    As pos _ _ -> pos
    Boolean pos _ -> pos
    Char pos _ -> pos
    Constructor pos _ _ -> pos
    EffectBind pos _ _ _ -> pos
    EffectPure pos _ -> pos
    Float pos _ -> pos
    Int pos _ -> pos
    Nat pos _ -> pos
    Pair pos _ _ -> pos
    SequenceLiteral pos _ -> pos
    SequenceOp pos _ _ _ -> pos
    Text pos _ -> pos
    Unbound pos -> pos
    Unit pos -> pos
    VarOrNullaryConstructor pos _ -> pos

setPos :: Ann -> Pattern v -> Pattern v
setPos pos = \case
  As _ a b -> As pos a b
  Boolean _ a -> Boolean pos a
  Char _ a -> Char pos a
  Constructor _ a b -> Constructor pos a b
  EffectBind _ a b c -> EffectBind pos a b c
  EffectPure _ a -> EffectPure pos a
  Float _ a -> Float pos a
  Int _ a -> Int pos a
  Nat _ a -> Nat pos a
  Pair _ a b -> Pair pos a b
  SequenceLiteral _ a -> SequenceLiteral pos a
  SequenceOp _ a b c -> SequenceOp pos a b c
  Text _ a -> Text pos a
  Unbound _ -> Unbound pos
  Unit _ -> Unit pos
  VarOrNullaryConstructor _ a -> VarOrNullaryConstructor pos a

data SeqOp
  = Concat
  | Cons
  | Snoc
  deriving stock (Show)
