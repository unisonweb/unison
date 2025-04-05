module Unison.Var
  ( Var (..),
    Type (..),
    InferenceType (..),
    bakeId,
    blank,
    freshIn,
    inferAbility,
    inferInput,
    inferOther,
    inferOutput,
    inferPatternBindE,
    inferPatternBindV,
    inferPatternPureE,
    inferPatternPureV,
    inferTypeConstructor,
    inferTypeConstructorArg,
    isAction,
    missingResult,
    name,
    nameStr,
    named,
    nameds,
    rawName,
    reset,
    uncapitalize,
    universallyQuantifyIfFree,
    unnamedRef,
    unnamedTest,
  )
where

import Data.Char (isAlphaNum, isLower, toLower)
import Data.Text (pack)
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.WatchKind (WatchKind, pattern TestWatch)

-- | A class for variables. Variables may have auxiliary information which
-- may not form part of their identity according to `Eq` / `Ord`. Laws:
--
--   * `typeOf (typed n) == n`
--   * `typeOf (ABT.freshIn vs v) == typeOf v`:
--     `ABT.freshIn` does not alter the name
class (Show v, ABT.Var v) => Var v where
  typed :: Type -> v
  typeOf :: v -> Type
  freshId :: v -> Word64
  freshenId :: Word64 -> v -> v

freshIn :: (ABT.Var v) => Set v -> v -> v
freshIn = ABT.freshIn

named :: (Var v) => Text -> v
named n = typed (User n)

-- This bakes the fresh id into the name portion of the variable
-- and resets the id to 0.
bakeId :: (Var v) => v -> v
bakeId v = named (name v)

rawName :: Type -> Text
rawName typ = case typ of
  User n -> n
  Inference Ability -> "𝕖"
  Inference Input -> "𝕒"
  Inference Output -> "𝕣"
  Inference Other -> "𝕩"
  Inference PatternPureE -> "𝕞"
  Inference PatternPureV -> "𝕧"
  Inference PatternBindE -> "𝕞"
  Inference PatternBindV -> "𝕧"
  Inference TypeConstructor -> "𝕗"
  Inference TypeConstructorArg -> "𝕦"
  MissingResult -> "_"
  Blank -> "_"
  Eta -> "_eta"
  ANFBlank -> "_anf"
  Float -> "_float"
  Pattern -> "_pattern"
  Irrelevant -> "_irrelevant"
  UnnamedReference ref -> Reference.idToText ref
  UnnamedWatch k guid -> fromString k <> "." <> guid
  Delay -> "()"

name :: (Var v) => v -> Text
name v = rawName (typeOf v) <> showid v
  where
    showid (freshId -> 0) = ""
    showid (freshId -> n) = pack (show n)

-- | Currently, actions in blocks are encoded as bindings
-- with names of the form _123 (an underscore, followed by
-- 1 or more digits). This function returns `True` if the
-- input variable has this form.
--
-- Various places check for this (the pretty-printer, to
-- determine how to print the binding), and the typechecker
-- (to decide if it should ensure the binding has type `()`).
isAction :: (Var v) => v -> Bool
isAction v = case Text.unpack (name v) of
  ('_' : rest) | Just _ <- (readMaybe rest :: Maybe Int) -> True
  _ -> False

uncapitalize :: (Var v) => v -> v
uncapitalize v = nameds $ go (nameStr v)
  where
    go (c : rest) = toLower c : rest
    go n = n

missingResult,
  blank,
  inferInput,
  inferOutput,
  inferAbility,
  inferPatternPureE,
  inferPatternPureV,
  inferPatternBindE,
  inferPatternBindV,
  inferTypeConstructor,
  inferTypeConstructorArg,
  inferOther ::
    (Var v) => v
missingResult = typed MissingResult
blank = typed Blank
inferInput = typed (Inference Input)
inferOutput = typed (Inference Output)
inferAbility = typed (Inference Ability)
inferPatternPureE = typed (Inference PatternPureE)
inferPatternPureV = typed (Inference PatternPureV)
inferPatternBindE = typed (Inference PatternBindE)
inferPatternBindV = typed (Inference PatternBindV)
inferTypeConstructor = typed (Inference TypeConstructor)
inferTypeConstructorArg = typed (Inference TypeConstructorArg)
inferOther = typed (Inference Other)

unnamedRef :: (Var v) => Reference.Id -> v
unnamedRef ref = typed (UnnamedReference ref)

unnamedTest :: (Var v) => Text -> v
unnamedTest guid = typed (UnnamedWatch TestWatch guid)

data Type
  = -- User provided variables, these should generally be left alone
    User Text
  | -- Variables created during type inference
    Inference InferenceType
  | -- Variables created to finish a block that doesn't end with an expression
    MissingResult
  | -- Variables invented for placeholder values inserted by user or by TDNR
    Blank
  | -- | An unnamed reference, created during unhashing a term/decl component.
    UnnamedReference Reference.Id
  | -- An unnamed watch expression of the given kind, for instance:
    --
    --  test> Ok "oog"
    --    has kind "test"
    --  > 1 + 1
    --    has kind ""
    UnnamedWatch WatchKind Text -- guid
    -- An unnamed variable for constructor eta expansion
  | Eta
  | -- An unnamed variable introduced by ANF transformation
    ANFBlank
  | -- An unnamed variable for a floated lambda
    Float
  | -- An unnamed variable introduced from pattern compilation
    Pattern
  | -- A variable for situations where we need to make up one that
    -- definitely won't be used.
    Irrelevant
  | -- A variable used to represent the ignored argument to a thunk, as in '(1 + 1)
    Delay
  deriving (Eq, Ord, Show)

data InferenceType
  = Ability
  | Input
  | Output
  | PatternPureE
  | PatternPureV
  | PatternBindE
  | PatternBindV
  | TypeConstructor
  | TypeConstructorArg
  | Other
  deriving (Eq, Ord, Show)

reset :: (Var v) => v -> v
reset v = typed (typeOf v)

nameStr :: (Var v) => v -> String
nameStr = Text.unpack . name

nameds :: (Var v) => String -> v
nameds s = named (Text.pack s)

universallyQuantifyIfFree :: forall v. (Var v) => v -> Bool
universallyQuantifyIfFree v =
  Text.all isLower (Text.take 1 n) && Text.all isAlphaNum n
  where
    n = name $ reset v
