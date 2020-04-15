{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}

module Unison.Var where

import Unison.Prelude

import Data.Char (toLower, isLower)
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import Unison.Util.Monoid (intercalateMap)
import Unison.Reference (Reference)
import qualified Unison.Reference as R

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

freshIn :: ABT.Var v => Set v -> v -> v
freshIn = ABT.freshIn

named :: Var v => Text -> v
named n = typed (User n)

-- | Variable whose name is derived from the given reference.
refNamed :: Var v => Reference -> v
refNamed ref = named ("ℍ" <> R.toText ref)

name :: Var v => v -> Text
name v = case typeOf v of
  User n -> n <> showid v
  Inference Ability -> "𝕖" <> showid v
  Inference Input -> "𝕒" <> showid v
  Inference Output -> "𝕣" <> showid v
  Inference Other -> "𝕩" <> showid v
  Inference PatternPureE -> "𝕞" <> showid v
  Inference PatternPureV -> "𝕧" <> showid v
  Inference PatternBindE -> "𝕞" <> showid v
  Inference PatternBindV -> "𝕧" <> showid v
  Inference TypeConstructor -> "𝕗" <> showid v
  Inference TypeConstructorArg -> "𝕦" <> showid v
  MissingResult -> "_" <> showid v
  Blank -> "_" <> showid v
  ANFBlank -> "_anf" <> showid v
  Float -> "_float" <> showid v
  Pattern -> "_pattern" <> showid v
  UnnamedWatch k guid -> fromString k <> "." <> guid <> showid v
  where
  showid (freshId -> 0) = ""
  showid (freshId -> n) = pack (show n)

uncapitalize :: Var v => v -> v
uncapitalize v = nameds $ go (nameStr v) where
  go (c:rest) = toLower c : rest
  go n = n

missingResult, blank, inferInput, inferOutput, inferAbility,
  inferPatternPureE, inferPatternPureV, inferPatternBindE, inferPatternBindV,
  inferTypeConstructor, inferTypeConstructorArg,
  inferOther :: Var v => v
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

unnamedTest :: Var v => Text -> v
unnamedTest guid = typed (UnnamedWatch TestWatch guid)

data Type
  -- User provided variables, these should generally be left alone
  = User Text
  -- Variables created during type inference
  | Inference InferenceType
  -- Variables created to finish a block that doesn't end with an expression
  | MissingResult
  -- Variables invented for placeholder values inserted by user or by TDNR
  | Blank
  -- An unnamed watch expression of the given kind, for instance:
  --
  --  test> Ok "oog"
  --    has kind "test"
  --  > 1 + 1
  --    has kind ""
  | UnnamedWatch WatchKind Text -- guid
  -- An unnamed variable introduced by ANF transformation
  | ANFBlank
  -- An unnamed variable for a floated lambda
  | Float
  -- An unnamed variable introduced from pattern compilation
  | Pattern
  deriving (Eq,Ord,Show)

type WatchKind = String

pattern RegularWatch = ""
pattern TestWatch = "test"

data InferenceType =
  Ability | Input | Output |
  PatternPureE | PatternPureV |
  PatternBindE | PatternBindV |
  TypeConstructor | TypeConstructorArg |
  Other
  deriving (Eq,Ord,Show)

reset :: Var v => v -> v
reset v = typed (typeOf v)

unqualified :: Var v => v -> v
unqualified v = case typeOf v of
  User _ -> named . unqualifiedName $ v
  _ -> v

unqualifiedName :: Var v => v -> Text
unqualifiedName = last . Text.splitOn "." . name

namespaced :: Var v => [v] -> v
namespaced vs = named $ intercalateMap "." name vs

nameStr :: Var v => v -> String
nameStr = Text.unpack . name

nameds :: Var v => String -> v
nameds s = named (Text.pack s)

joinDot :: Var v => v -> v -> v
joinDot prefix v2 =
  if name prefix == "." then named (name prefix `mappend` name v2)
  else named (name prefix `mappend` "." `mappend` name v2)

freshNamed :: Var v => Set v -> Text -> v
freshNamed used n = ABT.freshIn used (named n)

universallyQuantifyIfFree :: forall v . Var v => v -> Bool
universallyQuantifyIfFree v =
  ok (name $ reset v) && unqualified v == v
  where
  ok n = (all isLower . take 1 . Text.unpack) n
