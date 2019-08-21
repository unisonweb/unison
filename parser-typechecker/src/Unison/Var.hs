{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}

module Unison.Var where

import Unison.Prelude

import Data.Char (toLower, isLower)
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import Unison.Util.Monoid (intercalateMap)
import Unison.Reference (Reference)
import qualified Unison.Reference as R

-- | A class for variables. Variables may have auxiliary information which
-- may not form part of their identity according to `Eq` / `Ord`.
class (Show v, ABT.Var v) => Var v where
  typed :: Type -> v
  name :: v -> Text
  freshenId :: Word64 -> v -> v

freshIn :: ABT.Var v => Set v -> v -> v
freshIn = ABT.freshIn

named :: Var v => Text -> v
named n = typed (User n)

-- | Variable whose name is derived from the given reference.
refNamed :: Var v => Reference -> v
refNamed ref = named ("ℍ" <> R.toText ref)

uncapitalize :: Var v => v -> v
uncapitalize v = nameds $ go (nameStr v) where
  go (c:rest) = toLower c : rest
  go n = n

inferInput, inferOutput, inferAbility,
  inferPatternPureE, inferPatternPureV, inferPatternBindE, inferPatternBindV,
  inferTypeConstructor, inferTypeConstructorArg,
  inferOther :: Var v => v
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
  -- An unnamed watch expression of the given kind, for instance:
  --
  --  test> Ok "oog"
  --    has kind "test"
  --  > 1 + 1
  --    has kind ""
  | UnnamedWatch WatchKind Text -- guid
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
reset v = freshenId 0 v

isQualified :: Var v => v -> Bool
isQualified v = Text.any (== '.') (name v)

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
  ok (name $ reset v) && not (isQualified v)
  where
  ok n = (all isLower . take 1 . Text.unpack) n
