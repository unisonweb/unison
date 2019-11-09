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
  named :: Text -> v
  name :: v -> Text
  freshId :: v -> Word64
  reset :: v -> v

freshIn :: ABT.Var v => Set v -> v -> v
freshIn = ABT.freshIn

-- | Variable whose name is derived from the given reference.
refNamed :: Var v => Reference -> v
refNamed ref = named ("ℍ" <> R.toText ref)

uncapitalize :: Var v => v -> v
uncapitalize v = nameds $ go (nameStr v) where
  go (c:rest) = toLower c : rest
  go n = n

-- Variables created during type inference
inferInput, inferOutput, inferAbility,
  inferPatternPureE, inferPatternPureV, inferPatternBindE, inferPatternBindV,
  inferTypeConstructor, inferTypeConstructorArg,
  inferOther :: Var v => v
inferInput = named "𝕒"
inferOutput = named "𝕣"
inferAbility = named "𝕖"
inferPatternPureE = named "𝕞"
inferPatternPureV = named "𝕧"
inferPatternBindE = named "𝕞"
inferPatternBindV = named "𝕧"
inferTypeConstructor = named "𝕗"
inferTypeConstructorArg = named "𝕦"
inferOther = named "𝕩"

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
