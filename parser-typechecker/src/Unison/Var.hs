{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language ScopedTypeVariables #-}

module Unison.Var where

import Data.Char (toLower, isLower)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Word (Word64)
import Unison.Util.Monoid (intercalateMap)
import Unison.Reference (Reference)
import qualified Unison.Reference as R

-- | A class for variables. Variables may have auxiliary information which
-- may not form part of their identity according to `Eq` / `Ord`. Laws:
--
--   * `typeOf (typed n) == n`
--   * `Set.notMember (freshIn vs v) vs`:
--     `freshIn` returns a variable not used in the `Set`
--   * `typeOf (freshIn vs v) == typeOf v`:
--     `freshIn` does not alter the name
class (Show v, Eq v, Ord v) => Var v where
  typed :: Type -> v
  retype :: Type -> v -> v
  typeOf :: v -> Type
  freshId :: v -> Word64
  freshIn :: Set v -> v -> v
  freshenId :: Word64 -> v -> v

named :: Var v => Text -> v
named n = typed (User n)

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
  RefNamed r -> "ℍ" <> R.toText r <> showid v
  MissingResult -> "_" <> showid v
  Blank -> "_" <> showid v
  UnnamedWatch k guid -> fromString k <> "." <> guid <> showid v
  AskInfo -> "?" <> showid v
  where
  showid (freshId -> 0) = ""
  showid (freshId -> n) = pack (show n)

uncapitalize :: Var v => v -> v
uncapitalize v = nameds $ go (nameStr v) where
  go (c:rest) = toLower c : rest
  go n = n

askInfo, missingResult, blank, inferInput, inferOutput, inferAbility,
  inferPatternPureE, inferPatternPureV, inferPatternBindE, inferPatternBindV,
  inferTypeConstructor, inferTypeConstructorArg,
  inferOther :: Var v => v
askInfo = typed AskInfo
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
  -- Variables created in `makeSelfContained` for Evaluation
  | RefNamed Reference
  -- Variables created to finish a block that doesn't end with an expression
  | MissingResult
  -- Variables invented to query the typechecker for the type of subexpressions
  | AskInfo
  -- Variables invented for placeholder values inserted by user or by TDNR
  | Blank
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

freshes :: Var v => Set v -> [v] -> [v]
freshes _ [] = []
freshes used (h:t) =
  let h' = freshIn used h
  in h' : freshes (Set.insert h' used) t

freshInBoth :: Var v => Set v -> Set v -> v -> v
freshInBoth vs1 vs2 = freshIn vs1 . freshIn vs2

freshNamed :: Var v => Set v -> Text -> v
freshNamed used n = freshIn used (named n)

syntheticVars :: Var v => Set v
syntheticVars = Set.fromList . fmap typed $ [
  Inference Ability,
  Inference Input,
  Inference Output,
  Inference PatternPureE,
  Inference PatternPureV,
  Inference PatternBindE,
  Inference PatternBindV,
  Inference TypeConstructor,
  Inference TypeConstructorArg ]

isLowercase :: forall v . Var v => v -> Bool
isLowercase v = 
  ok (name $ reset v) && unqualified v == v
  where
  ok n = (all isLower . take 1 . Text.unpack) n ||
         Set.member n syntheticVarNames
  syntheticVarNames :: Set Text 
  syntheticVarNames = Set.map name (syntheticVars @v)
