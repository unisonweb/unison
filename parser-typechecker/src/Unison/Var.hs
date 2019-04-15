{-# Language OverloadedStrings #-}

module Unison.Var where

import Data.Set (Set)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Word (Word64)
import Unison.Util.Monoid (intercalateMap)

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
  User n -> n
  Inference Ability -> "𝕖" <> pack (show (freshId v))
  Inference Input -> "𝕒" <> pack (show (freshId v))
  Inference Output -> "𝕣" <> pack (show (freshId v))
  Inference Other -> "𝕩" <> pack (show (freshId v))
  MissingResult -> "_" <> pack (show (freshId v))
  AskInfo -> "?" <> pack (show (freshId v))

askInfo :: Var v => v
askInfo = typed AskInfo

missingResult :: Var v => v
missingResult = typed MissingResult

data Type
  -- User provided variables, these should generally be left alone
  = User Text
  -- Variables created during type inference
  | Inference InferenceType
  -- Variables created to finish a block that doesn't end with an expression
  | MissingResult
  -- Variables invented to query the typechecker for the type of subexpressions
  | AskInfo
  deriving (Eq,Ord,Show)

data InferenceType = Ability | Input | Output | Other deriving (Eq,Ord,Show)

reset :: Var v => v -> v
reset v = typed (typeOf v)

unqualified :: Var v => v -> v
unqualified = named . unqualifiedName

unqualifiedName :: Var v => v -> Text
unqualifiedName = last . Text.splitOn "." . name

namespaced :: Var v => [v] -> v
namespaced vs = named $ intercalateMap "." name vs

nameStr :: Var v => v -> String
nameStr = Text.unpack . name

nameds :: Var v => String -> v
nameds s = named (Text.pack s)

joinDot :: Var v => v -> v -> v
joinDot v v2 = named (name v `mappend` "." `mappend` name v2)

freshes :: Var v => Set v -> [v] -> [v]
freshes _ [] = []
freshes used (h:t) =
  let h' = freshIn used h
  in h' : freshes (Set.insert h' used) t

freshInBoth :: Var v => Set v -> Set v -> v -> v
freshInBoth vs1 vs2 = freshIn vs1 . freshIn vs2

freshNamed :: Var v => Set v -> Text -> v
freshNamed used n = freshIn used (named n)
