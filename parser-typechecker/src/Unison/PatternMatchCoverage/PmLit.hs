module Unison.PatternMatchCoverage.PmLit where

import Unison.Prelude
import Unison.Util.Pretty (Pretty, string)

data PmLit
  = Int Int64
  | Nat Word64
  | Boolean Bool
  | Float Double
  | Text Text
  | Char Char
  deriving stock (Show, Eq, Ord)

prettyPmLit :: IsString s => PmLit -> Pretty s
prettyPmLit =
  string . \case
    Int x -> show x
    Nat x -> show x
    Boolean x -> show x
    Float x -> show x
    Text x -> show x
    Char x -> show x
