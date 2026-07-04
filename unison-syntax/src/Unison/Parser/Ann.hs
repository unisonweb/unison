{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Parser.Ann where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.List.NonEmpty (NonEmpty)
import Data.Void (absurd)
import Unison.Lexer.Pos qualified as L
import Unison.Prelude

data Ann
  = -- Used for things like Builtins which don't have a source position.
    Intrinsic -- { sig :: String, start :: L.Pos, end :: L.Pos }
  | External
  | -- Indicates that the term was generated from something at this location.
    -- E.g. generated record field accessors (get, modify, etc.) are generated from their field definition, so are tagged
    -- with @GeneratedFrom <field position>@
    GeneratedFrom Ann
  | -- Indicates a synthesized term inserted by the elaborator that has
    -- no surface representation in the user's source — currently used
    -- for dictionary arguments filled into @=>@ slots by implicit
    -- resolution. The wrapped 'Ann' is the source position the
    -- synthesized term should be /associated/ with (typically the
    -- apply-site of the function being called), so LSP queries that
    -- find a node "at" a position still find this synthesised arg.
    -- Pretty-printers detect this constructor and elide the arg from
    -- the surface rendering (the default elide-mode).
    Synthetic Ann
  | -- Tags a term whose leading @=>@ arrows should be demoted to
    -- regular @->@ arrows at the typechecker site of use. Produced
    -- by the @give@ keyword: writing @give f@ at the source level
    -- means "give me the lowered version of f", so subsequent
    -- application supplies the dictionary explicitly instead of
    -- triggering implicit resolution. Annotation-aware passes that
    -- don't care about the distinction should recurse into the
    -- inner 'Ann' — same shape as 'Synthetic'.
    Lowered Ann
  | Ann {start :: L.Pos, end :: L.Pos}
  deriving (Eq, Ord, Show)

-- | Checks whether an annotation has a concrete position in a file.
isFileAnn :: Ann -> Bool
isFileAnn (Ann _ _) = True
isFileAnn _ = False

startingLine :: Ann -> Maybe L.Line
startingLine (Ann (L.line -> line) _) = Just line
startingLine (GeneratedFrom a) = startingLine a
startingLine (Synthetic a) = startingLine a
startingLine (Lowered a) = startingLine a
startingLine _ = Nothing

-- | 'True' iff the annotation chain ends in a 'Synthetic' marker —
-- i.e. this term was inserted by the elaborator (e.g. as a resolved
-- dictionary for an @=>@ slot) and should be elided by surface
-- printers.
isSynthetic :: Ann -> Bool
isSynthetic (Synthetic _) = True
isSynthetic (GeneratedFrom a) = isSynthetic a
isSynthetic (Lowered a) = isSynthetic a
isSynthetic _ = False

-- | 'True' iff the annotation chain ends in a 'Lowered' marker —
-- i.e. this term was the operand of the @give@ keyword and its
-- leading @=>@ arrows should be demoted to @->@ at the typechecker
-- site of use, so dictionaries can be supplied as ordinary
-- positional arguments instead of being filled by implicit
-- resolution.
isLowered :: Ann -> Bool
isLowered (Lowered _) = True
isLowered (GeneratedFrom a) = isLowered a
isLowered (Synthetic a) = isLowered a
isLowered _ = False

instance Monoid Ann where
  mempty = External

-- | This instance is commutative.
instance Semigroup Ann where
  Ann s1 e1 <> Ann s2 e2 = Ann (min s1 s2) (max e1 e2)
  -- If we have a concrete location from a file, use it
  External <> a = a
  a <> External = a
  Intrinsic <> a = a
  a <> Intrinsic = a
  GeneratedFrom a <> b = a <> b
  a <> GeneratedFrom b = a <> b
  Synthetic a <> b = Synthetic (a <> b)
  a <> Synthetic b = Synthetic (a <> b)
  Lowered a <> b = Lowered (a <> b)
  a <> Lowered b = Lowered (a <> b)

-- | Checks whether an annotation contains a given position
-- i.e. pos ∈ [start, end)
--
-- >>> Intrinsic `contains` L.Pos 1 1
-- False
--
-- >>> External `contains` L.Pos 1 1
-- False
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `contains` L.Pos 0 5
-- True
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `contains` L.Pos 0 10
-- False
contains :: Ann -> L.Pos -> Bool
contains Intrinsic _ = False
contains External _ = False
contains (Ann start end) p = start <= p && p < end
contains (GeneratedFrom ann) p = contains ann p
contains (Synthetic ann) p = contains ann p
contains (Lowered ann) p = contains ann p

-- | Checks whether an annotation contains another annotation.
--
-- i.e. pos ∈ [start, end)
--
-- >>> Intrinsic `encompasses` Ann (L.Pos 1 1) (L.Pos 2 1)
-- Nothing
--
-- >>> External `encompasses` Ann (L.Pos 1 1) (L.Pos 2 1)
-- Nothing
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `encompasses` Ann (L.Pos 0 1) (L.Pos 0 5)
-- Just True
--
-- >>> Ann (L.Pos 1 0) (L.Pos 1 10) `encompasses` Ann (L.Pos 0 0) (L.Pos 2 0)
-- Just False
encompasses :: Ann -> Ann -> Maybe Bool
encompasses Intrinsic _ = Nothing
encompasses External _ = Nothing
encompasses _ Intrinsic = Nothing
encompasses _ External = Nothing
encompasses (GeneratedFrom ann) other = encompasses ann other
encompasses ann (GeneratedFrom other) = encompasses ann other
encompasses (Synthetic ann) other = encompasses ann other
encompasses ann (Synthetic other) = encompasses ann other
encompasses (Lowered ann) other = encompasses ann other
encompasses ann (Lowered other) = encompasses ann other
encompasses (Ann start1 end1) (Ann start2 end2) =
  Just $ start1 <= start2 && end1 >= end2

class Annotated a where
  ann :: a -> Ann

-- | Predicate over a typechecker location indicating whether the
-- term it annotates was wrapped with the @give@ keyword and should
-- have its leading @=>@ arrows demoted to @->@ during synthesis.
-- Defaults to 'False' so that contexts whose @loc@ is not 'Ann'
-- simply never see lowerings — the feature is purely source-level.
class IsLoweredAnn loc where
  isLoweredAnn :: loc -> Bool
  isLoweredAnn _ = False

instance IsLoweredAnn Ann where
  isLoweredAnn = isLowered

instance IsLoweredAnn ()

instance Annotated Ann where
  ann = id

instance (Annotated a) => Annotated [a] where
  ann = foldMap ann

instance (Annotated a) => Annotated (NonEmpty a) where
  ann = foldMap ann

instance (Annotated a) => Annotated (Maybe a) where
  ann = foldMap ann

instance Annotated Void where
  ann = absurd

instance (Annotated a) => Annotated (Cofree f a) where
  ann (a :< _) = ann a
