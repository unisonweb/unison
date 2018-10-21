{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Util.PrettyPrint where

import qualified Data.ListLike      as LL
import           Data.String        (IsString, fromString)
import           Unison.Util.Monoid (intercalateMap)

-- A tree of `a` tokens, to be rendered to a character window by traversing the
-- leaves depth-first left-to-right, marked up with specifiers about where to
-- insert line-breaks.
data PrettyPrint a
  = Empty
  | Literal a
  | Append (PrettyPrint a) (PrettyPrint a)
  -- A subtree which can be rendered across multiple lines, and then indented.
  -- Example (\b_ for Breakable space):
  --   "if foo\b_then\b_" <> Nest "  " then_body
  | Nest a (PrettyPrint a)
  -- A delimiter token, which can optionally be replaced with a newline.
  | Breakable a
  -- A subtree for which we can decide to render broken or unbroken, separately
  -- from the enclosing tree.
  -- Example: (UInt64\b_-> UInt64\b_-> UInt64)
  | Group (PrettyPrint a)

unbrokenWidth :: LL.ListLike a b => PrettyPrint a -> Int
unbrokenWidth = \case
  Empty -> 0
  Literal a -> LL.length a
  Append a b -> unbrokenWidth a + unbrokenWidth b
  Nest _prefix a -> unbrokenWidth a
  Breakable a -> LL.length a
  Group a -> unbrokenWidth a

renderUnbroken :: Monoid a => PrettyPrint a -> a
renderUnbroken = \case
  Empty -> mempty
  Literal a -> a
  Append a b -> renderUnbroken a <> renderUnbroken b
  Nest _prefix a -> renderUnbroken a
  Breakable delim -> delim
  Group a -> renderUnbroken a

-- Render a `PrettyPrint a` into a rectangular window of width `width` characters.
-- `leading` characters of the first line have already been used (can be > width).
-- `start` is True if this is at the start of the outer-most term being printed.
renderBroken :: forall a b. (LL.ListLike a b, Eq b) 
             => Bool -> Int -> Int -> b -> PrettyPrint a -> a
renderBroken start width leading lineSeparator = \case
  Empty -> LL.empty
  Literal a -> a
  Append a b ->
    let ra = renderBroken False width leading lineSeparator a
        trailing = lengthOfLastLine lineSeparator ra
    in ra <> renderBroken False width trailing lineSeparator b
  Nest prefix a ->
    if ((leading == 0) && (not start))
    then
      -- Indent the subtree.
      let ra = renderBroken False (width - LL.length prefix) 0 lineSeparator a
      in prefix <> replaceOneWithMany lineSeparator (LL.cons lineSeparator prefix) ra
    else renderBroken False width leading lineSeparator a
  Breakable _delim -> LL.singleton lineSeparator
  -- Going inside a Group can allow us to revert to unbroken rendering.
  Group a -> render' False width leading lineSeparator a

  where  
    replaceOneWithMany :: (LL.FoldableLL a b, Eq b) => b -> a -> a -> a
    replaceOneWithMany target replacement list =
      LL.foldr (go target replacement) LL.empty list
        where go :: (LL.FoldableLL a b, Eq b) => b -> a -> b -> a -> a
              go target replacement b a =
                if b == target then LL.append replacement a else LL.cons b a

    lengthOfLastLine :: (LL.ListLike a b, Eq b) => b -> a -> Int
    lengthOfLastLine lineSeparator ra = 
      let ixs = LL.findIndices (==lineSeparator) ra in 
      (LL.length ra) - case ixs of 
                         [] -> 0
                         _  -> (LL.last ixs) + 1


render :: (LL.ListLike a Char) => Int -> PrettyPrint a -> a
render width doc = render' True width 0 '\n' doc

-- Render broken only if necessary.
render' :: (LL.ListLike a b, Eq b) => Bool -> Int -> Int -> b -> PrettyPrint a -> a
render' start width leading lineSeparator doc =
  if unbrokenWidth doc <= width - leading
  then renderUnbroken doc
  else renderBroken start width leading lineSeparator doc

softbreak :: IsString a => PrettyPrint a
softbreak = Breakable " "

semicolon :: IsString a => PrettyPrint a
semicolon = Breakable "; "

comma :: IsString a => PrettyPrint a
comma = Breakable ", "

softbreaks :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
softbreaks = intercalateMap softbreak id

semicolons :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
semicolons = intercalateMap semicolon id

commas :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
commas = intercalateMap comma id

instance Semigroup (PrettyPrint a) where
  (<>) = mappend

instance Monoid (PrettyPrint a) where
  mempty = Empty
  mappend a b = Append a b

instance IsString a => IsString (PrettyPrint a) where
  fromString = Literal . fromString

instance Show a => Show (PrettyPrint a) where 
  show = \case
    Empty -> "Empty"
    Literal a -> "Literal " ++ (show a)
    Append a b -> "Append (" ++ (show a) ++ ") (" ++ (show b) ++ ")"
    Nest prefix a -> "Nest (prefix = " ++ (show prefix) ++ ") (" ++ (show a) ++ ")"
    Breakable a -> "Breakable (" ++ (show a) ++ ")"
    Group a -> "Group (" ++ (show a) ++ ")"
