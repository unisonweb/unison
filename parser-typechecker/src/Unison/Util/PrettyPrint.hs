{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Util.PrettyPrint where

import qualified Data.ListLike      as LL
import           Data.String        (IsString, fromString)
import           Unison.Util.Monoid (intercalateMap)
import Debug.Trace
-- !!

-- A tree of `a` tokens, to be rendered to a character window by traversing the
-- leaves depth-first left-to-right, marked up with specifiers about where to
-- insert line-breaks.
data PrettyPrint a
  = Empty
  | Literal a
  | Append (PrettyPrint a) (PrettyPrint a)
  -- A subtree which can be rendered across multiple lines, and then indented.
  -- Example (\b_ for Breakable space):
  --   "if foo\b_then\b_" <> Nest "     " then_body
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
renderBroken :: forall a b. (LL.ListLike a b, Eq b)
             => Int -> Bool -> b -> PrettyPrint a -> a
renderBroken width beginLine lineSeparator = trace (show width) $ \case
  Empty -> LL.empty
  Literal a -> a
  Append a b ->
    let ra = renderBroken width beginLine lineSeparator a
        trailing = lengthOfLastLine lineSeparator ra
    in ra <> renderBroken (width - trailing) (trailing == 0) lineSeparator b
    -- TODO Does the above lose us the window width, just on account of already
    --      having used some of the first line?  Will test.
  Nest prefix a ->
    if beginLine
    then
      -- Indent the subtree.
      let ra = renderBroken (width - LL.length prefix) False lineSeparator a
      in prefix <> replaceOneWithMany lineSeparator (LL.cons lineSeparator prefix) ra
    else renderBroken width False lineSeparator a
  Breakable _delim -> LL.singleton lineSeparator
  -- Going inside a Group can allow us to revert to unbroken rendering.
  Group a -> render' width lineSeparator a

  where
    replaceOneWithMany :: (LL.FoldableLL a b, Eq b) => b -> a -> a -> a
    replaceOneWithMany target replacement list =
      LL.foldr (go target replacement) LL.empty list
        where go :: (LL.FoldableLL a b, Eq b) => b -> a -> b -> a -> a
              go target replacement b a =
                if b == target then LL.append replacement a else LL.cons b a

    lengthOfLastLine :: (LL.ListLike a b, Eq b) => b -> a -> Int
    lengthOfLastLine lineSeparator ra =
      LL.length ra + 1 - case LL.reverse $ LL.findIndices (==lineSeparator) ra of
        []    -> -1
        h : _ -> h

render :: LL.ListLike a Char => Int -> PrettyPrint a -> a
render width doc = render' width '\n' doc

-- Render broken only if necessary.
render' :: (LL.ListLike a b, Eq b) => Int -> b -> PrettyPrint a -> a
render' width lineSeparator doc =
  if unbrokenWidth doc <= width
  then renderUnbroken doc
  else renderBroken width False lineSeparator doc

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
