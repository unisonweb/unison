{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Util.PrettyPrint where

import Prelude hiding (lines)
import Data.List (foldl')
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.ListLike      as LL
import           Data.String        (IsString, fromString)
import Data.Sequence (Seq)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import           Unison.Util.Monoid (intercalateMap)

type Width = Word

-- Delta lines columns
data Delta = Delta !Word !Word

instance Semigroup Delta where (<>) = mappend
instance Monoid Delta where
  mempty = Delta 0 0
  mappend (Delta l c) (Delta 0 c2) = Delta l (c + c2)
  mappend (Delta l c) (Delta l2 c2) = Delta (l + l2) c2

data PP2 a = PP2
  { flow :: (Delta, Seq a)
  , breaks :: Seq (Width -> PP2 a) }
  deriving Functor

instance Semigroup (PP2 a) where (<>) = mappend
instance Monoid (PP2 a) where
  mempty = PP2 mempty mempty
  mappend p1 p2 =
    PP2 (flow p1 <> flow p2) (breaks p1 <> breaks p2)

group2 :: PP2 a -> PP2 a
group2 (PP2 f cs) = PP2 f $ pure (\w -> breakN w cs)

breakN :: Width -> Seq (Width -> PP2 a) -> PP2 a
breakN w cs = foldMap ($ w) cs

fit :: Width -> PP2 a -> (Delta, Seq a)
fit w (PP2 p@(Delta y x, s) _) | x <= w = p
fit w (PP2 _ cs) = fitN w cs

fitN :: Width -> Seq (Width -> PP2 a) -> (Delta, Seq a)
fitN avail cs = case LL.uncons cs of
  Nothing -> (mempty, mempty)
  Just (hd, tl) -> go (hd avail) where
    -- todo add a same check to prevent infinite loop if there's unbreakable
    -- literal greater than width
    go (PP2 p@(Delta _ x, _) _) | x <= avail =
      p <> fitN (avail - x) tl
    go (PP2 _ bs) = go (breakN avail bs)

render2 :: Monoid a => Width -> PP2 a -> a
render2 width pp = foldMap id . snd $ fit width pp

-- render :: (LL.ListLike a Char) => Int -> PrettyPrint a -> a
-- breakable2 :: IsString s => woot


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
  -- Same as Group, except it will always be rendered broken by renderBroken.  Used for let bindings.
  | BrokenGroup (PrettyPrint a)

-- What mode is this call to renderBroken using?
data BreakMode
  = Normal
  -- Line breaking is has been forced on by a BrokenGroup. (Another Group can return it to normal.)
  | Forced deriving (Eq)

containsForcedBreaks :: LL.ListLike a b => PrettyPrint a -> Bool
containsForcedBreaks = \case
  Empty -> False
  Literal _ -> False
  Append a b -> (containsForcedBreaks a) || (containsForcedBreaks b)
  Nest _prefix a -> containsForcedBreaks a
  Breakable _ -> False
  Group a -> containsForcedBreaks a
  BrokenGroup _ -> True

unbrokenWidth :: LL.ListLike a b => PrettyPrint a -> Int
unbrokenWidth = \case
  Empty -> 0
  Literal a -> LL.length a
  Append a b -> unbrokenWidth a + unbrokenWidth b
  Nest _prefix a -> unbrokenWidth a
  Breakable a -> LL.length a
  Group a -> unbrokenWidth a
  BrokenGroup a -> unbrokenWidth a

-- renderUnbroken produces output that fails the parser, in the following case.
-- * Let and Let Rec - these are rendered with "; " between bindings, which the parser does not accept.
renderUnbroken :: Monoid a => PrettyPrint a -> a
renderUnbroken = \case
  Empty -> mempty
  Literal a -> a
  Append a b -> renderUnbroken a <> renderUnbroken b
  Nest _prefix a -> renderUnbroken a
  Breakable delim -> delim
  Group a -> renderUnbroken a
  BrokenGroup a -> renderUnbroken a

-- Render a `PrettyPrint a` into a rectangular window of width `width` characters.
-- `leading` characters of the first line have already been used (can be > width).
-- `start` is True if this is at the start of the outer-most term being printed.
renderBroken :: forall a b. (LL.ListLike a b, Eq b)
             => BreakMode -> Bool -> Int -> Int -> b -> PrettyPrint a -> a
renderBroken breakMode start width leading lineSeparator = \case
  Empty -> LL.empty
  Literal a -> a
  Append a b ->
    let ra = renderBroken breakMode False width leading lineSeparator a
        trailing = lengthOfLastLine lineSeparator ra
    in ra <> renderBroken breakMode False width trailing lineSeparator b
  Nest prefix a ->
    if ((leading == 0) && (not start))
    then
      -- Indent the subtree.
      let ra = renderBroken breakMode False (width - LL.length prefix) 0 lineSeparator a
      in prefix <> replaceOneWithMany lineSeparator (LL.cons lineSeparator prefix) ra
    else renderBroken breakMode False width leading lineSeparator a
  Breakable _delim -> LL.singleton lineSeparator
  -- Going inside a Group can allow us to revert to unbroken rendering.
  Group a       -> render' Normal False width leading lineSeparator a
  BrokenGroup a -> render' Forced False width leading lineSeparator a

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
render width doc = render' Normal True width 0 '\n' doc

-- Render broken only if necessary.
render' :: (LL.ListLike a b, Eq b) => BreakMode -> Bool -> Int -> Int -> b -> PrettyPrint a -> a
render' breakMode start width leading lineSeparator doc =
  if (breakMode /= Forced) && (not $ containsForcedBreaks doc) && (unbrokenWidth doc <= width - leading)
  then renderUnbroken doc
  else renderBroken breakMode start width leading lineSeparator doc

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

lines :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
lines ps = brokenGroup $ softbreaks ps

linesSpaced :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
linesSpaced ps = brokenGroup $ intercalateMap (softbreak <> softbreak) id ps

group :: PrettyPrint a -> PrettyPrint a
group p = Group p

brokenGroup :: PrettyPrint a -> PrettyPrint a
brokenGroup p = BrokenGroup p

breakable :: IsString a => a -> PrettyPrint a
breakable = Breakable

padTo :: (IsString a, LL.ListLike a b) => Int -> PrettyPrint a -> PrettyPrint a
padTo n p =
  let rem = n - unbrokenWidth p
  in if rem > 0 then p <> (fromString (replicate rem ' '))
     else p

column2 :: (IsString a, LL.ListLike a b) => [(PrettyPrint a, PrettyPrint a)] -> PrettyPrint a
column2 rows = lines (group <$> alignedRows) where
  maxWidth = foldl' max 0 (unbrokenWidth . fst <$> rows) + 1
  alignedRows = [ padTo maxWidth col0 <> col1 | (col0, col1) <- rows ]

text :: IsString a => Text -> PrettyPrint a
text t = fromString (Text.unpack t)

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
    BrokenGroup a -> "BrokenGroup (" ++ (show a) ++ ")"
