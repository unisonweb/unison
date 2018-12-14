{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}

module Unison.Util.Pretty (
   Pretty,
   bulleted,
   -- breakable
   column2,
   commas,
   oxfordCommas,
   dashed,
   flatMap,
   group,
   hang',
   hang,
   hangUngrouped',
   hangUngrouped,
   indent,
   indentAfterNewline,
   indentN,
   indentNAfterNewline,
   leftPad,
   lines,
   linesSpaced,
   lit,
   map,
   nest,
   newline,
   numbered,
   orElse,
   orElses,
   parenthesize,
   parenthesizeCommas,
   parenthesizeIf,
   preferredWidth,
   preferredHeight,
   render,
   renderUnbroken,
   rightPad,
   sep,
   sepSpaced,
   softbreak,
   spaceIfBreak,
   spacesIfBreak,
   spaced,
   spacedMap,
   surroundCommas,
   text,
   toANSI,
   toPlain,
   wrap,
   wrapString,
   black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold
  ) where

import           Data.Char                      ( isSpace )
import           Data.Foldable                  ( toList )
import           Data.List                      ( foldl' , foldr1, intersperse )
import           Data.Sequence                  ( Seq )
import           Data.String                    ( IsString , fromString )
import           Data.Text                      ( Text )
import           Prelude                 hiding ( lines , map )
import qualified Unison.Util.ColorText         as CT
import           Unison.Util.Monoid             ( intercalateMap )
import qualified Data.ListLike                 as LL
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as Text

type Width = Int

data Pretty s = Pretty { delta :: Delta, out :: F s (Pretty s) }

instance Functor Pretty where
  fmap f (Pretty d o) = Pretty d (mapLit f $ fmap (fmap f) o)

data F s r
  = Empty | Group r | Lit s | Wrap (Seq r) | OrElse r r | Append (Seq r)
  deriving (Show, Foldable, Traversable, Functor)

mapLit :: (s -> t) -> F s r -> F t r
mapLit f (Lit s) = Lit (f s)
mapLit _ Empty = Empty
mapLit _ (Group r) = Group r
mapLit _ (Wrap s) = Wrap s
mapLit _ (OrElse r s) = OrElse r s
mapLit _ (Append s) = Append s

lit :: (IsString s, LL.ListLike s Char) => s -> Pretty s
lit s = lit' (foldMap chDelta $ LL.toList s) s

lit' :: Delta -> s -> Pretty s
lit' d s = Pretty d (Lit s)

orElse :: Pretty s -> Pretty s -> Pretty s
orElse p1 p2 = Pretty (delta p1) (OrElse p1 p2)

orElses :: [Pretty s] -> Pretty s
orElses [] = mempty
orElses ps = foldr1 orElse ps

wrapImpl :: IsString s => [Pretty s] -> Pretty s
wrapImpl [] = mempty
wrapImpl (p:ps) = wrap_ . Seq.fromList $
  p : fmap (\p -> (" " <> p) `orElse` (newline <> p)) ps

wrapString :: (LL.ListLike s Char, IsString s) => String -> Pretty s
wrapString s = wrap (lit $ fromString s)

wrap :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s
wrap p = wrapImpl (toLeaves [p]) where
  toLeaves [] = []
  toLeaves (hd:tl) = case out hd of
    Empty -> toLeaves tl
    Lit s -> wordify s ++ toLeaves tl
    Group _ -> hd : toLeaves tl
    OrElse a _ -> toLeaves (a:tl)
    Wrap _ -> hd : toLeaves tl
    Append hds -> toLeaves (toList hds ++ tl)
  wordify s0 = let s = LL.dropWhile isSpace s0 in
    if LL.null s then []
    else case LL.break isSpace s of (word1, s) -> lit word1 : wordify s

wrap_ :: Seq (Pretty s) -> Pretty s
wrap_ ps = Pretty (foldMap delta ps) (Wrap ps)

group :: Pretty s -> Pretty s
group p = Pretty (delta p) (Group p)

toANSI :: Width -> Pretty CT.ColorText -> String
toANSI avail p = CT.toANSI (render avail p)

toPlain :: Width -> Pretty CT.ColorText -> String
toPlain avail p = CT.toPlain (render avail p)

renderUnbroken :: (Monoid s, IsString s) => Pretty s -> s
renderUnbroken = render maxBound

render :: (Monoid s, IsString s) => Width -> Pretty s -> s
render availableWidth p = go mempty [Right p] where
  go _   []       = mempty
  go cur (p:rest) = case p of
    Right p -> -- `p` might fit, let's try it!
      if p `fits` cur then flow p <> go (cur <> delta p) rest
      else go cur (Left p : rest) -- nope, switch to breaking mode
    Left p -> case out p of -- `p` requires breaking
      Append ps  -> go cur ((Left <$> toList ps) <> rest)
      Empty      -> go cur rest
      Group p    -> go cur (Right p : rest)
      -- Note: literals can't be broken further so they're
      -- added to output unconditionally
      Lit l      -> l <> go (cur <> delta p) rest
      OrElse _ p -> go cur (Right p : rest)
      Wrap ps    -> go cur ((Right <$> toList ps) <> rest)

  flow p = case out p of
    Append ps -> foldMap flow ps
    Empty -> mempty
    Group p -> flow p
    Lit s -> s
    OrElse p _ -> flow p
    Wrap ps -> foldMap flow ps

  fits p cur =
    let cur' = cur { maxCol = col cur }
    in maxCol (cur' <> delta p) < availableWidth

newline :: IsString s => Pretty s
newline = lit' (chDelta '\n') (fromString "\n")

spaceIfBreak :: IsString s => Pretty s
spaceIfBreak = "" `orElse` " "

spacesIfBreak :: IsString s => Int -> Pretty s
spacesIfBreak n = "" `orElse` (fromString $ replicate n ' ')

softbreak :: IsString s => Pretty s
softbreak = " " `orElse` newline

spaced :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
spaced = intercalateMap softbreak id

spacedMap :: (Foldable f, IsString s) => (a -> Pretty s) -> f a -> Pretty s
spacedMap f as = spaced . fmap f $ toList as

commas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
commas = intercalateMap ("," <> softbreak) id

oxfordCommas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
oxfordCommas xs = case toList xs of
  []     -> ""
  [x]    -> x
  [x, y] -> x <> " and " <> y
  xs ->
    intercalateMap ("," <> softbreak) id (init xs)
      <> ","
      <> softbreak
      <> "and"
      <> softbreak
      <> last xs

parenthesizeCommas :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
parenthesizeCommas = surroundCommas "(" ")"

surroundCommas :: (Foldable f, IsString s) => Pretty s -> Pretty s -> f (Pretty s) -> Pretty s
surroundCommas start stop fs = group $
  start <> spaceIfBreak
        <> intercalateMap ("," <> softbreak <> align) id fs
        <> stop
  where align = spacesIfBreak (preferredWidth start + 1)

sepSpaced :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
sepSpaced between = sep (between <> softbreak)

sep :: (Foldable f, IsString s) => Pretty s -> f (Pretty s) -> Pretty s
sep between = intercalateMap between id

parenthesize :: IsString s => Pretty s -> Pretty s
parenthesize p = group $ "(" <> p <> ")"

parenthesizeIf :: IsString s => Bool -> Pretty s -> Pretty s
parenthesizeIf False s = s
parenthesizeIf True s = parenthesize s

lines :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
lines = intercalateMap newline id

linesSpaced :: (Foldable f, IsString s) => f (Pretty s) -> Pretty s
linesSpaced ps = lines (intersperse "" $ toList ps)

bulleted :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty s) -> Pretty s
bulleted = intercalateMap newline (\b -> "* " <> indentAfterNewline "  " b)

dashed :: (Foldable f, LL.ListLike s Char, IsString s) => f (Pretty s) -> Pretty s
dashed = intercalateMap newline (\b -> "- " <> indentAfterNewline "  " b)

numbered :: (Foldable f, LL.ListLike s Char, IsString s) => (Int -> Pretty s) -> f (Pretty s) -> Pretty s
numbered num ps = column2 (fmap num [1..] `zip` toList ps)

leftPad, rightPad :: IsString s => Int -> Pretty s -> Pretty s
leftPad n p =
  let rem = n - preferredWidth p
  in if rem > 0 then fromString (replicate rem ' ') <> p
     else p
rightPad n p =
  let rem = n - preferredWidth p
  in if rem > 0 then p <> fromString (replicate rem ' ')
     else p

column2 :: (LL.ListLike s Char, IsString s) => [(Pretty s, Pretty s)] -> Pretty s
column2 rows = lines (group <$> alignedRows) where
  maxWidth = foldl' max 0 (preferredWidth . fst <$> rows) + 1
  alignedRows = [ rightPad maxWidth col0 <> indentNAfterNewline maxWidth col1
                | (col0, col1) <- rows ]

text :: IsString s => Text -> Pretty s
text t = fromString (Text.unpack t)

hang' :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s -> Pretty s
hang' from by p = group $
  if preferredHeight p > 0 then from <> "\n" <> group (indent by p)
  else (from <> " " <> group p) `orElse`
       (from <> "\n" <> group (indent by p))

hangUngrouped' :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s -> Pretty s
hangUngrouped' from by p =
  if preferredHeight p > 0 then from <> "\n" <> indent by p
  else (from <> " " <> p) `orElse`
       (from <> "\n" <> indent by p)

hangUngrouped :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
hangUngrouped from p = hangUngrouped' from "  " p

hang :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
hang from p = hang' from "  " p

nest :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
nest by = hang' "" by

indent :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
indent by p = by <> indentAfterNewline by p

indentN :: (LL.ListLike s Char, IsString s) => Width -> Pretty s -> Pretty s
indentN by = indent (fromString $ replicate by ' ')

indentNAfterNewline :: (LL.ListLike s Char, IsString s) => Width -> Pretty s -> Pretty s
indentNAfterNewline by = indentAfterNewline (fromString $ replicate by ' ')

indentAfterNewline :: (LL.ListLike s Char, IsString s) => Pretty s -> Pretty s -> Pretty s
indentAfterNewline by p = flatMap f p where
  f s0 = case LL.break (== '\n') s0 of
    (hd, s) -> if LL.null s then lit s0
               -- use `take` and `drop` to preserve annotations or
               -- or other extra info attached to the original `s`
               else lit (LL.take (LL.length hd) s0) <>
                    "\n" <> by <> f (LL.drop 1 s)

instance IsString s => IsString (Pretty s) where
  fromString s = lit' (foldMap chDelta s) (fromString s)

instance Semigroup (Pretty s) where (<>) = mappend
instance Monoid (Pretty s) where
  mempty = Pretty mempty Empty
  mappend p1 p2 = Pretty (delta p1 <> delta p2) .
    Append $ case (out p1, out p2) of
      (Append ps1, Append ps2) -> ps1 <> ps2
      (Append ps1, _) -> ps1 <> pure p2
      (_, Append ps2) -> pure p1 <> ps2
      (_,_) -> pure p1 <> pure p2

data Delta =
  Delta { line :: !Int, col :: !Int, maxCol :: !Int }
  deriving (Eq,Ord,Show)

instance Semigroup Delta where (<>) = mappend
instance Monoid Delta where
  mempty = Delta 0 0 0
  mappend (Delta l c mc) (Delta 0 c2 mc2) = Delta l (c + c2) (mc `max` mc2 `max` (c+c2))
  mappend (Delta l _ mc) (Delta l2 c2 mc2) = Delta (l + l2) c2 (mc `max` mc2)

chDelta :: Char -> Delta
chDelta '\n' = Delta 1 0 0
chDelta _ = Delta 0 1 1

preferredWidth :: Pretty s -> Width
preferredWidth p = col (delta p)

preferredHeight :: Pretty s -> Width
preferredHeight p = line (delta p)

black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen,
  hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold :: Pretty CT.ColorText -> Pretty CT.ColorText
black = map CT.black
red = map CT.red
green = map CT.green
yellow = map CT.yellow
blue = map CT.blue
purple = map CT.purple
cyan = map CT.cyan
white = map CT.white
hiBlack = map CT.hiBlack
hiRed = map CT.hiRed
hiGreen = map CT.hiGreen
hiYellow = map CT.hiYellow
hiBlue = map CT.hiBlue
hiPurple = map CT.hiPurple
hiCyan = map CT.hiCyan
hiWhite = map CT.hiWhite
bold = map CT.bold

instance Show s => Show (Pretty s) where
  show p = render 80 (metaPretty p)

metaPretty :: Show s => Pretty s -> Pretty String
metaPretty p = go (0::Int) p where
  go prec p = case out p of
    Lit s -> parenthesizeIf (prec > 0) $ "Lit" `hang` lit (show s)
    Empty -> "Empty"
    Group g -> parenthesizeIf (prec > 0) $ "Group" `hang` go 1 g
    Wrap s -> parenthesizeIf (prec > 0) $ "Wrap" `hang`
      surroundCommas "[" "]" (go 1 <$> s)
    OrElse a b -> parenthesizeIf (prec > 0) $
      "OrElse" `hang` spaced [go 1 a, go 1 b]
    Append s -> surroundCommas "[" "]" (go 1 <$> s)

map :: LL.ListLike s2 Char => (s -> s2) -> Pretty s -> Pretty s2
map f p = case out p of
  Append ps -> foldMap (map f) ps
  Empty -> mempty
  Group p -> group (map f p)
  Lit s -> lit' (foldMap chDelta $ LL.toList s2) s2 where s2 = f s
  OrElse p1 p2 -> orElse (map f p1) (map f p2)
  Wrap p -> wrap_ (map f <$> p)

flatMap :: (s -> Pretty s2) -> Pretty s -> Pretty s2
flatMap f p = case out p of
  Append ps -> foldMap (flatMap f) ps
  Empty -> mempty
  Group p -> group (flatMap f p)
  Lit s -> f s
  OrElse p1 p2 -> orElse (flatMap f p1) (flatMap f p2)
  Wrap p -> wrap_ (flatMap f <$> p)
