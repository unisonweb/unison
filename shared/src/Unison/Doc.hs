-- |
-- A combinator library for building responsive layouts.
-- Like a prettyprinting library, a single `Doc` may be laid out at multiple
-- widths in a way that respects constraints on linebreaks set by the
-- programmer. But additionally, all nodes of the produced `Doc` are
-- annotated with a 'path' and we can quickly lookup the rectangular region
-- corresponding to a path, or lookup what path corresponds
-- to a given location in the layout.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Unison.Doc where

import Control.Comonad.Cofree (Cofree(..), unwrap) -- (:<)
import Control.Comonad (extract)
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Functor
import Data.List hiding (group)
import Data.String (IsString)
import Data.Text (Text)
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..))
import Unison.Path (Path)
import qualified Data.Text as Text
import qualified Unison.Dimensions as Dimensions
import qualified Unison.Path as Path

data Padded e r =
  Padded { top :: e, bottom :: e, left :: e, right :: e, element :: r }
  deriving (Functor, Foldable, Traversable)

data D e r
  = Empty
  | Embed e
  | Pad (Padded e r)
  | Breakable e
  | Linebreak
  | Group r
  | Nest e r
  | Append r r deriving (Functor, Foldable, Traversable)

-- | A `Doc e p` describes a layout that may be rendered at
-- multiple widths. The `e` parameter is the type of primitive documents,
-- possibly `String` or `Text`. The `p` parameter is the path type,
-- generally a list. Note that the full path corresponding to a
-- subtree in the document is the concatenation of all paths starting
-- from the root.
type Doc e p = Cofree (D e) p

data L e r
  = LEmpty
  | LEmbed e
  | LPad (Padded e r)
  | LLinebreak
  | LNest e r
  | LAppend r r deriving (Functor, Foldable, Traversable)

-- A `Doc` without the nondeterminism. All layout decisions have been fixed.
type Layout e p = Cofree (L e) p

data Direction = Horizontal | Vertical deriving (Eq,Ord)

data B e r
  = BEmpty
  | BEmbed e
  | BFlow Direction [r] deriving (Functor, Foldable, Traversable)

type Boxed e p = Cofree (B e) p

accumulate :: Functor f => (f b -> b) -> Cofree f a -> Cofree f (a,b)
accumulate alg (a :< f) = case fmap (accumulate alg) f of
  f -> (a, alg (fmap (snd . root) f)) :< f

einterpret :: Bifunctor f => (f e e -> e) -> Cofree (f e) p -> e
einterpret alg (_ :< f) = alg $ second (einterpret alg) f

rewrite :: Functor f => (f (Cofree f a) -> f (Cofree f a)) -> Cofree f a -> Cofree f a
rewrite alg (a :< f) = a :< fmap (rewrite alg) f

bounds :: (e -> (Width,Height)) -> Boxed e p -> Boxed e (p, (Width,Height))
bounds dims b = accumulate step b where
  step BEmpty = zero
  step (BEmbed e) = dims e
  step (BFlow Horizontal bs) = foldl' hcombine zero bs
  step (BFlow Vertical bs) = foldl' vcombine zero bs
  hcombine (w1,h1) (w2,h2) = (Dimensions.plus w1 w2, h1 `max` h2)
  vcombine (w1,h1) (w2,h2) = (w1 `max` w2, Dimensions.plus h1 h2)
  zero = (Dimensions.zero, Dimensions.zero)

flatten :: Boxed e p -> Boxed e p
flatten b = rewrite step b where
  step b = case b of
    BEmpty -> b
    BEmbed _ -> b
    BFlow dir bs -> BFlow dir $ bs >>= h where
      h (_ :< BFlow dir2 bsi) | dir == dir2 = bsi
      h x = [x]

breduce :: (a -> a -> a) -> a -> [a] -> a
breduce f z s = done $ foldl' step [] s where
  step !stack a = fixup ((a, 1 :: Int) : stack)
  fixup ((a2,n):(a1,m):tl) | m >= n = fixup ((f a1 a2, n+m) : tl)
  fixup stack = stack
  done [] = z
  done stack = foldl1' (\a2 a1 -> f a1 a2) (map fst stack)

boxed :: Path p => Layout e p -> Boxed e p
boxed l = go l [] [] [] where

  empty = Path.root :< BEmpty
  line hbuf = breduce beside empty (reverse hbuf)
  above = combine $ \b1 b2 -> BFlow Horizontal [b1,b2]
  beside = combine $ \b1 b2 -> BFlow Vertical [b1,b2]
  combine f (p :< b) (p2 :< b2) = case Path.factor p p2 of
    (root, (p,p2)) -> root :< f (p :< b) (p2 :< b2)

  bembed e = Path.root :< BEmbed e
  advance hbuf vbuf todo = go (Path.root :< LEmpty) hbuf vbuf todo
  go (p :< l) hbuf vbuf todo = case l of
    LEmpty -> case todo of
      [] -> breduce above empty (reverse $ line hbuf : vbuf)
      hd:todo -> go hd hbuf vbuf todo
    LEmbed e -> advance vbuf ((p :< BEmbed e) : hbuf) todo
    LNest e r -> let inner = p :< BFlow Horizontal [Path.root :< BEmbed e, boxed r]
                 in advance (inner:hbuf) vbuf todo
    LAppend a b -> go a hbuf vbuf (b:todo)
    LPad (Padded top bot l r e) -> advance (inner : hbuf) vbuf todo where
      inner = p :< BFlow Horizontal
        [ bembed l
        , Path.root :< BFlow Vertical [bembed top, boxed e, bembed bot]
        , bembed r ]
    LLinebreak | null hbuf -> advance hbuf vbuf todo
    LLinebreak -> advance [] (line hbuf : vbuf) todo

boundedBoxes :: Path p => (e -> (Width,Height)) -> Layout e p -> Boxed e (p, (Width,Height))
boundedBoxes dims l = bounds dims (boxed l)

-- | Compute the list of path segments corresponding to the given point.
-- Concatenating the full list of segments gives the deepest path into the
-- structure whose layout region contains the point. Concatenating all but the
-- last segment yields the parent of the deepest path, and so on.
--
-- The point (X 0, Y 0) is assumed to correspond to the top left
-- corner of the layout.
at :: Boxed e (p, (Width,Height)) -> (X,Y) -> [p]
at box = go box []
  where
  fw acc ((_,(w,_)) :< _) = Dimensions.plus acc w
  fh acc ((_,(_,h)) :< _) = Dimensions.plus acc h
  accBounds f bs = let acc = scanl' f Dimensions.zero bs in acc `zip` (tail acc)
  within (X x, Y y) (w,h) = Width x <= w && Height y <= h
  go ((p,region@(w,h)) :< box) stack pt@(x,y) =
    if not (pt `within` region) then []
    else p : case box of
      BEmpty -> []
      BEmbed _ -> []
      BFlow Horizontal bs ->
        take 1 [ (b,bw) | (b,(bw,aw)) <- bs `zip` accBounds fw bs, within pt (aw,h) ]
        >>= \(b,Width w) -> go b stack (Dimensions.minus x (X w), y)
      BFlow Vertical bs ->
        take 1 [ (b,bh) | (b,(bh,ah)) <- bs `zip` accBounds fh bs, within pt (w,ah) ]
        >>= \(b,Height h) -> go b stack (x, Dimensions.minus y (Y h))

regions :: Boxed e (p, (Width,Height)) -> [p] -> [(X,Y,Width,Height)]
regions box p = error "todo"

-- | Produce a `Layout` which tries to fit in the given width,
-- assuming that embedded `e` elements have the computed width.
-- Runs in linear time without backtracking.
layout :: (e -> Width) -> Width -> Doc e p -> Layout e p
layout width maxWidth doc =
  fmap fst $ evalState (go (preferredWidth width doc)) (maxWidth, maxWidth)
  where
  go doc = do
    (maxWidth, remainingWidth) <- get
    case doc of
      (_,w) :< _ | w <= remainingWidth ->
        put (maxWidth, remainingWidth `Dimensions.minus` w) $> flow doc
      _ :< Group doc -> break doc
      _ -> break doc

  -- | Break a document into a list of documents, separated by lines,
  -- respecting the linebreak constraints of the input `Doc`.
  break (p :< doc) = get >>= \(maxWidth, remainingWidth) -> case doc of
    Empty -> pure $ p :< LEmpty
    Embed e -> put (maxWidth, remainingWidth `Dimensions.minus` width e) $> (p :< LEmbed e)
    Breakable _ -> put (maxWidth, maxWidth) $> (p :< LLinebreak)
    Linebreak -> put (maxWidth, maxWidth) $> (p :< LLinebreak)
    Append a b -> (:<) p <$> (LAppend <$> break a <*> break b)
    Pad padded ->
      let borderWidth = width (left padded) `Dimensions.plus` width (right padded)
      in do
        put (maxWidth `Dimensions.minus` borderWidth, remainingWidth `Dimensions.minus` borderWidth)
        inner <- break (element padded)
        modify (\(_, remainingWidth) -> (maxWidth, remainingWidth `Dimensions.minus` borderWidth))
        return $ p :< LPad (padded { element = inner })
    Nest e doc -> do
      case maxWidth == remainingWidth of
        -- we're immediately preceded by newline, insert `e` and indent
        True -> do
          put $ let newMax = maxWidth `Dimensions.minus` width e in (newMax, newMax)
          doc <- break doc
          return $ p :< LNest e doc
        -- we're in the middle of a line, ignore `e`
        False -> break doc
    Group doc -> go doc -- we try to avoid breaking subgroups

-- | Layout the `Doc` assuming infinite available width
flow :: Doc e p -> Layout e p
flow (p :< doc) = case doc of
  Empty -> p :< LEmpty
  Embed e -> p :< LEmbed e
  Pad padded -> p :< LPad (padded { element = flow (element padded) })
  Linebreak -> p :< LLinebreak
  Breakable e -> p :< LEmbed e -- don't linebreak, it fits
  Append a b -> p :< (flow a `LAppend` flow b)
  Group r -> flow r
  Nest _ r -> flow r

-- | Annotate the document with the preferred width of each subtree,
-- assuming that embedded elements have the given width function.
preferredWidth :: (e -> Width) -> Doc e p -> Doc e (p,Width)
preferredWidth width (p :< d) = case d of
  Empty -> (p, Dimensions.zero) :< Empty
  Embed e -> (p, width e) :< Embed e
  Pad padded ->
    let borderWidth = width (left padded) `Dimensions.plus` width (right padded)
        inner = preferredWidth width (element padded)
        innerWidth = snd (root inner)
    in (p, borderWidth `Dimensions.plus` innerWidth) :< Pad (padded { element = inner })
  -- Since we just use this to decide whether to break or not,
  -- as long as `flow` and `break` both interpret `Linebreak` properly,
  -- a zero width for linebreaks is okay
  Linebreak -> (p, Dimensions.zero) :< Linebreak
  Breakable e -> (p, width e) :< Breakable e -- assuming we fit on the line
  Append left right ->
    let left' = preferredWidth width left
        right' = preferredWidth width right
    in (p, snd (extract left') `Dimensions.plus` snd (extract right')) :< Append left' right'
  Group d ->
    let pd@((_,n) :< _) = preferredWidth width d
    in (p, n) :< Group pd
  Nest e d -> -- assume it fits, so ignore the `e`
    let pd@((_,n) :< _) = preferredWidth width d
    in (p,n) :< Nest e pd

-- | The root path of this document
root :: Cofree f p -> p
root (p :< _) = p

-- | The embedded elements of this document
elements :: Doc e p -> [e]
elements d = go (unwrap d) [] where
  one a = (a:)
  many xs tl = foldr (:) tl xs
  go (Append d1 d2) = go (unwrap d1) . go (unwrap d2)
  go (Group d) = go (unwrap d)
  go (Nest e d) = one e . go (unwrap d)
  go (Breakable e) = one e
  go (Embed e) = one e
  go (Pad (Padded t b l r inner)) = many [t, b, l, r] . go (unwrap inner)
  go _ = id

-- | Map over all `e` elements in this `Doc e p`.
etraverse :: Applicative f => (e -> f e2) -> Doc e p -> f (Doc e2 p)
etraverse f (p :< d) = (p :<) <$> case d of
  Append d1 d2 -> Append <$> etraverse f d1 <*> etraverse f d2
  Group d -> Group <$> etraverse f d
  Nest e d -> Nest <$> f e <*> etraverse f d
  Breakable e -> Breakable <$> f e
  Embed e -> Embed <$> f e
  Pad (Padded t b l r inner) -> Pad <$> (Padded <$> f t <*> f b <*> f l <*> f r <*> etraverse f inner)
  Linebreak -> pure Linebreak
  Empty -> pure Empty

-- | Map over all `e` elements in this `Doc e p`.
emap :: Bifunctor f => (e -> e2) -> Cofree (f e) a -> Cofree (f e2) a
emap f (p :< r) = p :< first f (second (emap f) r)

-- | Substitute all `e` elements in this `Doc e p`. The
-- function must return an `embed e2` when targeting elements
-- embedded in a `nest` or `pad`, otherwise the substitution fails
-- with `Nothing`.
ebind :: (e -> Doc e2 p) -> Doc e p -> Maybe (Doc e2 p)
ebind f (p :< d) = case d of
  Embed e -> Just (f e)
  d -> (p :<) <$> case d of
    Embed _ -> error "GHC can't figure out this is not possible"
    Append d1 d2 -> Append <$> ebind f d1 <*> ebind f d2
    Group d -> Group <$> ebind f d
    Nest e d -> Nest <$> e2 e <*> ebind f d
    Breakable e -> Breakable <$> e2 e
    Pad (Padded t b l r inner) -> Pad <$> (Padded <$> e2 t <*> e2 b <*> e2 l <*> e2 r <*> ebind f inner)
    Linebreak -> Just Linebreak
    Empty -> Just Empty
    where
    e2 e = case unwrap (f e) of
      Embed e2 -> Just e2
      _ -> Nothing

-- | The empty document
empty :: Path p => Doc e p
empty = Path.root :< Empty

-- | Append two documents
append :: Path p => Doc e p -> Doc e p -> Doc e p
append (p1 :< d1) (p2 :< d2) =
  case Path.factor p1 p2 of
    (lca, (p1, p2)) -> lca :< ((p1 :< d1) `Append` (p2 :< d2))

-- | Replace the path component of this `Doc`
reroot :: p -> Cofree f p -> Cofree f p
reroot p (_ :< d) = p :< d

-- | Cons `hd` onto the path of this `Doc`
sub :: e -> Cofree f [e] -> Cofree f [e]
sub hd (tl :< d) = (hd : tl) :< d

-- | Append `hd` onto the path of this `Doc`
sub' :: [e] -> Cofree f [e] -> Cofree f [e]
sub' hd (tl :< d) = (hd ++ tl) :< d

-- | Make a `Doc` from a token and give it an empty path
embed :: Path p => e -> Doc e p
embed e = Path.root :< Embed e

-- | Make a `Doc` from a token and give it the specified path
embed' :: p -> e -> Doc e p
embed' p e = p :< Embed e

-- | Wrap this `Doc` in a group, which constrains all layout
-- choices in the group to be the same. For instance,
-- `group $ embed "a" <> line " " <> embed "b" <> line " " <> embed "c"`
-- will layout as either "a b c" OR as both spaces replaced by newlines.
group :: Path p => Doc e p -> Doc e p
group (p :< d) = p :< Group (Path.root :< d)

-- | If immediately preceded by a newline, indent the `Doc` by the given element
-- otherwise ignore the `e` argument.
nest :: Path p => e -> Doc e p -> Doc e p
nest e (p :< d) = p :< Nest e (Path.root :< d)

-- | Specify that layout may insert a line break at this point in the document.
-- If a line break is not inserted, the given `e` is inserted instead.
breakable :: Path p => e -> Doc e p
breakable e = breakable' Path.root e

-- | Like `breakable`, but supply a path to attach to the returned `Doc`.
breakable' :: Path p => p -> e -> Doc e p
breakable' p e = p :< Breakable e

-- | Insert a linebreak. Unlike `breakable`, this guarantees we insert
-- a linebreak at the location in the layout where this `Doc` appears,
-- whereas `breakable` just specifies that a linebreak _may_ be inserted.
linebreak :: Path p => Doc e p
linebreak = linebreak' Path.root

-- | Like `linebreak`, but supply a path to attach to the returned `Doc`.
linebreak' :: Path p => p -> Doc e p
linebreak' p = p :< Linebreak

-- | Convert a layout to a list of tokens, using `newline` where the layout
-- calls for a linebreak.
tokens :: e -> Layout e p -> [e]
tokens newline l = finish (execState (go l) ([],[],True))
  where
  finish (_, buf, _) = reverse buf
  col3 p top mid bot = p :< (LAppend (p :< (LAppend (p :< LEmbed top) mid)) (p :< LEmbed bot))
  row3 p left mid right = p :< (LAppend (p :< (LAppend (p :< LEmbed left) mid)) (p :< LEmbed right))
  -- state is (indentation snoc list, token buffer snoc list, whether immediately preceded by newline)
  -- go :: Layout e p -> State ([e],[e],Bool) ()
  go (p :< l) = case l of
    LEmpty -> return ()
    LLinebreak -> modify cr where
      cr (indent, buf, _) = (indent, newline : buf, True)
    LEmbed e -> modify g where
      -- we indent if we're the first token on this line
      g (indent, buf, True) = (indent, e : (indent ++ buf), False)
      g (indent, buf, _) = (indent, e : buf, False)
    LPad padded -> modify g where
      inner = tokens newline
        (row3 p (left padded)
                (col3 p (top padded) (element padded) (bottom padded))
                (right padded))
      -- we indent if we're the first token on this line
      g (indent, buf, True) = (indent, reverse inner ++ (indent ++ buf), False)
      g (indent, buf, _) = (indent, reverse inner ++ buf, False)
    LNest e r -> do
      modify (\(i,b,fst) -> (e : i, b, fst))
      go r
      modify (\(i,b,fst) -> (drop 1 i, b, fst))
    LAppend a b -> go a *> go b

renderString :: Layout String p -> String
renderString l = concat (tokens "\n" l)

formatString :: Width -> Doc String p -> String
formatString availableWidth d = renderString (layout (Width . fromIntegral . length) availableWidth d)

formatText :: Width -> Doc Text p -> String
formatText availableWidth d =
  formatString availableWidth (emap Text.unpack d)

docs :: Path p => [Doc e p] -> Doc e p
docs [] = empty
docs ds = foldr1 append ds

embeds :: Path p => [e] -> Doc e p
embeds = docs . map embed

delimit :: Path p => Doc e p -> [Doc e p] -> Doc e p
delimit d = docs . intersperse d

sep :: Path p => e -> [Doc e p] -> Doc e p
sep _ [] = empty
sep delim ds = group (foldr1 combine ds)
  where combine a b = a `append` breakable delim `append` b

sep' :: Path p => e -> [e] -> Doc e p
sep' delim ds = sep delim (map embed ds)

parenthesize :: (IsString s, Path p) => Bool -> Doc s p -> Doc s p
parenthesize b d =
  let r = root d
  in if b then docs [embed' r "(", d, embed' r ")"] else d

-- various instances

instance Bifunctor Padded where
  second = fmap
  first f (Padded e1 e2 e3 e4 r) = Padded (f e1) (f e2) (f e3) (f e4) r

instance Bifunctor B where
  second = fmap
  first f b = case b of
    BEmpty -> BEmpty
    BEmbed e -> BEmbed (f e)
    BFlow dir bs -> BFlow dir bs

instance Bifunctor D where
  second = fmap
  first f d = case d of
    Empty -> Empty
    Embed e -> Embed (f e)
    Pad p -> Pad (first f p)
    Breakable e -> Breakable (f e)
    Linebreak -> Linebreak
    Group r -> Group r
    Nest e r -> Nest (f e) r
    Append r r2 -> Append r r2
