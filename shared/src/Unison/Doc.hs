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
import Data.Foldable
import Data.List hiding (group)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..), Region)
import Unison.Path (Path)
import qualified Data.Text as Text
import qualified Unison.Dimensions as Dimensions
import qualified Unison.Path as Path

data D e r
  = Empty
  | Embed e
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
  | LLinebreak
  | LNest e r
  | LAppend r r
  | LGroup r deriving (Functor, Foldable, Traversable)

-- A `Doc` without the nondeterminism. All layout decisions have been fixed.
type Layout e p = Cofree (L e) p

data Direction = Horizontal | Vertical deriving (Eq,Ord)

data B e r
  = BEmpty
  | BEmbed e
  | BFlow Direction [r] deriving (Functor, Foldable, Traversable)

type Box e p = Cofree (B e) p

-- | The root path of this document
root :: Cofree f p -> p
root (p :< _) = p

-- | The embedded elements of this document
elements :: Doc e p -> [e]
elements d = go (unwrap d) [] where
  one a = (a:)
  go (Append d1 d2) = go (unwrap d1) . go (unwrap d2)
  go (Group d) = go (unwrap d)
  go (Nest e d) = one e . go (unwrap d)
  go (Breakable e) = one e
  go (Embed e) = one e
  go _ = id

-- | Map over all `e` elements in this `Doc e p`.
etraverse :: Applicative f => (e -> f e2) -> Doc e p -> f (Doc e2 p)
etraverse f (p :< d) = (p :<) <$> case d of
  Append d1 d2 -> Append <$> etraverse f d1 <*> etraverse f d2
  Group d -> Group <$> etraverse f d
  Nest e d -> Nest <$> f e <*> etraverse f d
  Breakable e -> Breakable <$> f e
  Embed e -> Embed <$> f e
  Linebreak -> pure Linebreak
  Empty -> pure Empty

-- | Map over all `e` elements in this `Doc e p`.
emap :: Bifunctor f => (e -> e2) -> Cofree (f e) a -> Cofree (f e2) a
emap f (p :< r) = p :< first f (second (emap f) r)

-- | Substitute all `e` elements in this `Doc e p`. The
-- function must return an `embed e2` when targeting elements
-- embedded in a `nest` or `pad`, otherwise the substitution fails
-- with `Nothing`.
ebind :: Path p => (e -> Doc e2 p) -> Doc e p -> Maybe (Doc e2 p)
ebind f (p :< d) = case d of
  Embed e -> Just (sub' p $ f e)
  d -> (p :<) <$> case d of
    Embed _ -> error "GHC can't figure out this is not possible"
    Append d1 d2 -> Append <$> ebind f d1 <*> ebind f d2
    Group d -> Group <$> ebind f d
    Nest e d -> Nest <$> e2 e <*> ebind f d
    Breakable e -> Breakable <$> e2 e
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
append d1 (_ :< Empty) = d1
append (_ :< Empty) d2 = d2
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
sub' :: Path p => p -> Cofree f p -> Cofree f p
sub' hd (tl :< d) = (Path.extend hd tl) :< d

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
nest e (p :< d) = Path.root :< Nest e (p :< d)

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

renderString :: Layout String p -> String
renderString l = concat (tokens "\n" l)

stringWidth :: String -> Width
stringWidth = Width . fromIntegral . length

textWidth :: Text -> Width
textWidth = Width . fromIntegral . Text.length

formatString :: Width -> Doc String p -> String
formatString availableWidth d = renderString (layout stringWidth availableWidth d)

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

-- various interpreters

accumulate :: Functor f => (f b -> b) -> Cofree f a -> Cofree f (a,b)
accumulate alg (a :< f) = case fmap (accumulate alg) f of
  f -> (a, alg (fmap (snd . root) f)) :< f

einterpret :: Bifunctor f => (f e e -> e) -> Cofree (f e) p -> e
einterpret alg (_ :< f) = alg $ second (einterpret alg) f

rewrite :: Functor f => (f (Cofree f a) -> f (Cofree f a)) -> Cofree f a -> Cofree f a
rewrite alg (a :< f) = a :< (alg $ fmap (rewrite alg) f)

-- | Produce a `Layout` which tries to fit in the given width,
-- assuming that embedded `e` elements have the computed width.
-- Runs in linear time without backtracking.
layout :: (e -> Width) -> Width -> Doc e p -> Layout e p
layout width maxWidth doc =
  fmap fst $ evalState (go (preferredWidth width doc)) (maxWidth, maxWidth)
  where
  sub p d = p :< LAppend (p :< LEmpty) d
  go doc = do
    (maxWidth, remainingWidth) <- get
    case doc of
      (_,w) :< _ | w <= remainingWidth ->
        put (maxWidth, remainingWidth `Dimensions.minus` w) $> flow doc
      p :< Group doc -> (\d -> p :< LGroup d) <$> break doc
      _ -> break doc

  -- | Break a document into a list of documents, separated by lines,
  -- respecting the linebreak constraints of the input `Doc`.
  break (p :< doc) = get >>= \(maxWidth, remainingWidth) -> case doc of
    Empty -> pure $ p :< LEmpty
    Embed e -> put (maxWidth, remainingWidth `Dimensions.minus` width e) $> (p :< LEmbed e)
    Breakable _ -> put (maxWidth, maxWidth) $> (p :< LLinebreak)
    Linebreak -> put (maxWidth, maxWidth) $> (p :< LLinebreak)
    Append a b -> (:<) p <$> (LAppend <$> break a <*> break b)
    Nest e doc -> do
      case maxWidth == remainingWidth of
        -- we're immediately preceded by newline, insert `e` and indent
        True -> do
          put $ let newMax = maxWidth `Dimensions.minus` width e in (newMax, newMax)
          doc <- break doc
          return $ p :< LNest e doc
        -- we're in the middle of a line, ignore `e`
        False -> sub p <$> break doc
    Group _ -> go (p :< doc) -- we try to avoid breaking subgroups

-- | Layout the `Doc` assuming infinite available width
flow :: Doc e p -> Layout e p
flow (p :< doc) = case doc of
  Empty -> p :< LEmpty
  Embed e -> p :< LEmbed e
  Linebreak -> p :< LLinebreak
  Breakable e -> p :< LEmbed e -- don't linebreak, it fits
  Append a b -> p :< (flow a `LAppend` flow b)
  Group r -> p :< LGroup (flow r)
  Nest _ r -> p :< LAppend (p :< LEmpty) (flow r)

-- | Annotate the document with the preferred width of each subtree,
-- assuming that embedded elements have the given width function.
preferredWidth :: (e -> Width) -> Doc e p -> Doc e (p,Width)
preferredWidth width (p :< d) = case d of
  Empty -> (p, Dimensions.zero) :< Empty
  Embed e -> (p, width e) :< Embed e
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

-- | Convert a layout to a list of tokens, using `newline` where the layout
-- calls for a linebreak.
tokens :: e -> Layout e p -> [e]
tokens newline l = finish (execState (go l) ([],[],True))
  where
  finish (_, buf, _) = reverse buf
  -- state is (indentation snoc list, token buffer snoc list, whether immediately preceded by newline)
  -- go :: Layout e p -> State ([e],[e],Bool) ()
  go (_ :< l) = case l of
    LGroup r -> go r
    LEmpty -> return ()
    LLinebreak -> modify cr where
      cr (indent, buf, _) = (indent, newline : buf, True)
    LEmbed e -> modify g where
      -- we indent if we're the first token on this line
      g (indent, buf, True) = (indent, e : (indent ++ buf), False)
      g (indent, buf, _) = (indent, e : buf, False)
    LNest e r -> do
      modify (\(i,b,fst) -> (e : i, b, fst))
      go r
      modify (\(i,b,fst) -> (drop 1 i, b, fst))
    LAppend a b -> go a *> go b

-- | Convert a `Layout` to a `Box`.
box :: Path p => Layout e p -> Box e p
box l = go l [] [] [] where
  line hbuf = Path.root :< BFlow Horizontal (reverse hbuf)
  advance hbuf vbuf todo = go (Path.root :< LEmpty) hbuf vbuf todo
  go (p :< l) hbuf vbuf todo = case l of
    LEmpty -> case todo of
      [] -> Path.root :< BFlow Vertical (reverse $ line hbuf : vbuf)
      hd:todo -> go hd hbuf vbuf todo
    LEmbed e -> advance ((p :< BEmbed e) : hbuf) vbuf todo
    LGroup r@(_ :< LEmbed _) -> go (sub' p r) hbuf vbuf todo
    -- LGroup r -> advance ((p :< BFlow Horizontal [box r]) : hbuf) vbuf todo
    LGroup r -> advance (sub' p (box r) : hbuf) vbuf todo
    LNest e r ->
      let inner = p :< BFlow Horizontal [Path.root :< BEmbed e, box r]
      in advance (inner:hbuf) vbuf todo
    LAppend a b -> go (sub' p a) hbuf vbuf (sub' p b : todo)
    LLinebreak | null hbuf -> advance hbuf vbuf todo
    LLinebreak -> advance [] (line hbuf : vbuf) todo

-- | Un-nest any `BFlow` elements as much as possible.
flatten :: Box e p -> Box e p
flatten b = rewrite step b where
  step b = case b of
    BEmpty -> b
    BEmbed _ -> b
    BFlow dir bs -> BFlow dir $ bs >>= h where
      h (_ :< BFlow dir2 bsi) | dir == dir2 = bsi
      h (_ :< BFlow _ [bsi]) = [bsi]
      h x = [x]

flatten' :: (Eq p, Path p) => Box e p -> Box e p
flatten' b = rewrite step b where
  step b = case b of
    BEmpty -> b
    BEmbed _ -> b
    BFlow dir bs -> BFlow dir $ bs >>= h where
      h (p :< BFlow dir2 bsi) | dir == dir2 && p == Path.root = bsi
      h x = [x]

-- | Balanced reduction of a list (todo: find better home)
foldb :: (a -> a -> a) -> a -> [a] -> a
foldb f z s = done $ foldl' step [] s where
  step !stack a = fixup ((a, 1 :: Int) : stack)
  fixup ((a2,n):(a1,m):tl) | n >= m = fixup ((f a1 a2, n+m) : tl)
  fixup stack = stack
  done [] = z
  done stack = foldl1' (\a2 a1 -> f a1 a2) (map fst stack)

-- | Compute the width and height occupied by every node in the layout.
areas :: (e -> (Width,Height)) -> Box e p -> Box e (p, (Width,Height))
areas dims b = accumulate step b where
  zero = (Dimensions.zero, Dimensions.zero)
  step BEmpty = zero
  step (BEmbed e) = dims e
  step (BFlow Horizontal bs) = foldl' Dimensions.hcombine zero bs
  step (BFlow Vertical bs) = foldl' Dimensions.vcombine zero bs

-- | Compute the region of every node in the layout, consisting of a
-- an (x,y,w,h), where (x,y) is the top left corner of the region, and
-- (w,h) are the width and height of the region, respectively. All (x,y)
-- coordinates are relative to the top left of the root `Box` passed
-- in, which will always have an (x,y) component of (0,0).
bounds :: (e -> (Width,Height)) -> Box e p -> Box e (p, Region)
bounds dims b = go (areas dims b) (Dimensions.zero, Dimensions.zero) where
  go ((p,(w,h)) :< box) xy@(x,y) = (p, (x,y,w,h)) :< case box of
    BEmpty -> BEmpty
    BEmbed e -> BEmbed e
    BFlow Horizontal bs -> BFlow Horizontal
      [ go b pt | (b,pt) <- bs `zip` centerAlignedH xy (map (snd . root) bs) ]
    BFlow Vertical bs -> BFlow Vertical
      [ go b pt | (b,pt) <- bs `zip` leftAlignedV xy (map (snd . root) bs) ]
  -- todo, should support other types of alignment for BFlow rather
  -- than assuming items are center-aligned for horizontal and left-aligned for vertical
  centerAlignedH (x, Y y) areas =
    let Height maxh = maximum (Height 0 : map snd areas)
        xs = scanl' (\x (Width w) -> Dimensions.plus x (X w)) x (map fst areas)
    in [(x, Y $ y + ((maxh - h) `quot` 2)) | (x,(_,Height h)) <- xs `zip` areas ]
  leftAlignedV (x, y) areas = map (x,) $
    scanl' (\y (Height h) -> Dimensions.plus y (Y h)) y (map snd areas)

-- | Compute the longest path whose region contains the given point.
-- See note on `hits`.
at :: (Path p, Eq p) => Box e (p, Region) -> (X,Y) -> p
at box (x,y) = contains box (x,y,Dimensions.zero,Dimensions.zero)

-- | Compute the longest path whose region passes the `hit` function,
-- which is given the top left and lower right corners of the input region.
--
-- The point (X 0, Y 0) is assumed to correspond to the top left
-- corner of the layout.
hits :: (Path p, Eq p)
     => ((X,Y) -> (X,Y) -> Region -> Bool)
     -> Box e (p, Region) -> Region -> p
hits hit box (X x,Y y,Width w,Height h) = foldr Path.extend Path.root $ go box
  where
  -- only include nonempty path segments, with exception of first
  pt1 = (X x, Y y)
  pt2 = (X (x+w), Y (y+h))
  go ((p,region) :< box) | hit pt1 pt2 region = p : (toList box >>= go)
                         | otherwise          = []

-- | Compute the longest path whose bounding region fully contains
-- the input region. See note on `hits`. Satisfies `region box (contains box r) == r`.
contains :: (Path p, Eq p) => Box e (p, Region) -> Region -> p
contains = hits $ \p1 p2 region ->
  Dimensions.within p1 region && Dimensions.within p2 region

-- | Find the leaf region (contains no other regions) corresponding to the path
region :: (Path p, Eq p) => Box e (p, Region) -> p -> Region
region box path = fromMaybe (snd . root $ box) r
  where
  rs = possible path box
  r = find (\r -> contains box r == path) rs
  possible searchp ((_,region) :< _) | searchp == Path.root = [region]
  possible searchp ((p,region) :< box) =
    -- bail on this branch if we can't fully consume its path segment
    -- OR if segment nonempty and shares nothing in common w/ query
    if p' /= Path.root || (p /= Path.root && lca == Path.root) then []
    else region : (toList box >>= possible searchp')
    where
    (lca, (p',searchp')) = Path.factor p searchp

up', down', left', right' :: (Eq p, Path p) => Box e (p, Region) -> p -> Maybe p
up' = navigate' go where
  go (_,y0,_,_) =
    let max r1 (_,y,_,_) | y > y0 = r1 -- region below origin, ignore
        max Nothing r1 = Just r1
        max (Just (_,y,_,_)) r2@(_,y2,_,_) | y2 > y = Just r2 -- prefer closer to bottom
        max r1 _ = r1
    in max

down' = navigate' go where
  go (_,y0,_,_) =
    let max r1 (_,y,_,_) | y < y0 = r1 -- region above origin, ignore
        max Nothing r1 = Just r1
        max (Just (_,y,_,_)) r2@(_,y2,_,_) | y2 < y = Just r2 -- prefer closer to top
        max r1 _ = r1
    in max

left' = navigate' go where
  go (x0,_,_,_) =
    let max r1 (x,_,_,_) | x > x0 = r1 -- region to the right of origin, ignore
        max Nothing r1 = Just r1
        max (Just (x,_,_,_)) r2@(x2,_,_,_) | x2 > x = Just r2 -- prefer closer to right
        max r1 _ = r1
    in max

right' = navigate' go where
  go (x0,_,_,_) =
    let max r1 (x,_,_,_) | x < x0 = r1 -- region to the left of origin, ignore
        max Nothing r1 = Just r1
        max (Just (x,_,_,_)) r2@(x2,_,_,_) | x2 < x = Just r2 -- prefer closer to left
        max r1 _ = r1
    in max

up, down, left, right :: (Eq p, Path p) => Box e (p, Region) -> p -> p
up box p = fromMaybe p (up' box p)
down box p = fromMaybe p (down' box p)
left box p = fromMaybe p (left' box p)
right box p = fromMaybe p (right' box p)

navigate' :: (Eq p, Path p)
          => (Region -> Maybe Region -> Region -> Maybe Region)
          -> Box e (p, Region) -> p -> Maybe p
navigate' f box p =
  let
    origin = region box p
    leafRegions rs = [ r | r:tl <- init . tails $ rs
                         , null (takeWhile (r `has`) tl)
                         , not (origin `has` r) ]
    leaves = leafRegions $ map snd (preorder box)
    has super (X x,Y y,Width w,Height h) =
      Dimensions.within (X x,Y y) super &&
      Dimensions.within (X $ x+w, Y $ y+h) super
  in contains box <$> foldl' (f origin) Nothing leaves

navigate :: (Eq p, Path p)
         => (Region -> Maybe Region -> Region -> Maybe Region)
         -> Box e (p, Region) -> p -> p
navigate f box p = fromMaybe p (navigate' f box p)

-- | Preorder traversal of the annotations of a `Cofree`.
preorder :: Foldable f => Cofree f p -> [p]
preorder (p :< f) = p : (toList f >>= preorder)

-- for debugging
debugBox :: Show e => Box e p -> String
debugBox b = formatString (Width 80) doc where
  doc = einterpret go (emap (embed . show) b)
  go BEmpty = empty :: Doc String ()
  go (BEmbed e) = e
  go (BFlow Horizontal bs) = group $ docs [embed "h[ ", delimit (breakable " ") (map (nest "   ") bs), embed " ]"]
  go (BFlow Vertical bs) = group $ docs [embed "v[ ", delimit (breakable " ") (map (nest "   ") bs), embed " ]"]

debugBoxp :: Show p => Box e p -> String
debugBoxp b = formatString (Width 80) (go b) where
  go (p :< BEmpty) = embed (show p) :: Doc String ()
  go (p :< BEmbed _) = embed (show p)
  go (p :< BFlow dir bs) = group $ docs [ embed (d dir), embed (show p), embed " [ ", breakable ""
                                        , delimit (breakable " ") (map (nest "  " . go) bs)
                                        , breakable " "
                                        , embed "]"]
    where d Horizontal = "h "
          d Vertical = "v "

debugLayout :: Show p => Layout e p -> [String]
debugLayout (p :< l) = show p : case l of
  LAppend (_ :< LEmpty) b -> debugLayout b
  LAppend a (_ :< LEmpty) -> debugLayout a
  LAppend a b -> debugLayout a ++ debugLayout b
  LNest _ r -> debugLayout r
  LGroup r -> debugLayout r
  _ -> []

debugDoc :: Show p => Doc e p -> [String]
debugDoc (p :< l) = show p : case l of
  Append a b -> debugDoc a ++ debugDoc b
  Nest _ r -> debugDoc r
  Group r -> debugDoc r
  _ -> []

-- various instances

instance Bifunctor L where
  second = fmap
  first f b = case b of
    LEmpty -> LEmpty
    LEmbed e -> LEmbed (f e)
    LAppend a b -> LAppend a b
    LGroup r -> LGroup r
    LLinebreak -> LLinebreak
    LNest e p -> LNest (f e) p

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
    Breakable e -> Breakable (f e)
    Linebreak -> Linebreak
    Group r -> Group r
    Nest e r -> Nest (f e) r
    Append r r2 -> Append r r2
