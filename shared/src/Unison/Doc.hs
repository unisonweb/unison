-- |
-- A combinator library for building responsive layouts.
-- Like a prettyprinting library, a single `Doc` may be laid out at multiple
-- widths in a way that respects constraints on linebreaks set by the
-- programmer. But additionally, all nodes of the produced `Doc` are
-- annotated with a 'path' and we can quickly lookup the rectangular region
-- corresponding to a path, or lookup what path corresponds
-- to a given location in the layout.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Unison.Doc where

import Control.Comonad.Cofree (Cofree(..), unwrap) -- (:<)
import Control.Comonad (extract)
import Control.Monad.State.Strict
import Data.Functor
import Data.List (intersperse)
import Unison.Path (Path)
import qualified Unison.Path as Path

data Padded e r =
  Padded { top :: e, bottom :: e, left :: e, right :: e, element :: r } deriving Functor

data D e r
  = Empty
  | Embed e
  | Pad (Padded e r)
  | Breakable e
  | Linebreak
  | Group r
  | Nest e r
  | Append r r deriving Functor

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
  | LAppend r r deriving Functor

-- A `Doc` without the nondeterminism. All layout decisions have been fixed.
type Layout e p = Cofree (L e) p

-- | Produce a `Layout` which tries to fit in the given width,
-- assuming that embedded `e` elements have the computed width.
-- Runs in linear time without backtracking.
layout :: (e -> Int) -> Int -> Doc e p -> Layout e p
layout width maxWidth doc =
  fmap fst $ evalState (go (preferredWidth width doc)) (maxWidth, maxWidth)
  where
  go doc = do
    (maxWidth, remainingWidth) <- get
    case doc of
      (_,w) :< _ | w <= remainingWidth ->
        put (maxWidth, remainingWidth - w) $> flow doc
      _ :< Group doc -> break doc
      _ -> break doc

  -- | Break a document into a list of documents, separated by lines,
  -- respecting the linebreak constraints of the input `Doc`.
  break (p :< doc) = get >>= \(maxWidth, remainingWidth) -> case doc of
    Empty -> pure $ p :< LEmpty
    Embed e -> put (maxWidth, remainingWidth - width e) $> (p :< LEmbed e)
    Breakable _ -> put (maxWidth, maxWidth) $> (p :< LLinebreak)
    Linebreak -> put (maxWidth, maxWidth) $> (p :< LLinebreak)
    Append a b -> (:<) p <$> (LAppend <$> break a <*> break b)
    Pad padded ->
      let borderWidth = width (left padded) + width (right padded)
      in do
        put (maxWidth - borderWidth, remainingWidth - borderWidth)
        inner <- break (element padded)
        modify (\(_, remainingWidth) -> (maxWidth, remainingWidth - borderWidth))
        return $ p :< LPad (padded { element = inner })
    Nest e doc -> do
      case maxWidth == remainingWidth of
        -- we're immediately preceded by newline, insert `e` and indent
        True -> do
          put $ let newMax = maxWidth - width e in (newMax, newMax)
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
preferredWidth :: (e -> Int) -> Doc e p -> Doc e (p,Int)
preferredWidth width (p :< d) = case d of
  Empty -> (p, 0) :< Empty
  Embed e -> (p, width e) :< Embed e
  Pad padded ->
    let borderWidth = width (left padded) + width (right padded)
        inner = preferredWidth width (element padded)
        innerWidth = snd (root inner)
    in (p, borderWidth + innerWidth) :< Pad (padded { element = inner })
  -- Since we just use this to decide whether to break or not,
  -- as long as `flow` and `break` both interpret `Linebreak` properly,
  -- a zero width for linebreaks is okay
  Linebreak -> (p, 0) :< Linebreak
  Breakable e -> (p, width e) :< Breakable e -- assuming we fit on the line
  Append left right ->
    let left' = preferredWidth width left
        right' = preferredWidth width right
    in (p, snd (extract left') + snd (extract right')) :< Append left' right'
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
emap :: (e -> e2) -> Doc e p -> Doc e2 p
emap f (p :< d) = p :< case d of
  Append d1 d2 -> Append (emap f d1) (emap f d2)
  Group d -> Group (emap f d)
  Nest e d -> Nest (f e) (emap f d)
  Breakable e -> Breakable (f e)
  Embed e -> Embed (f e)
  Pad (Padded t b l r inner) -> Pad (Padded (f t) (f b) (f l) (f r) (emap f inner))
  Linebreak -> Linebreak
  Empty -> Empty

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

data Renderer e =
  Renderer { rhorizontal :: [e] -> e, rvertical :: [e] -> e }

data Renderer' e =
  Renderer' { rconcat :: [e] -> e, rnewline :: e }

render :: Renderer e -> (e0 -> e) -> Layout e0 p -> e
render r f l =
  finish (execState (go l) ([],[],[]))
  where
  finish (_, vstack, []) = rvertical r (reverse vstack)
  finish (_, vstack, hbuf) = finish ([], rhorizontal r (reverse hbuf) : vstack, [])
  col3 p top mid bot = p :< (LAppend (p :< (LAppend (p :< LEmbed top) mid)) (p :< LEmbed bot))
  row3 p left mid right = p :< (LAppend (p :< (LAppend (p :< LEmbed left) mid)) (p :< LEmbed right))
  -- state is (indentation snoc list, vertical buffer snoc list, current line snoc list)
  go (p :< l) = case l of
    LEmpty -> return ()
    LLinebreak -> modify cr where
      cr (indent, vstack, hbuf) = (indent, rhorizontal r (reverse hbuf) : vstack, [])
    LEmbed e -> modify g where
      -- we indent if we're the first token on this line
      g (indent, vstack, []) = (indent, vstack, f e : indent)
      g (indent, vstack, hbuf) = (indent, vstack, f e : hbuf)
    LPad padded -> modify g where
      inner = render r f (col3 p (top padded)
                                 (row3 p (left padded) (element padded) (right padded))
                                 (bottom padded))
      -- we indent if we're the first token on this line
      g (indent, vstack, []) = (indent, vstack, inner : indent)
      g (indent, vstack, hbuf) = (indent, vstack, inner : hbuf)
    LNest e r -> do
      modify (\(i,v,h) -> (f e : i, v, h))
      go r
      modify (\(i,v,h) -> (tail i, v, h))
    LAppend a b -> go a *> go b

render' :: Renderer' e -> (e0 -> e) -> Layout e0 p -> e
render' r f l = finish (execState (go l) ([],[],True))
  where
  finish (_, buf, _) = rconcat r (reverse buf)
  col3 p top mid bot = p :< (LAppend (p :< (LAppend (p :< LEmbed top) mid)) (p :< LEmbed bot))
  row3 p left mid right = p :< (LAppend (p :< (LAppend (p :< LEmbed left) mid)) (p :< LEmbed right))
  -- state is (indentation snoc list, token buffer snoc list)
  go (p :< l) = case l of
    LEmpty -> return ()
    LLinebreak -> modify cr where
      cr (indent, buf, _) = (indent, rnewline r : buf, True)
    LEmbed e -> modify g where
      -- we indent if we're the first token on this line
      g (indent, buf, True) = (indent, f e : (indent ++ buf), False)
      g (indent, buf, _) = (indent, f e : buf, False)
    LPad padded -> modify g where
      inner = render' r f (col3 p (top padded)
                                  (row3 p (left padded) (element padded) (right padded))
                                  (bottom padded))
      -- we indent if we're the first token on this line
      g (indent, buf, True) = (indent, inner : (indent ++ buf), False)
      g (indent, buf, _) = (indent, inner : buf, False)
    LNest e r -> do
      modify (\(i,b,fst) -> (f e : i, b, fst))
      go r
      modify (\(i,b,fst) -> (tail i, b, fst))
    LAppend a b -> go a *> go b

renderString :: Layout String p -> String
renderString = render' (Renderer' concat "\n") id

formatString :: Int -> Doc String p -> String
formatString availableWidth d = renderString (layout length availableWidth d)

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
