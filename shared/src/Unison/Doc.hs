-- |
-- A combinator library for building responsive layouts.
-- Like a prettyprinting library, a single `Doc` may be laid out at multiple
-- widths in a way that respects constraints on linebreaks set by the
-- programmer. But additionally, all nodes of the produced `Doc` are
-- annotated with a 'path' and we can quickly lookup the rectangular region
-- corresponding to a path, or lookup what path corresponds
-- to a given location in the layout.

{-# LANGUAGE FlexibleContexts #-}

module Unison.Doc where

import Control.Monad.State.Strict
import Data.Functor
import Unison.Layout (Layout(..))
import Unison.Path (Path)
import qualified Unison.Layout as Layout
import qualified Unison.Path as Path

data D e r
  = Empty
  | Embed e
  | Line e
  | Append r r
  | Group r
  | Nest e r

-- | A `Doc p e` describes a layout that may be rendered at
-- multiple widths. The `e` parameter is the token type, possibly
-- `String` or `Text`. The `p` parameter is the path type,
-- generally a list. Note that the full path corresponding to a
-- subtree in the document is the concatenation of all paths starting
-- from the root.
data Doc p e = Doc p (D e (Doc p e))

-- | Produce a `Layout` which tries to fit in the given width,
-- assuming that embedded `e` elements have the computed width.
-- Runs in linear time without backtracking.
layout :: (e -> Int) -> Int -> Doc p e -> Layout p e
layout width maxWidth doc =
  Layout.mapPath fst $ evalState (go (preferredWidth width doc)) (maxWidth, maxWidth)
  where
  go doc = do
    (maxWidth, remainingWidth) <- get
    case doc of
      Doc (_,w) _ | w <= remainingWidth ->
        put (maxWidth, remainingWidth - w) $> flow doc
      Doc _ (Group doc) -> break doc
      _ -> break doc

  -- | Break a document into a list of documents, separated by lines,
  -- respecting the linebreak constraints of the input `Doc`.
  break (Doc p doc) = get >>= \(maxWidth, remainingWidth) -> case doc of
    Empty -> pure $ Layout p Layout.Empty
    Embed e -> put (maxWidth, remainingWidth - width e) $> Layout p (Layout.Embed e)
    Line _ -> put (maxWidth, maxWidth) $> Layout p Layout.Linebreak
    Append a b -> Layout p <$> (Layout.Append <$> break a <*> break b)
    Nest e doc -> do
      case maxWidth == remainingWidth of
        -- we're immediately preceded by newline, insert `e` and indent
        True -> do
          put $ let newMax = maxWidth - width e in (newMax, newMax)
          doc <- break doc
          return $ Layout p (Layout.Nest e doc)
        -- we're in the middle of a line, ignore `e`
        False -> break doc
    Group doc -> go doc -- we try to avoid breaking subgroups

-- | Layout the `Doc` assuming infinite available width
flow :: Doc p e -> Layout p e
flow (Doc p doc) = case doc of
  Empty -> Layout p Layout.Empty
  Embed e -> Layout p (Layout.Embed e)
  Line e -> Layout p (Layout.Embed e) -- don't linebreak, it fits
  Append a b -> Layout p (flow a `Layout.Append` flow b)
  Group r -> flow r
  Nest _ r -> flow r

-- | Annotate the document with the preferred width of each subtree,
-- assuming that embedded elements have the given width function.
preferredWidth :: (e -> Int) -> Doc p e -> Doc (p,Int) e
preferredWidth width (Doc p d) = case d of
  Empty -> Doc (p, 0) Empty
  Embed e -> Doc (p, width e) (Embed e)
  Line e -> Doc (p, width e) (Line e) -- assuming we fit on the line
  Append left right ->
    let left'@(Doc (_,n) _) = preferredWidth width left
        right'@(Doc (_,m) _) = preferredWidth width right
    in Doc (p, n+m) (Append left' right')
  Group d ->
    let pd@(Doc (_,n) _) = preferredWidth width d
    in Doc (p, n) (Group pd)
  Nest e d -> -- assume it fits, so ignore the `e`
    let pd@(Doc (_,n) _) = preferredWidth width d
    in Doc (p,n) (Nest e pd)

-- | The root path of this document
root :: Doc p e -> p
root (Doc p _) = p

-- | The empty document
empty :: Path p => Doc p e
empty = Doc Path.root Empty

-- | Append two documents
append :: Path p => Doc p e -> Doc p e -> Doc p e
append (Doc p1 d1) (Doc p2 d2) =
  case Path.factor p1 p2 of
    (lca, (p1, p2)) -> Doc lca (Doc p1 d1 `Append` Doc p2 d2)

-- | Replace the path component of this `Doc`
reroot :: p -> Doc p e -> Doc p e
reroot p (Doc _ d) = Doc p d

-- | Make a `Doc` from a token and give it an empty path
embed :: Path p => e -> Doc p e
embed e = Doc Path.root (Embed e)

-- | Make a `Doc` from a token and give it the specified path
embed' :: p -> e -> Doc p e
embed' p e = Doc p (Embed e)

-- | Wrap this `Doc` in a group, which constrains all layout
-- choices in the group to be the same. For instance,
-- `group $ embed "a" <> line " " <> embed "b" <> line " " <> embed "c"`
-- will layout as either "a b c" OR as both spaces replaced by newlines.
group :: Path p => Doc p e -> Doc p e
group (Doc p d) = Doc p (Group (Doc Path.root d))

-- | If immediately preceded by a newline, indent the `Doc` by the given element
-- otherwise ignore the `e` argument.
nest :: Path p => e -> Doc p e -> Doc p e
nest e (Doc p d) = Doc p (Nest e (Doc Path.root d))

-- | Output a newline, or the given `e` on the same line if it fits
line :: Path p => e -> Doc p e
line e = Doc Path.root (Line e)

-- | Output a newline, or the given `e` on the same line if it fits
line' :: Path p => p -> e -> Doc p e
line' p e = Doc p (Line e)

instance Path p => Monoid (Doc p e) where
  mempty = empty
  mappend = append
