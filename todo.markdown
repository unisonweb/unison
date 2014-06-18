testing of typechecker
editing combinators
editor backend


-- a layout tree uses hashes, not names
-- merely a convention of the editor that it
-- transitively updates dependent hashes
-- that is, given:
-- y = 42
-- f x = x + y
-- the editor just adopts the convention that
-- updating `y` will update `f` to point to the
-- new hash of the thing called `y`, rather than
-- continuing to point at old hash
--

-- need path and action type for ADTs
-- implement node server locally
-- no need to have layout type in Haskell, as layouts are
-- entirely interpreted client side

module Unison.Syntax.Layout where

import Unison.Syntax.Layout.Style
import Data.Text

{-
 - source : a -> Layout
 - dataflow : a -> Layout
 - execute : IO a -> Layout
 - horizontal : List Layout -> Layout
 - vertical : List Layout -> Layout
 - cell : Style -> (a -> Layout) -> a -> Layout
 - class : {Escaped} String -> Layout -> Layout
 - id : {Escaped} String -> Layout -> Layout
 - bounded : Height -> Width -> Layout -> Layout
 - scroll : Layout -> Layout
 - river : Stream Layout -> Layout
 -}

-- | A @Layout k@ describes a visual arrangement of Unison
-- terms and types, where @k@ will generally be 'Unison.Syntax.Hash'.
data Layout k
  = Source k                -- ^ Exposed in Unison as @a -> Layout@
  | Type k                  -- ^ Same as 'Source', but we are laying out a type
  | Dataflow k              -- ^ Exposed in Unison as @dataflow : a -> Layout@
  | Trace k                 -- ^ Like 'Dataflow', but gives trace of all intermediate evaluation steps
  | Execute k               -- ^ Like 'Dataflow', but runs IO actions as well,
                            -- ^ exposed as @execute : IO a -> Layout@
  | Cell Style k k          -- ^ @Cell Style layout term@
  | Horizontal [Layout k]   -- ^ Horizontal flow, each child takes up its preferred space
  | Vertical [Layout k]     -- ^ Vertical flow, each child takes up its preferred space
  | Class Text (Layout k)   -- ^ Attach a "class" attribute to this 'Layout'
  | Id Text (Layout k)      -- ^ Attach an "id" attribute to this 'Layout'
  deriving (Eq,Ord,Show,Read)

-- | Controls presentation of a cell in a 'Unison.Edit.Layout'
data Style = Style {
  name :: Text,
  nameVisible :: Bool,
  showEquals :: Bool,
  style :: Text
} deriving (Eq,Ord,Show,Read)
