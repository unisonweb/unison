-- | This module exposes the underlying representation of `NameSegment`, and
--   thus should only be imported by parsers & printers.
module Unison.NameSegment.Internal (NameSegment (..)) where

import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical)

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment
  { -- | Convert a name segment to unescaped text.
    --
    -- You might use this when storing a name segment as text in a database, where the literal name segment bytes are all
    -- that matter. However, you wouldn't use this to display the name segment to a user - that depends on concrete syntax.
    -- See Unison.Syntax.NameSegment (or indeed, some actual yet-built interface that abstracts concrete syntax) for that
    -- kind of function.
    --
    -- > toUnescapedText (unsafeFromText ".~") = ".~"
    toUnescapedText :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Alphabetical)
