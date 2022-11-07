-- | The private Unison.Name innards. Prefer importing Unison.Name instead, unless you need the data constructor of
-- Name.
module Unison.Name.Internal
  ( Name (..),
  )
where

import Control.Lens as Lens
import Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List (NonEmpty)
import Unison.NameSegment (NameSegment)
import Unison.Position (Position (..))
import Unison.Prelude

-- | A name is an absolute-or-relative non-empty list of name segments.
data Name
  = -- A few example names:
    --
    --   "foo.bar"  --> Name Relative ["bar", "foo"]
    --   ".foo.bar" --> Name Absolute ["bar", "foo"]
    --   "|>.<|"    --> Name Relative ["<|", "|>"]
    --   "."        --> Name Relative ["."]
    --   ".."       --> Name Absolute ["."]
    --
    Name
      -- whether the name is positioned absolutely (to some arbitrary root namespace), or relatively
      Position
      -- the name segments in reverse order
      (List.NonEmpty NameSegment)
  deriving stock (Eq, Generic, Show)

instance Ord Name where
  compare (Name p0 ss0) (Name p1 ss1) =
    compare ss0 ss1 <> compare p0 p1

instance Lens.Snoc Name Name NameSegment NameSegment where
  _Snoc =
    Lens.prism snoc unsnoc
    where
      snoc :: (Name, NameSegment) -> Name
      snoc (Name p (x :| xs), y) =
        Name p (y :| x : xs)
      unsnoc :: Name -> Either Name (Name, NameSegment)
      unsnoc name =
        case name of
          Name _ (_ :| []) -> Left name
          Name p (x :| y : ys) -> Right (Name p (y :| ys), x)
