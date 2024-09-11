module Unison.Name.Forward where

import Data.List.NonEmpty (NonEmpty)
import Unison.Name qualified as Name
import Unison.Name.Internal (Name)
import Unison.NameSegment (NameSegment)

newtype ForwardName = ForwardName {toList :: NonEmpty NameSegment} deriving (Eq, Ord, Show)

-- | O(d)
fromName :: Name -> ForwardName
fromName n = ForwardName $ Name.segments n
