module Unison.Name.Forward where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Unison.Name qualified as Name
import Unison.Name.Internal (Name)
import Unison.NameSegment (NameSegment)

newtype ForwardName = ForwardName {toList :: NonEmpty NameSegment} deriving (Eq, Ord, Show)

-- | O(d)
fromName :: Name -> ForwardName
fromName n = ForwardName $ Name.segments n

stripNamePrefix :: ForwardName -> ForwardName -> Maybe ForwardName
stripNamePrefix (ForwardName (p :| ps)) (ForwardName (n :| ns)) =
  if p /= n
    then Nothing
    else ForwardName <$> maybe Nothing nonEmpty (List.stripPrefix ps ns)
