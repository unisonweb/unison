module Unison.Node where

import Unison.Node.Panel
import Unison.Node.Metadata as M

data Node m k t e = Node {
  term :: k -> m (Maybe e), -- ^ Lookup the source of the term identified by @k@
  typ :: k -> m (Maybe t), -- ^ Lookup the source of the type identified by @k@
  metadata :: k -> m (Maybe (M.Metadata k)), -- ^ Access the metadata for the term or type identified by @k@
  panel :: k -> m (Maybe (Panel k t e)) -- ^ Render the term or type identified by @k@ as a panel

  -- editing, creating terms and types
  -- evaluation
}


