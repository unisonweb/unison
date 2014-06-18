module Unison.Node.Panel where

import qualified Data.Map as M
import Unison.Node.Metadata as D

-- | Represents a view of a collection of Unison types in @t@ and
-- terms in @e@, with hashes of type @k@. This structure can in principle be
-- derived client side using only the layout; this type exists for
-- convenience and efficiency so the client can avoid multiple requests to
-- the server when first rendering a panel.
data Panel k t e = Panel {
  layout :: e, -- ^ Specifies overall layout of the panel
  terms :: M.Map k e, -- ^ Term definitions referenced by the layout
  types :: M.Map k t, -- ^ Type definitions references by the layout
  metadata :: M.Map k (D.Metadata k), -- ^ The metadata for all hashes used in the layout
  dependencies :: M.Map k [k], -- ^ The dependencies of each hash, for dataflow-style updating by client
  layouts :: M.Map k e -- ^ For each cell, the prerendered layout
}
