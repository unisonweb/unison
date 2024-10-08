{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances for LSP types which are strangely missing.
module Unison.LSP.Orphans where

import Control.Lens
import Language.LSP.Protocol.Lens (HasUri (..))
import Language.LSP.Protocol.Types

instance HasUri NormalizedUri Uri where
  uri = iso fromNormalizedUri toNormalizedUri
