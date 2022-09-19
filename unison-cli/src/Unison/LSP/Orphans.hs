{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances for LSP types which are strangely missing.
module Unison.LSP.Orphans where

import Control.Lens
import Data.Function (on)
import Language.LSP.Types
import Language.LSP.Types.Lens (HasTextDocument (..), HasUri (..))

instance Ord TextDocumentIdentifier where
  compare = compare `on` view uri

instance HasTextDocument TextDocumentIdentifier TextDocumentIdentifier where
  textDocument = Prelude.id

instance HasTextDocument VersionedTextDocumentIdentifier VersionedTextDocumentIdentifier where
  textDocument = Prelude.id

instance HasUri NormalizedUri Uri where
  uri = iso fromNormalizedUri toNormalizedUri
