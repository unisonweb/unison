{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances for LSP types which are strangely missing.
module Unison.LSP.Orphans where

import Control.Lens
import Language.LSP.Protocol.Lens (HasTextDocument (..), HasUri (..))
import Language.LSP.Protocol.Types

instance HasTextDocument TextDocumentIdentifier TextDocumentIdentifier where
  textDocument = Prelude.id

instance HasTextDocument VersionedTextDocumentIdentifier VersionedTextDocumentIdentifier where
  textDocument = Prelude.id

instance HasUri NormalizedUri Uri where
  uri = iso fromNormalizedUri toNormalizedUri
