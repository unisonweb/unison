{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.LSP.Orphans where

import Data.Function (on)
import Language.LSP.Types (TextDocumentIdentifier (..), VersionedTextDocumentIdentifier)
import Language.LSP.Types.Lens (HasTextDocument (..))

instance Ord TextDocumentIdentifier where
  compare = compare `on` _uri

instance HasTextDocument TextDocumentIdentifier TextDocumentIdentifier where
  textDocument = id

instance HasTextDocument VersionedTextDocumentIdentifier VersionedTextDocumentIdentifier where
  textDocument = id
