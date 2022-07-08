{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.CodeLens where

import Language.LSP.Types
import Unison.LSP.Types
import Unison.Prelude

-- | Computes code lenses for a document.
codeLensHandler :: RequestMessage 'TextDocumentCodeLens -> (Either ResponseError (ResponseResult 'TextDocumentCodeLens) -> Lsp ()) -> Lsp ()
codeLensHandler _m respond =
  respond . maybe (Right mempty) (Right . List) =<< runMaybeT do
    pure []

-- | Runs a selected code lens
codeLensResolveHandler :: RequestMessage 'CodeLensResolve -> (Either ResponseError (ResponseResult 'CodeLensResolve) -> Lsp ()) -> Lsp ()
codeLensResolveHandler _m respond = respond (Left $ error "unsupported")
