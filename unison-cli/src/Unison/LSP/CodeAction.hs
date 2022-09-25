{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.CodeAction where

import Control.Lens hiding (List)
import qualified Data.IntervalMap as IM
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.Debug as Debug
import Unison.LSP.Conversions
import Unison.LSP.FileAnalysis
import Unison.LSP.Types
import Unison.Prelude

-- | Computes code actions for a document.
codeActionHandler :: RequestMessage 'TextDocumentCodeAction -> (Either ResponseError (ResponseResult 'TextDocumentCodeAction) -> Lsp ()) -> Lsp ()
codeActionHandler m respond =
  respond . maybe (Right mempty) (Right . List . fmap InR) =<< runMaybeT do
    FileAnalysis {codeActions} <- getFileAnalysis (m ^. params . textDocument . uri)
    let r = m ^. params . range
    let relevantActions = IM.intersecting codeActions (rangeToInterval r)
    Debug.debugM Debug.LSP "All CodeActions" (codeActions)
    Debug.debugM Debug.LSP "Relevant actions" (r, relevantActions)
    pure $ fold relevantActions
