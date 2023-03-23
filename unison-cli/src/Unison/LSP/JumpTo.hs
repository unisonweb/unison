{-# LANGUAGE DataKinds #-}

module Unison.LSP.JumpTo (jumpToDefinitionHandler) where

import Control.Lens hiding (List, (:<))
import Control.Monad.Reader
import Data.Either.Extra (mapRight)
import Language.LSP.Types
import Language.LSP.Types.Lens
import Unison.LSP.Types
import qualified Unison.LSP.VFS as VFS
import Unison.Prelude

jumpToDefinitionHandler :: RequestMessage 'TextDocumentDefinition -> (Either ResponseError (ResponseResult 'TextDocumentDefinition) -> Lsp ()) -> Lsp ()
jumpToDefinitionHandler m respond =
  respond . maybe (Right (InR (InL mempty))) (mapRight InL) =<< runMaybeT do
    let p = m ^. params
    let fileUri = p ^. textDocument . uri
    let pos = p ^. position
    -- ref <- LSPQ.refAtPosition fileUri pos
    identifier <- VFS.identifierAtPosition fileUri pos
    -- We always just add new definitions at the top of the file so it's easy to jump there.
    let editRange = Range (Position 0 0) (Position 0 0)
    let newText = "Testing!\n"
    wait <- lift $ VFS.editFileInClient fileUri ("Edit " <> identifier) editRange newText
    liftIO wait
      >>= ( \case
              Left err -> pure (Left err)
              Right resp -> do
                guard (resp ^. applied)
                pure (Right $ Location fileUri editRange)
          )
