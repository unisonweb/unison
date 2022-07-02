{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Unison.LSP.RequestHandlers where

import Control.Lens hiding (List)
import Control.Monad.IO.Class
import Language.LSP.Types
import Language.LSP.Types.Lens
import Unison.LSP.Types
import Unison.LSP.VFS
import Unison.Prelude

hoverHandler :: RequestMessage 'TextDocumentHover -> (Either ResponseError (ResponseResult 'TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m _respond = do
  identifierAtPositionParams m
  liftIO $ putStrLn "HOVER"
  pure ()

completionHandler :: RequestMessage 'TextDocumentCompletion -> (Either ResponseError (ResponseResult 'TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m respond = do
  completion <- completionPrefix (m ^. params)
  liftIO $ print ("COMPLETION" :: String, completion)
  respond . Right . InL . List . fmap expand $ maybeToList completion
  where
    expand :: (Text, Text) -> CompletionItem
    expand (namespace, name) =
      CompletionItem
        { _label = namespace <> "." <> name <> "and.more",
          _kind = Nothing,
          _tags = Nothing,
          _detail = Nothing,
          _documentation = Nothing,
          _deprecated = Nothing,
          _preselect = Nothing,
          _sortText = Nothing,
          _filterText = Nothing,
          _insertText = Nothing,
          _insertTextFormat = Nothing,
          _insertTextMode = Nothing,
          _textEdit = Nothing,
          _additionalTextEdits = Nothing,
          _commitCharacters = Nothing,
          _command = Nothing,
          _xdata = Nothing
        }

data Thing = Thing

identifierAtPosition :: (HasTextDocument a TextDocumentIdentifier, HasPosition a Position) => a -> Lsp Thing
identifierAtPosition p = do
  let _pos = p ^. position
  let _doc = p ^. textDocument
  pure Thing

identifierAtPositionParams :: (HasParams m p, HasTextDocument p TextDocumentIdentifier, HasPosition p Position) => m -> Lsp Thing
identifierAtPositionParams m = do
  let p = (m ^. params)
  identifierAtPosition p
