{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Unison.LSP.RequestHandlers where

import Control.Lens
import Control.Monad.IO.Class
import Language.LSP.Types
import Language.LSP.Types.Lens
import Unison.LSP.Types

hoverHandler :: RequestMessage 'TextDocumentHover -> (Either ResponseError (ResponseResult 'TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m _respond = do
  identifierAtPositionParams m
  liftIO $ putStrLn "HOVER"
  pure ()

completionHandler :: RequestMessage 'TextDocumentCompletion -> (Either ResponseError (ResponseResult 'TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m _respond = do
  identifierAtPositionParams m
  liftIO $ putStrLn "HOVER"
  pure ()

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
