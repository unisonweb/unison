{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.LSP.WorkspaceSymbols where

import Control.Lens
import Data.Row.Records qualified as Rec
import Language.LSP.Protocol.Lens qualified as Msg
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.Debug qualified as Debug
import Unison.LSP.Orphans ()
import Unison.LSP.Types

workspaceSymbolHandler :: Msg.TRequestMessage 'Msg.Method_WorkspaceSymbol -> (Either Msg.ResponseError ([SymbolInformation] |? ([WorkspaceSymbol] |? Null)) -> Lsp ()) -> Lsp ()
workspaceSymbolHandler msg respond = do
  Debug.debugLogM Debug.Temp "Request!"
  let _q = msg ^. Msg.params . Msg.query
  respond . Right . InR . InL $
    [ foo
    ]

foo :: WorkspaceSymbol
foo =
  WorkspaceSymbol
    { _name = "foo",
      _kind = SymbolKind_Function,
      _tags = Nothing,
      _containerName = Nothing,
      _location = InR (#uri Rec..== (Uri "scratch.u")),
      _data_ = Nothing
    }

workspaceSymbolResolveHandler :: Msg.TRequestMessage 'Msg.Method_WorkspaceSymbolResolve -> (Either Msg.ResponseError WorkspaceSymbol -> Lsp ()) -> Lsp ()
workspaceSymbolResolveHandler _msg respond = do
  Debug.debugLogM Debug.Temp "Resolve!"
  respond . Right $ foo
