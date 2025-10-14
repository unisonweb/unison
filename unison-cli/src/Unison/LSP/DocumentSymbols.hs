module Unison.LSP.DocumentSymbols
  ( documentSymbolsHandler,
  )
where

import Control.Lens hiding (List)
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens hiding (error)
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.LSP.FileAnalysis
import Unison.LSP.FileAnalysis qualified as FileAnalysis
import Unison.LSP.Types
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Util.Pretty qualified as Pretty

-- | Go to Definition handler
documentSymbolsHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentDocumentSymbol -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentDocumentSymbol) -> Lsp ()) -> Lsp ()
documentSymbolsHandler m respond = do
  respond . Right . maybe (InR . InL $ []) (InR . InL) =<< runMaybeT do
    let fileUri = m ^. params . textDocument . uri
    FileAnalysis {documentSymbols} <- FileAnalysis.getFileAnalysis fileUri
    ppe <- PPED.suffixifiedPPE <$> lift (ppedForFile fileUri)
    pure (asLspDocumentSymbols ppe <$> documentSymbols)

asLspDocumentSymbols :: PrettyPrintEnv -> UDocumentSymbol -> DocumentSymbol
asLspDocumentSymbols
  ppe
  UDocumentSymbol
    { symbolName,
      symbolSignature,
      symbolKind,
      symbolRange,
      symbolChildren
    } =
    DocumentSymbol
      { _name = Name.toText symbolName,
        _detail = do
          typ <- symbolSignature
          pure $ ": " <> (Text.pack $ TypePrinter.prettyStr typeWidth ppe typ),
        _kind = lspSymbolKind symbolKind,
        _tags = Just [],
        _deprecated = Nothing,
        _range = symbolRange,
        _selectionRange = symbolRange,
        _children = if null symbolChildren then Nothing else Just (asLspDocumentSymbols ppe <$> symbolChildren)
      }
    where
      typeWidth = Pretty.Width 120
      lspSymbolKind = \case
        DataDeclSymbol -> SymbolKind_Class
        EffectDeclSymbol -> SymbolKind_Interface
        TermSymbol -> SymbolKind_Function
