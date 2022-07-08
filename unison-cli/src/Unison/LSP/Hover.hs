{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Unison.LSP.Hover where

import Control.Lens hiding (List)
import Control.Monad.Reader
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.Codebase.Path as Path
import qualified Unison.HashQualified as HQ
import Unison.LSP.Types
import Unison.LSP.VFS
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.Syntax as Server
import qualified Unison.Server.Types as Backend

-- | Rudimentary hover handler
--
-- TODO: Add docs, use FileAnalysis to select hover target.
hoverHandler :: RequestMessage 'TextDocumentHover -> (Either ResponseError (ResponseResult 'TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m respond =
  respond . Right =<< runMaybeT do
    let p = (m ^. params)
    identifier <- MaybeT $ identifierAtPosition p
    cb <- asks codebase
    rt <- asks runtime
    results <- MaybeT . fmap eitherToMaybe $ (lspBackend $ Backend.prettyDefinitionsBySuffixes Path.empty Nothing Nothing (Backend.Suffixify True) rt cb [HQ.unsafeFromText identifier])
    let termResults = formatTermDefinition <$> toList (Backend.termDefinitions results)
    let typeResults = formatTypeDefinition <$> toList (Backend.typeDefinitions results)
    let markup = Text.intercalate "\n\n---\n\n" $ termResults <> typeResults
    pure $
      Hover
        { _contents = HoverContents (MarkupContent MkPlainText markup),
          _range = Nothing -- TODO add range info
        }
  where
    formatTermDefinition :: Backend.TermDefinition -> Text
    formatTermDefinition (Backend.TermDefinition {bestTermName, signature}) =
      bestTermName <> " : " <> Text.pack (Server.toPlain signature)

    formatTypeDefinition :: Backend.TypeDefinition -> Text
    formatTypeDefinition (Backend.TypeDefinition {bestTypeName}) = bestTypeName
