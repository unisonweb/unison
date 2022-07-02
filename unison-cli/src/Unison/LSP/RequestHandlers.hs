{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.RequestHandlers where

import Control.Lens hiding (List)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.String.Here.Uninterpolated (here)
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Text.FuzzyFind as Fuzzy
import qualified Unison.Codebase.Path as Path
import qualified Unison.HashQualified as HQ
import Unison.LSP.Types
import Unison.LSP.VFS
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import Unison.Server.Doc (Doc)
import qualified Unison.Server.Endpoints.FuzzyFind as FZF
import qualified Unison.Server.Syntax as Server
import qualified Unison.Server.Types as Backend

hoverHandler :: RequestMessage 'TextDocumentHover -> (Either ResponseError (ResponseResult 'TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m respond =
  respond . Right =<< runMaybeT do
    let p = (m ^. params)
    identifier <- MaybeT $ identifierAtPosition p
    cb <- asks codebase
    rt <- asks runtime
    results <- MaybeT . fmap eitherToMaybe $ (lspBackend $ Backend.prettyDefinitionsBySuffixes Path.empty Nothing Nothing (Backend.Suffixify True) rt cb [HQ.unsafeFromText identifier])
    liftIO . print $ ("HOVER" :: String, results)
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
    formatTermDefinition (Backend.TermDefinition {bestTermName, signature, termDocs}) =
      Text.unlines
        [ bestTermName <> " : " <> Text.pack (Server.toPlain signature),
          Text.intercalate "\n---\n" (termDocs <&> \(_name, _hash_, doc) -> docText doc)
        ]

    formatTypeDefinition :: Backend.TypeDefinition -> Text
    formatTypeDefinition (Backend.TypeDefinition {bestTypeName, typeDocs}) =
      Text.unlines
        [ bestTypeName,
          Text.intercalate "\n\n---\n\n" (typeDocs <&> \(_name, _hash_, doc) -> docText doc)
        ]

    -- TODO: Use a proper docs renderer
    docText :: Doc -> Text
    docText = tShow

completionHandler :: RequestMessage 'TextDocumentCompletion -> (Either ResponseError (ResponseResult 'TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m respond =
  respond =<< do
    mayPrefix <- completionPrefix (m ^. params)
    liftIO $ print ("COMPLETION" :: String, mayPrefix)
    case mayPrefix of
      Nothing -> pure . Right . InL . List $ []
      Just (range, prefix) -> do
        matches <- expand range prefix
        let isIncomplete = True -- TODO: be smarter about this
        pure . Right . InR . CompletionList isIncomplete . List $ snippetCompletions prefix range <> matches
  where
    resultToCompletion :: Range -> Text -> FZF.FoundResult -> CompletionItem
    resultToCompletion range prefix = \case
      FZF.FoundTermResult (FZF.FoundTerm {namedTerm = Backend.NamedTerm {termName, termType}}) -> do
        (mkCompletionItem termName)
          { _detail = (": " <>) . Text.pack . Server.toPlain <$> termType,
            _kind = Just CiVariable,
            _insertText = Text.stripPrefix prefix termName,
            _textEdit = Just $ CompletionEditText (TextEdit range termName)
          }
      FZF.FoundTypeResult (FZF.FoundType {namedType = Backend.NamedType {typeName, typeTag}}) ->
        let (detail, kind) = case typeTag of
              Backend.Ability -> ("Ability", CiInterface)
              Backend.Data -> ("Data", CiClass)
         in (mkCompletionItem typeName)
              { _detail = Just detail,
                _kind = Just kind
              }
    expand :: Range -> Text -> Lsp [CompletionItem]
    expand range prefix = do
      -- We should probably write a different fzf specifically for completion, but for now, it
      -- expects the unique pieces of the query to be different "words".
      let query = Text.unwords . Text.splitOn "." $ prefix
      cb <- asks codebase
      lspBackend (FZF.serveFuzzyFind cb Nothing Nothing Nothing Nothing (Just $ Text.unpack query)) >>= \case
        Left _be -> pure []
        Right results ->
          pure . fmap (resultToCompletion range prefix . snd) . take 15 . sortOn (Fuzzy.score . fst) $ results

snippetCompletions :: Text -> Range -> [CompletionItem]
snippetCompletions prefix range =
  [ ("handle", handlerTemplate)
  ]
    & filter (Text.isPrefixOf prefix . fst)
    & fmap toCompletion
  where
    toCompletion :: (Text, Text) -> CompletionItem
    toCompletion (pat, snippet) =
      (mkCompletionItem pat)
        { _insertTextFormat = Just Snippet,
          _insertTextMode = Just AdjustIndentation,
          _textEdit = Just $ CompletionEditText (TextEdit range snippet)
        }
    handlerTemplate =
      [here|
${1:handlerName} : v -> Request (${2:ability}) a -> a
${1:handlerName} storedValue = cases
  {${3} -> continue} -> do
    ${4}
    |]

mkCompletionItem :: Text -> CompletionItem
mkCompletionItem lbl =
  CompletionItem
    { _label = lbl,
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
