{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.Completion where

import Control.Lens hiding (List, (:<))
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.List.Extra (nubOrdOn)
import Data.Text qualified as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.LSP.Completion.Helpers (matchCompletions)
import Unison.LSP.FileAnalysis
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types
import Unison.LSP.VFS qualified as VFS
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.HashQualified' qualified as HQ' (toText)
import Unison.Syntax.Name qualified as Name (fromText, toText)
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Util.Pretty qualified as Pretty
import UnliftIO qualified

completionHandler :: RequestMessage 'TextDocumentCompletion -> (Either ResponseError (ResponseResult 'TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m respond =
  respond . maybe (Right $ InL mempty) (Right . InR) =<< runMaybeT do
    let fileUri = (m ^. params . textDocument . uri)
    (range, prefix) <- VFS.completionPrefix (m ^. params . textDocument . uri) (m ^. params . position)
    ppe <- PPED.suffixifiedPPE <$> lift (ppedForFile fileUri)
    codebaseCompletions <- lift getCodebaseCompletions
    fileComps <- (fileCompletions <$> getLastSuccessfulTypecheckAnalysis fileUri) <|> pure mempty
    let allCompletions = fileComps <> codebaseCompletions
    Config {maxCompletions} <- lift getConfig
    let defMatches = matchCompletions allCompletions prefix
    let (isIncomplete, defCompletions) =
          defMatches
            & nubOrdOn (\(p, _name, ref) -> (p, ref))
            & fmap (over _1 Path.toText)
            & case maxCompletions of
              Nothing -> (False,)
              Just n -> takeCompletions n
    let defCompletionItems =
          defCompletions
            & mapMaybe \(path, fqn, dep) ->
              let biasedPPE = PPE.biasTo [fqn] ppe
                  hqName = LD.fold (PPE.types biasedPPE) (PPE.terms biasedPPE) dep
               in hqName <&> \hqName -> mkDefCompletionItem fileUri range (HQ'.toName hqName) fqn path (HQ'.toText hqName) dep
    pure . CompletionList isIncomplete . List $ defCompletionItems
  where
    -- Takes at most the specified number of completions, but also indicates with a boolean
    -- whether there were more completions remaining so we can pass that along to the client.
    takeCompletions :: Int -> [a] -> (Bool, [a])
    takeCompletions 0 xs = (not $ null xs, [])
    takeCompletions _ [] = (False, [])
    takeCompletions n (x : xs) = second (x :) $ takeCompletions (pred n) xs

mkDefCompletionItem :: Uri -> Range -> Name -> Name -> Text -> Text -> LabeledDependency -> CompletionItem
mkDefCompletionItem fileUri range relativeName fullyQualifiedName path suffixified dep =
  CompletionItem
    { _label = lbl,
      _kind = case dep of
        LD.TypeReference _ref -> Just CiClass
        LD.TermReferent ref -> case ref of
          Referent.Con {} -> Just CiConstructor
          Referent.Ref {} -> Just CiValue,
      _tags = Nothing,
      _detail = Just (Name.toText fullyQualifiedName),
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Just path,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Just (CompletionEditText $ TextEdit range suffixified),
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _xdata = Just $ Aeson.toJSON $ CompletionItemDetails {dep, relativeName, fullyQualifiedName, fileUri}
    }
  where
    -- We should generally show the longer of the path or suffixified name in the label,
    -- it helps the user understand the difference between options which may otherwise look
    -- the same.
    --
    -- E.g. if I type "ma" then the suffixied options might be: List.map, Bag.map, but the
    -- path matches are just "map" and "map" since the query starts at that segment, so we
    -- show the suffixified version to disambiguate.
    --
    -- However, if the user types "base.List.ma" then the matching path is "base.List.map" and
    -- the suffixification is just "List.map", so we use the path in this case because it more
    -- closely matches what the user actually typed.
    --
    -- This is what's felt best to me, anecdotally.
    lbl =
      if Text.length path > Text.length suffixified
        then path
        else suffixified

-- | Called to resolve additional details for a completion item that the user is considering.
completionItemResolveHandler :: RequestMessage 'CompletionItemResolve -> (Either ResponseError CompletionItem -> Lsp ()) -> Lsp ()
completionItemResolveHandler message respond = do
  let completion :: CompletionItem
      completion = message ^. params
  respond . maybe (Right completion) Right =<< runMaybeT do
    case Aeson.fromJSON <$> (completion ^. xdata) of
      Just (Aeson.Success (CompletionItemDetails {dep, fullyQualifiedName, relativeName, fileUri})) -> do
        pped <- lift $ ppedForFile fileUri

        builtinsAsync <- liftIO . UnliftIO.async $ UnliftIO.evaluate IOSource.typecheckedFile
        checkBuiltinsReady <- liftIO do
          pure
            ( UnliftIO.poll builtinsAsync
                <&> ( \case
                        Nothing -> False
                        Just (Left {}) -> False
                        Just (Right {}) -> True
                    )
            )
        renderedDocs <-
          -- We don't want to block the type signature hover info if the docs are taking a long time to render;
          -- We know it's also possible to write docs that eval forever, so the timeout helps
          -- protect against that.
          lift (UnliftIO.timeout 2_000_000 (LSPQ.markdownDocsForFQN fileUri (HQ.NameOnly fullyQualifiedName)))
            >>= ( \case
                    Nothing ->
                      checkBuiltinsReady >>= \case
                        False -> pure ["\n---\n🔜 Doc renderer is initializing, try again in a few seconds."]
                        True -> pure ["\n---\n⏳ Timeout evaluating docs"]
                    Just [] -> pure []
                    -- Add some space from the type signature
                    Just xs@(_ : _) -> pure ("\n---\n" : xs)
                )
        case dep of
          LD.TermReferent ref -> do
            typ <- LSPQ.getTypeOfReferent fileUri ref
            let renderedType = ": " <> (Text.pack $ TypePrinter.prettyStr (Just typeWidth) (PPED.suffixifiedPPE pped) typ)
            let doc = CompletionDocMarkup $ toMarkup (Text.unlines $ ["```unison", Name.toText fullyQualifiedName, "```"] ++ renderedDocs)
            pure $ (completion {_detail = Just renderedType, _documentation = Just doc} :: CompletionItem)
          LD.TypeReference ref ->
            case ref of
              Reference.Builtin {} -> do
                let renderedBuiltin = ": <builtin>"
                let doc = CompletionDocMarkup $ toMarkup (Text.unlines $ ["```unison", Name.toText fullyQualifiedName, "```"] ++ renderedDocs)
                pure $ (completion {_detail = Just renderedBuiltin, _documentation = Just doc} :: CompletionItem)
              Reference.DerivedId refId -> do
                decl <- LSPQ.getTypeDeclaration fileUri refId
                let renderedDecl = ": " <> (Text.pack . Pretty.toPlain typeWidth . Pretty.syntaxToColor $ DeclPrinter.prettyDecl pped ref (HQ.NameOnly relativeName) decl)
                let doc = CompletionDocMarkup $ toMarkup (Text.unlines $ ["```unison", Name.toText fullyQualifiedName, "```"] ++ renderedDocs)
                pure $ (completion {_detail = Just renderedDecl, _documentation = Just doc} :: CompletionItem)
      _ -> empty
  where
    toMarkup txt = MarkupContent {_kind = MkMarkdown, _value = txt}
    -- Completion windows can be very small, so this seems like a good default
    typeWidth = Pretty.Width 20

-- | Data which will be provided back to us in the completion resolve handler when the user considers this completion.
data CompletionItemDetails = CompletionItemDetails
  { dep :: LD.LabeledDependency,
    relativeName :: Name,
    fullyQualifiedName :: Name,
    fileUri :: Uri
  }

instance Aeson.ToJSON CompletionItemDetails where
  toJSON CompletionItemDetails {dep, relativeName, fullyQualifiedName, fileUri} =
    Aeson.object
      [ "relativeName" Aeson..= Name.toText relativeName,
        "fullyQualifiedName" Aeson..= Name.toText fullyQualifiedName,
        "fileUri" Aeson..= fileUri,
        "dep" Aeson..= ldJSON dep
      ]
    where
      ldJSON :: LD.LabeledDependency -> Aeson.Value
      ldJSON = \case
        LD.TypeReference ref -> Aeson.object ["kind" Aeson..= ("type" :: Text), "ref" Aeson..= Reference.toText ref]
        LD.TermReferent ref -> Aeson.object ["kind" Aeson..= ("term" :: Text), "ref" Aeson..= Referent.toText ref]

instance Aeson.FromJSON CompletionItemDetails where
  parseJSON = Aeson.withObject "CompletionItemDetails" \obj -> do
    dep <- ((obj Aeson..: "dep") >>= ldParser)
    relativeName <- (obj Aeson..: "relativeName" >>= maybe (fail "Invalid name in CompletionItemDetails") pure . Name.fromText)
    fullyQualifiedName <- (obj Aeson..: "fullyQualifiedName" >>= maybe (fail "Invalid name in CompletionItemDetails") pure . Name.fromText)
    fileUri <- obj Aeson..: "fileUri"
    pure $ CompletionItemDetails {..}
    where
      ldParser :: Aeson.Value -> Aeson.Parser LD.LabeledDependency
      ldParser = Aeson.withObject "LabeledDependency" \obj -> do
        kind <- obj Aeson..: "kind"
        case kind of
          ("type" :: Text) -> LD.TypeReference <$> (obj Aeson..: "ref" >>= either (const $ fail "Invalid Reference in LabeledDependency") pure . Reference.fromText)
          ("term" :: Text) -> LD.TermReferent <$> (obj Aeson..: "ref" >>= maybe (fail "Invalid Referent in LabeledDependency") pure . Referent.fromText)
          _ -> fail "Invalid LabeledDependency kind"
