{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Unison.LSP.Hover where

import Control.Lens hiding (List)
import Control.Monad.Reader
import qualified Data.IntervalMap.Lazy as IM
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens hiding (to)
import qualified Unison.ABT as ABT
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Path as Path
import Unison.ConstructorReference (ConstructorReference)
import qualified Unison.HashQualified as HQ
import Unison.LSP.Conversions (annToInterval)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.LSP.VFS
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Server.Backend as Backend
import qualified Unison.Server.Syntax as Server
import qualified Unison.Server.Types as Backend
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Syntax.TypePrinter as TypePrinter
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
import Unison.Util.List (safeHead)

-- | Rudimentary hover handler
--
-- TODO: Add docs, use FileAnalysis to select hover target.
hoverHandler :: RequestMessage 'TextDocumentHover -> (Either ResponseError (ResponseResult 'TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m respond =
  respond . Right =<< runMaybeT do
    let p = (m ^. params)
    txtIdentifier <- MaybeT $ identifierAtPosition p
    hqIdentifier <- MaybeT . pure $ HQ.fromText txtIdentifier
    cb <- asks codebase
    rt <- asks runtime
    results <- MaybeT . fmap eitherToMaybe $ (lspBackend $ Backend.prettyDefinitionsForHQName Path.empty Nothing Nothing (Backend.Suffixify True) rt cb hqIdentifier)
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

hoverInfo :: Uri -> Position -> MaybeT Lsp Text
hoverInfo uri p = do
  FileAnalysis {tokenMap, parsedFile} <- MaybeT $ getFileAnalysis uri
  subTermMap <- subTermMap <$> MaybeT (pure parsedFile)
  matchingHoverInfo <- MaybeT . pure . safeHead . IM.elems $ IM.containing subTermMap p
  matchingToken <- MaybeT . pure . safeHead . IM.elems $ IM.containing tokenMap p
  case matchingHoverInfo of
    SimpleHint txt -> pure txt
    Ref ref -> do
      Env {codebase} <- ask
      typ <- MaybeT . liftIO $ Codebase.getTypeOfReferent codebase ref
      ppe <- lift $ globalPPE
      pure . Text.pack $ TypePrinter.prettyStr (Just 40) (PPED.suffixifiedPPE ppe) typ

subTermMap :: Parser.Annotated a => UF.UnisonFile v a -> IM.IntervalMap Position HoverInfo
subTermMap (UF.UnisonFileId {terms}) =
  terms ^@.. (folded . _2 . subTerms . (reindexed ABT.annotation selfIndex) <. termHoverInfo)
    & mapMaybe (\(a, trm) -> IM.singleton <$> annToInterval (Parser.ann a) <*> pure trm)
    & fold

subTerms :: Fold (Term.Term v a) (Term.Term v a)
subTerms =
  cosmosOf (to ABT.out . folded)

data HoverInfo
  = SimpleHint Text
  | Ref Referent

termHoverInfo :: Fold (Term.Term v a) HoverInfo
termHoverInfo = folding \term ->
  case ABT.out term of
    ABT.Tm f -> case f of
      Term.Int {} -> Just (SimpleHint "Int")
      Term.Nat {} -> Just (SimpleHint "Nat")
      Term.Float {} -> Just (SimpleHint "Float")
      Term.Boolean {} -> Just (SimpleHint "Boolean")
      Term.Text {} -> Just (SimpleHint "Text")
      Term.Char {} -> Just (SimpleHint "Char")
      Term.Blank {} -> Nothing
      Term.Ref ref -> Just (Ref ref)
      Term.Constructor cRef -> Just (ConstructorRef cRef)
      Term.Request cRef -> Just (ConstructorRef cRef)
      Term.Handle {} -> Nothing
      Term.App {} -> Nothing
      Term.Ann {} -> Nothing
      Term.List {} -> Nothing
      Term.If {} -> Nothing
      Term.And {} -> Nothing
      Term.Or {} -> Nothing
      Term.Lam {} -> Nothing
      Term.LetRec {} -> Nothing
      Term.Let {} -> Nothing
      Term.Match {} -> Nothing
      Term.TermLink {} -> Nothing
      Term.TypeLink {} -> Nothing
    _ -> Nothing
