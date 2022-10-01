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
import qualified Unison.ConstructorType as CT
import qualified Unison.Debug as Debug
import Unison.LSP.Conversions (annToInterval)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Lexer as Lex
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Syntax.TypePrinter as TypePrinter
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
import Unison.Util.List (safeHead)
import qualified Unison.Var as Var

-- | Hover help handler
--
-- TODO: Add docs
hoverHandler :: RequestMessage 'TextDocumentHover -> (Either ResponseError (ResponseResult 'TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m respond =
  respond . Right =<< runMaybeT do
    let pos = (m ^. params . position)
    -- let p = (m ^. params)
    -- txtIdentifier <- MaybeT $ identifierAtPosition p
    -- hqIdentifier <- MaybeT . pure $ HQ.fromText txtIdentifier
    -- cb <- asks codebase
    -- rt <- asks runtime

    hoverTxt <- hoverInfo (m ^. params . textDocument . uri) pos
    -- results <- MaybeT . fmap eitherToMaybe $ (lspBackend $ Backend.prettyDefinitionsForHQName Path.empty Nothing Nothing (Backend.Suffixify True) rt cb hqIdentifier)
    -- let termResults = formatTermDefinition <$> toList (Backend.termDefinitions results)
    -- let typeResults = formatTypeDefinition <$> toList (Backend.typeDefinitions results)
    -- let markup = Text.intercalate "\n\n---\n\n" $ termResults <> typeResults
    pure $
      Hover
        { _contents = HoverContents (MarkupContent MkPlainText hoverTxt),
          _range = Nothing -- TODO add range info
        }
  where

-- formatTermDefinition :: Backend.TermDefinition -> Text
-- formatTermDefinition (Backend.TermDefinition {bestTermName, signature}) =
--   bestTermName <> " : " <> Text.pack (Server.toPlain signature)

-- formatTypeDefinition :: Backend.TypeDefinition -> Text
-- formatTypeDefinition (Backend.TypeDefinition {bestTypeName}) = bestTypeName

hoverInfo :: Uri -> Position -> MaybeT Lsp Text
hoverInfo uri p = do
  Debug.debugM Debug.LSP "POINT" p
  FileAnalysis {tokenMap, typecheckedFile} <- MaybeT $ getFileAnalysis uri
  Debug.debugM Debug.LSP "TYPECHECKED" typecheckedFile
  subTermMap <- mkSubTermMap <$> MaybeT (pure typecheckedFile)
  Debug.debugM Debug.LSP "SubTerms" subTermMap
  let matchingHoverInfos = concat . IM.elems $ IM.containing subTermMap p
  let matchingLexeme = IM.elems $ IM.containing tokenMap p
  Debug.debugM Debug.LSP "Matching" matchingHoverInfos
  renderedTypes <- for matchingHoverInfos \info -> do
    case info of
      BuiltinType txt -> pure txt
      LocalVar _v -> pure $ "<local>"
      Ref ref -> do
        Env {codebase} <- ask
        typ <- MaybeT . liftIO $ Codebase.getTypeOfReferent codebase ref
        ppe <- lift $ globalPPE
        pure . Text.pack $ TypePrinter.prettyStr (Just 40) (PPED.suffixifiedPPE ppe) typ
  Debug.debugM Debug.LSP "Rendered" renderedTypes
  -- Due to the way hover info is computed, there should be at most one.
  typ <- MaybeT . pure $ safeHead renderedTypes
  MaybeT . pure $ case listToMaybe matchingLexeme of
    Just (Lex.WordyId n _) -> Just $ Text.pack n <> " : " <> typ
    Just (Lex.SymbolyId n _) -> Just $ Text.pack n <> " : " <> typ
    Just (Lex.Textual _) -> Just $ "<text literal> : " <> typ
    Just (Lex.Character _) -> Just $ "<char literal> : " <> typ
    Just (Lex.Numeric _) -> Just $ "<numeric literal> : " <> typ
    Just (Lex.Bytes _) -> Just $ "<byte literal> : " <> typ
    -- TODO: add other lexemes
    _ -> Nothing

mkSubTermMap :: (Parser.Annotated a, Show a) => UF.TypecheckedUnisonFile Symbol a -> IM.IntervalMap Position [HoverInfo]
mkSubTermMap (UF.TypecheckedUnisonFileId {hashTermsId}) =
  hashTermsId ^@.. (folded . _3 . subTerms . (reindexed ABT.annotation selfIndex) <. termHoverInfo)
    & Debug.debug Debug.LSP "Cosmos'd"
    & map (\(a, info) -> IM.singleton <$> annToInterval (Parser.ann a) <*> pure [info])
    & Debug.debug Debug.LSP "Converted1"
    & mapMaybe Prelude.id
    & Debug.debug Debug.LSP "Converted2"
    & IM.unionsWith (<>)

subTerms :: Fold (Term.Term v a) (Term.Term v a)
subTerms =
  cosmosOf (to ABT.out . folded)

data HoverInfo
  = BuiltinType Text
  | LocalVar Text
  | Ref Referent
  deriving stock (Show, Eq, Ord)

termHoverInfo :: Fold (Term.Term Symbol a) HoverInfo
termHoverInfo = folding \term ->
  case ABT.out term of
    ABT.Tm f -> case f of
      Term.Int {} -> Just (BuiltinType "Int")
      Term.Nat {} -> Just (BuiltinType "Nat")
      Term.Float {} -> Just (BuiltinType "Float")
      Term.Boolean {} -> Just (BuiltinType "Boolean")
      Term.Text {} -> Just (BuiltinType "Text")
      Term.Char {} -> Just (BuiltinType "Char")
      Term.Blank {} -> Nothing
      Term.Ref ref -> Just (Ref $ Referent.Ref ref)
      Term.Constructor cRef -> Just (Ref $ Referent.Con cRef CT.Data)
      Term.Request cRef -> Just (Ref $ Referent.Con cRef CT.Effect)
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
    -- ABT.Abs v r -> case f of
    ABT.Var v -> Just (LocalVar $ Var.name v)
    _ -> Nothing
