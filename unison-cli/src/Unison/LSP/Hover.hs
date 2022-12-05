{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Unison.LSP.Hover where

import Control.Lens hiding (List)
import Control.Monad.Reader
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.ABT as ABT
import qualified Unison.HashQualified as HQ
import Unison.LSP.FileAnalysis (ppedForFile)
import qualified Unison.LSP.Queries as LSPQ
import Unison.LSP.Types
import qualified Unison.LSP.VFS as VFS
import qualified Unison.LabeledDependency as LD
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.Reference as Reference
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.DeclPrinter as DeclPrinter
import qualified Unison.Syntax.Name as Name
import qualified Unison.Syntax.TypePrinter as TypePrinter
import qualified Unison.Term as Term
import qualified Unison.Util.Pretty as Pretty

-- | Hover help handler
--
-- TODO:
--   * Add docs
--   * Resolve fqn on hover
hoverHandler :: RequestMessage 'TextDocumentHover -> (Either ResponseError (ResponseResult 'TextDocumentHover) -> Lsp ()) -> Lsp ()
hoverHandler m respond =
  respond . Right =<< runMaybeT do
    let pos = (m ^. params . position)
    hoverTxt <- hoverInfo (m ^. params . textDocument . uri) pos
    pure $
      Hover
        { _contents = HoverContents (MarkupContent MkMarkdown hoverTxt),
          _range = Nothing -- TODO add range info
        }

hoverInfo :: Uri -> Position -> MaybeT Lsp Text
hoverInfo uri pos =
  markdownify <$> (hoverInfoForRef <|> hoverInfoForLiteral)
  where
    markdownify :: Text -> Text
    markdownify rendered = Text.unlines ["```unison", rendered, "```"]
    prettyWidth :: Pretty.Width
    prettyWidth = 40
    hoverInfoForRef :: MaybeT Lsp Text
    hoverInfoForRef = do
      symAtCursor <- VFS.identifierAtPosition uri pos
      ref <- LSPQ.refAtPosition uri pos
      pped <- lift $ ppedForFile uri
      case ref of
        LD.TypeReference (Reference.Builtin {}) -> pure (symAtCursor <> " : <builtin>")
        LD.TypeReference ref@(Reference.DerivedId refId) -> do
          nameAtCursor <- MaybeT . pure $ Name.fromText symAtCursor
          decl <- LSPQ.getTypeDeclaration uri refId
          let typ = Text.pack . Pretty.toPlain prettyWidth . Pretty.syntaxToColor $ DeclPrinter.prettyDecl pped ref (HQ.NameOnly nameAtCursor) decl
          pure typ
        LD.TermReferent ref -> do
          typ <- LSPQ.getTypeOfReferent uri ref
          let renderedType = Text.pack $ TypePrinter.prettyStr (Just prettyWidth) (PPED.suffixifiedPPE pped) typ
          pure (symAtCursor <> " : " <> renderedType)
    hoverInfoForLiteral :: MaybeT Lsp Text
    hoverInfoForLiteral = do
      LSPQ.nodeAtPosition uri pos >>= \case
        Left term -> do
          typ <- hoistMaybe $ builtinTypeForLiterals term
          pure (": " <> typ)
        Right {} -> empty

    hoistMaybe :: Maybe a -> MaybeT Lsp a
    hoistMaybe = MaybeT . pure

-- | Get the type for term literals.
builtinTypeForLiterals :: Term.Term Symbol Ann -> Maybe Text
builtinTypeForLiterals term =
  case ABT.out term of
    ABT.Tm f -> case f of
      Term.Int {} -> Just "Int"
      Term.Nat {} -> Just "Nat"
      Term.Float {} -> Just "Float"
      Term.Boolean {} -> Just "Boolean"
      Term.Text {} -> Just "Text"
      Term.Char {} -> Just "Char"
      Term.Blank {} -> Nothing
      Term.Ref {} -> Nothing
      Term.Constructor {} -> Nothing
      Term.Request {} -> Nothing
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
    ABT.Var {} -> Nothing
    ABT.Cycle {} -> Nothing
    ABT.Abs {} -> Nothing
