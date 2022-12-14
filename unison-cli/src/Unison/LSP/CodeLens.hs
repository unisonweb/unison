{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.CodeLens where

import Control.Lens hiding (List)
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.HashQualified as HQ
import Unison.LSP.Commands (TextReplacement (TextReplacement), replaceText)
import Unison.LSP.FileAnalysis
import Unison.LSP.Types
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.Syntax.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as CT

data TypeSigInsertion = TypeSigInsertion
  { range :: Range,
    typeSignature :: Text,
    fileUri :: Uri
  }

instance Aeson.ToJSON TypeSigInsertion where
  toJSON (TypeSigInsertion range typeSignature fileUri) =
    Aeson.object
      [ "range" Aeson..= range,
        "typeSignature" Aeson..= typeSignature,
        "fileUri" Aeson..= fileUri
      ]

instance Aeson.FromJSON TypeSigInsertion where
  parseJSON = Aeson.withObject "TypeSigInsertion" $ \o ->
    TypeSigInsertion <$> o Aeson..: "range"
      <*> o Aeson..: "typeSignature"
      <*> o Aeson..: "fileUri"

-- | Computes code actions for a document.
codeLensHandler :: RequestMessage 'TextDocumentCodeLens -> (Either ResponseError (List CodeLens) -> Lsp ()) -> Lsp ()
codeLensHandler m respond =
  respond . maybe (Right mempty) Right =<< runMaybeT do
    let fileUri = m ^. params . textDocument . uri
    FileAnalysis {typeSignatureHints} <- MaybeT $ getFileAnalysis fileUri
    codeLenses <- ifor typeSignatureHints \_v (TypeSignatureHint name ref startPos typ) -> do
      ppe <- PPED.suffixifiedPPE <$> lift (ppedForFile fileUri)
      let [rendered] = Text.pack . CT.toPlain 80 <$> TypePrinter.prettySignaturesCT ppe [(ref, HQ.NameOnly name, typ)]
      let prevLine = Range (startPos {_character = 0}) (startPos {_character = 0})
      pure $
        CodeLens
          (Range startPos (startPos {_character = 999}))
          (Just $ replaceText rendered $ TextReplacement prevLine "Insert type signature" (rendered <> "\n") fileUri)
          Nothing
    pure (List (Map.elems codeLenses))
