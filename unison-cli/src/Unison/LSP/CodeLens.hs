{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Unison.LSP.CodeLens where

import Control.Lens hiding (List)
import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens hiding (error)
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.Codebase.Editor.Slurp qualified as Slurp
import Unison.Codebase.Editor.SlurpComponent qualified as SC
import Unison.Codebase.Editor.SlurpResult qualified as SR
import Unison.HashQualified qualified as HQ
import Unison.LSP.Commands (TextReplacement (TextReplacement), replaceText)
import Unison.LSP.Commands qualified as Commands
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.FileAnalysis
import Unison.LSP.Types
import Unison.LSP.Types qualified as Lsp
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.UnisonFile.Summary qualified as UF
import Unison.Util.Pretty qualified as CT

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
    TypeSigInsertion
      <$> o
        Aeson..: "range"
      <*> o
        Aeson..: "typeSignature"
      <*> o
        Aeson..: "fileUri"

-- | Computes code actions for a document.
codeLensHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentCodeLens -> (Either Msg.ResponseError ([CodeLens] |? Null) -> Lsp ()) -> Lsp ()
codeLensHandler m respond =
  respond . maybe (Right $ InL mempty) Right =<< runMaybeT do
    let fileUri = m ^. params . textDocument . uri
    FileAnalysis {typeSignatureHints} <- getFileAnalysis fileUri
    slurpLenses <- lift $ mkSlurpLenses fileUri
    typeSigLenses <- ifor typeSignatureHints \_v (TypeSignatureHint name ref range typ) -> do
      ppe <- PPED.suffixifiedPPE <$> lift (ppedForFile fileUri)
      let rendered = case TypePrinter.prettySignaturesCT ppe [(ref, HQ.NameOnly name, typ)] of
            [sig] -> Text.pack . CT.toPlain 80 $ sig
            _ -> error "codeLensHandler: prettySignaturesCT returned more than one signature"
      let insertLocation =
            range
              & start . character .~ 0
              & end . character .~ 0
      pure $
        CodeLens
          range
          (Just $ replaceText rendered $ TextReplacement insertLocation "Insert type signature" (rendered <> "\n") fileUri)
          Nothing
    pure (InL $ Map.elems typeSigLenses <> slurpLenses)

data AddOrUpdate = Add | Update

mkSlurpLenses :: Uri -> Lsp [CodeLens]
mkSlurpLenses uri = do
  fmap (fromMaybe []) . runMaybeT $ do
    FileAnalysis {typecheckedFile} <- getFileAnalysis uri
    fileSummary <- getFileSummary uri
    let locations = UF.fileDefLocations fileSummary
    tf <- hoistMaybe typecheckedFile
    currentNames <- lift Lsp.getCurrentNames
    let SR.SlurpResult {adds, updates} = Slurp.slurpFile tf mempty Slurp.CheckOp currentNames
    let lenses =
          ((fmap (Add,) . Set.toList $ SC.slurpComponentSymbols adds) <> (fmap (Update,) . Set.toList $ SC.slurpComponentSymbols updates))
            & foldMap \(op, var) -> do
              insertLocations <- maybeToList $ Map.lookup var locations
              (Set.toList insertLocations)
                & mapMaybe Cv.annToRange
                & foldMap \loc -> do
                  let lspRange =
                        loc
                          & start . character .~ 0
                          & end . character .~ 0
                  pure $
                    case op of
                      Add -> CodeLens lspRange (Just $ Commands.addCommand var) Nothing
                      Update -> CodeLens lspRange (Just $ Commands.updateCommand var) Nothing
    pure lenses
