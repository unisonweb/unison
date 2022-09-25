{-# LANGUAGE DataKinds #-}

module Unison.LSP.Formatting where

import Control.Lens hiding (List)
import qualified Data.List.NonEmpty.Extra as NEL
import qualified Data.Text as Text
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens hiding (id, to)
import qualified Unison.ABT as ABT
import qualified Unison.Codebase.Path as Path
import qualified Unison.Debug as Debug
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getFileAnalysis, ppedForFile)
import Unison.LSP.Types
import qualified Unison.Name as Name
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.Syntax.TermPrinter as TermPrinter
import Unison.UnisonFile (UnisonFile (..))
import qualified Unison.Util.Pretty as Pretty

-- | TODO: Add an LSP option for formatting width
prettyPrintWidth :: Pretty.Width
prettyPrintWidth = 80

formatDocRequest :: RequestMessage 'TextDocumentFormatting -> (Either ResponseError (List TextEdit) -> Lsp ()) -> Lsp ()
formatDocRequest m respond = do
  Debug.debugLogM Debug.LSP "Formatting!!"
  edits <- formatDefs (m ^. params . textDocument . uri)
  Debug.debugM Debug.LSP "Formatting Edits" edits
  respond . Right . List $ edits

-- | Return a folding range for each top-level definition
formatDefs :: Uri -> Lsp [TextEdit]
formatDefs fileUri =
  fromMaybe []
    <$> runMaybeT do
      cwd <- lift getCurrentPath
      FileAnalysis {parsedFile} <- getFileAnalysis fileUri
      UnisonFileId {terms} <- MaybeT $ pure parsedFile
      filePPED <- ppedForFile fileUri
      for terms \(sym, trm) -> do
        symName <- hoistMaybe (Name.fromVar sym)
        let defNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let defName = Name.fromSegments defNameSegments
        let biasedPPED = PPED.biasTo [defName] filePPED
        -- We use unsuffixified here in an attempt to keep names within the file the same
        let biasedPPE = PPED.unsuffixifiedPPE biasedPPED
        let formatted = Pretty.toPlain prettyPrintWidth $ TermPrinter.pretty biasedPPE trm
        editRange <- hoistMaybe . annToRange $ ABT.annotation trm
        let edit = TextEdit editRange (Text.pack formatted)
        pure edit
