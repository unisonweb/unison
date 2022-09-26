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
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.TermPrinter as TermPrinter
import Unison.Term hiding (Ann, List)
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
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
      FileAnalysis {typecheckedFile} <- getFileAnalysis fileUri
      UF.TypecheckedUnisonFileId {topLevelComponents'} <- MaybeT $ pure typecheckedFile
      filePPED <- ppedForFile fileUri
      for (concat topLevelComponents') \(sym, topLevelTerm, _typ) -> do
        termToPrint <- hoistMaybe $ case topLevelTerm of
          Term.Ann' trm _typ -> Just trm
          Term.Lam' {} -> toList $ ABT.out topLevelTerm
          trm -> Just trm
        symName <- hoistMaybe (Name.fromVar sym)
        let defNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let defName = Name.fromSegments defNameSegments
        let biasedPPED = PPED.biasTo [defName] filePPED
        let biasedPPE = PPED.suffixifiedPPE biasedPPED
        let formatted = Pretty.toPlain prettyPrintWidth $ TermPrinter.pretty biasedPPE termToPrint
        -- This is an unfortunate hack; we need to fix annotations so they actually represent the span of the term.
        -- for now this 'folds' all annotations so we hopefully cover the whole term, but ideally the top-level ann
        -- would actually just contain the whole term.
        editRange <- hoistMaybe . annToRange $ fold termToPrint
        let edit = TextEdit editRange (Text.pack formatted)
        Debug.debugM Debug.LSP "DEBUGTERMS" (debugTerms termToPrint)
        pure edit

addASTName :: Term Symbol Ann -> String
addASTName = show . ABT.out

debugTerms :: Term Symbol Ann -> [(Ann, Term Symbol Ann)]
debugTerms tm =
  toListOf
    ( cosmosOf
        ( folding
            (\(_, tm) -> ((ABT.annotation tm,) <$> toList (ABT.out tm)))
        )
    )
    (ABT.annotation tm, tm)
