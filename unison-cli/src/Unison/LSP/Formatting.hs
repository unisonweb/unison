{-# LANGUAGE DataKinds #-}

module Unison.LSP.Formatting where

import Control.Lens hiding (List)
import qualified Data.List as List
import qualified Data.List.NonEmpty.Extra as NEL
import qualified Data.Map as Map
import qualified Data.Text as Text
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens hiding (id, to)
import qualified Unison.ABT as ABT
import qualified Unison.Codebase.Path as Path
import qualified Unison.DataDeclaration as Decl
import qualified Unison.HashQualified as HQ
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getFileAnalysis, ppedForFile)
import Unison.LSP.Types
import qualified Unison.Lexer.Pos as L
import qualified Unison.Name as Name
import qualified Unison.Parser.Ann as Ann
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.Reference as Reference
import qualified Unison.Syntax.DeclPrinter as DeclPrinter
import qualified Unison.Syntax.TermPrinter as TermPrinter
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Monoid as Monoid
import qualified Unison.Util.Pretty as Pretty

formatDocRequest :: RequestMessage 'TextDocumentFormatting -> (Either ResponseError (List TextEdit) -> Lsp ()) -> Lsp ()
formatDocRequest m respond = do
  edits <- formatDefs (m ^. params . textDocument . uri)
  respond . Right . List $ edits

-- | Return a folding range for each top-level definition
formatDefs :: Uri -> Lsp [TextEdit]
formatDefs fileUri =
  fromMaybe []
    <$> runMaybeT do
      cwd <- lift getCurrentPath
      FileAnalysis {typecheckedFile, parsedFile, lexedSource = (src, _)} <- getFileAnalysis fileUri
      (datas, effects, terms) <- case (typecheckedFile, parsedFile) of
        (Just (UF.TypecheckedUnisonFileId {dataDeclarationsId', effectDeclarationsId', hashTermsId}), _) -> pure (dataDeclarationsId', effectDeclarationsId', hashTermsId ^@.. ifolded <. _3)
        (_, Just (UF.UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms, watches})) -> pure (dataDeclarationsId, effectDeclarationsId, terms <> fold watches)
        (Nothing, Nothing) -> empty
      filePPED <- ppedForFile fileUri
      let decls = Map.toList (fmap Right <$> datas) <> Map.toList (fmap Left <$> effects)
      formattedDecls <- for decls \(sym, (ref, decl)) -> do
        symName <- hoistMaybe (Name.fromVar sym)
        let declNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let declName = Name.fromSegments declNameSegments
        let hqName = HQ.fromName symName
        let biasedPPED = PPED.biasTo [declName] filePPED
        pure $ (either (Decl.annotation . Decl.toDataDecl) (Decl.annotation) decl, DeclPrinter.prettyDecl biasedPPED (Reference.DerivedId ref) hqName decl)
      formattedTerms <- for terms \(sym, trm) -> do
        symName <- hoistMaybe (Name.fromVar sym)
        let defNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let defName = Name.fromSegments defNameSegments
        let hqName = HQ.NameOnly symName
        let biasedPPED = PPED.biasTo [defName] filePPED
        -- We use unsuffixified here in an attempt to keep names within the file the same
        let biasedPPE = PPED.suffixifiedPPE biasedPPED
        let formatted = TermPrinter.prettyBinding biasedPPE hqName trm
        pure (ABT.annotation trm, formatted)

      -- Only keep definitions which are _actually_ in the file, skipping generated accessors
      -- and such.
      let filteredDefs =
            (formattedTerms <> formattedDecls)
              & filter
                ( \(ann, _) -> case ann of
                    Ann.Ann {} -> True
                    _ -> False
                )
      defsRange <- hoistMaybe $
        case foldMap fst filteredDefs of
          Ann.Ann _ end -> annToRange (Ann.Ann mempty end)
          _ -> annToRange $ Ann.Ann mempty (L.Pos (succ . Prelude.length . Text.lines $ src) 0)
      when (null filteredDefs) empty {- Don't format if we have no definitions or it wipes out the fold! -}
      Config {formattingWidth} <- lift getConfig
      filteredDefs
        & List.sortOn fst -- Sort defs in the order they were parsed.
        & Monoid.intercalateMap "\n\n" (Pretty.toPlain (Pretty.Width formattingWidth) . Pretty.syntaxToColor . snd)
        & (\txt -> [TextEdit defsRange (Text.pack txt)])
        & pure
