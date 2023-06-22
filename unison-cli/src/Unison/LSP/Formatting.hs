{-# LANGUAGE DataKinds #-}

module Unison.LSP.Formatting where

import Control.Lens hiding (List)
import Data.List qualified as List
import Data.List.NonEmpty.Extra qualified as NEL
import Data.Map qualified as Map
import Data.Text qualified as Text
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens hiding (id, to)
import Unison.ABT qualified as ABT
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as Decl
import Unison.HashQualified qualified as HQ
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getFileAnalysis, ppedForFile)
import Unison.LSP.Types
import Unison.Lexer.Pos qualified as L
import Unison.Name qualified as Name
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Symbol qualified as Symbol
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term qualified as Term
import Unison.UnisonFile qualified as UF
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as Pretty
import Unison.Var qualified as Var

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
      (datas, effects, termsAndWatches) <- case (typecheckedFile, parsedFile) of
        (Just (UF.TypecheckedUnisonFileId {dataDeclarationsId', effectDeclarationsId', hashTermsId}), _) -> do
          let termsWithWatchKind =
                Map.toList hashTermsId
                  <&> \(sym, (_id, wk, tm, _typ)) -> (sym, tm, wk)
          pure (dataDeclarationsId', effectDeclarationsId', termsWithWatchKind)
        (_, Just (UF.UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms, watches})) -> do
          -- Currently we can't correctly print Record types without a successful typecheck, so we just bail on printing
          -- entirely if a record might be present.
          -- We also can't easily determine whether a given type is actually a record.
          when (not . null $ dataDeclarationsId) empty
          let termsWithKind = terms <&> \(sym, trm) -> (sym, trm, Nothing)
          let watchesWithKind = watches & ifoldMap \wk exprs -> exprs <&> \(sym, trm) -> (sym, trm, Just wk)
          pure (dataDeclarationsId, effectDeclarationsId, termsWithKind <> watchesWithKind)
        (Nothing, Nothing) -> empty
      filePPED <- lift $ ppedForFile fileUri
      let decls = Map.toList (fmap Right <$> datas) <> Map.toList (fmap Left <$> effects)
      formattedDecls <- for decls \(sym, (ref, decl)) -> do
        symName <- hoistMaybe (Name.fromVar sym)
        let declNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let declName = Name.fromSegments declNameSegments
        let hqName = HQ.fromName symName
        let biasedPPED = PPED.biasTo [declName] filePPED
        pure $
          (either (Decl.annotation . Decl.toDataDecl) (Decl.annotation) decl, DeclPrinter.prettyDecl biasedPPED (Reference.DerivedId ref) hqName decl)
            & over _2 Pretty.syntaxToColor
      formattedTerms <- for termsAndWatches \(sym, trm, wk) -> do
        symName <- hoistMaybe (Name.fromVar sym)
        let defNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let defName = Name.fromSegments defNameSegments
        let hqName = HQ.NameOnly symName
        let biasedPPED = PPED.biasTo [defName] filePPED
        -- We use unsuffixified here in an attempt to keep names within the file the same
        let biasedPPE = PPED.suffixifiedPPE biasedPPED
        let formattedTm = case sym of
              Symbol.Symbol _ (Var.User {}) -> Pretty.syntaxToColor $ TermPrinter.prettyBinding biasedPPE hqName (stripTypeAnnotation trm)
              _ -> TermPrinter.pretty biasedPPE (stripTypeAnnotation trm)
        let formatted = case wk of
              Nothing -> Pretty.syntaxToColor $ TermPrinter.prettyBinding biasedPPE hqName trm
              Just wk -> Pretty.string wk <> "> " <> formattedTm
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
        & Monoid.intercalateMap "\n\n" (Pretty.toPlain (Pretty.Width formattingWidth) . snd)
        & (\txt -> [TextEdit defsRange (Text.pack txt)])
        & pure
  where
    stripTypeAnnotation ::
      (Term.Term v a) -> (Term.Term v a)
    stripTypeAnnotation = \case
      Term.Ann' tm _annotation -> tm
      x -> x
