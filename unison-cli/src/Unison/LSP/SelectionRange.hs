{-# LANGUAGE DataKinds #-}

module Unison.LSP.SelectionRange where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding (List)
import Data.Foldable qualified as Foldable
import Data.IntervalMap.Lazy qualified as IM
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types as LSP
import Unison.ABT qualified as ABT
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.LSP.Conversions (annToLspRange, annToURange, lspToUPos, rangeToInterval, uToLspRange)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Lexer.Pos qualified as L
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.UnisonFile.Type qualified as UF
import Unison.Util.Range qualified as URange

selectionRangeHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentSelectionRange -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentSelectionRange) -> Lsp ()) -> Lsp ()
selectionRangeHandler m respond =
  respond . Right . InL . fromMaybe mempty =<< runMaybeT do
    ranges <- rangesForFile (m ^. LSP.params . LSP.textDocument . LSP.uri)
    Debug.debugM Debug.LSP "selection ranges in file" ranges
    let results =
          (m ^. LSP.params . LSP.positions) & fmap \position ->
            fromMaybe emptyRange $ toSelectionRange (lspToUPos position) ranges
    Debug.debugM Debug.LSP "selection range results" ranges
    pure results
  where
    -- According to the lsp specification, send an empty range if the position doesn't have a
    -- valid range.
    emptyRange = SelectionRange {_range = LSP.Range {_start = Position 0 0, _end = Position 0 0}, _parent = Nothing}

allRangesForFile :: Uri -> MaybeT Lsp (IM.IntervalMap Position URange.Range)
allRangesForFile uri = do
  FileAnalysis {parsedFile} <- getFileAnalysis uri
  pf <- MaybeT $ pure parsedFile
  let anns = Foldable.toList pf
  Debug.debugM Debug.LSP "anns" anns
  pure $
    anns
      & foldMap aToR
      & fmap (\r -> (rangeToInterval (uToLspRange r), r))
      & IM.fromList
  where
    aToR :: Ann -> [URange.Range]
    aToR = maybeToList . annToURange

rangesForTerm :: (Foldable f, Functor f) => (ABT.Term f v a) -> Cofree [] a
rangesForTerm =
  ABT.cata \a abt ->
    a Cofree.:< toList abt

rangesForFile :: Uri -> MaybeT Lsp [Cofree [] Ann]
rangesForFile uri = do
  FileAnalysis {parsedFile} <- getFileAnalysis uri
  pf <- MaybeT $ pure parsedFile
  let termRanges =
        (UF.terms pf <> fold (Map.elems (UF.watches pf)))
          <&> \(_v, ann, term) -> ann Cofree.:< [rangesForTerm term]
  let declRanges =
        ((UF.dataDeclarationsId pf ^.. folded . _2) <> (UF.effectDeclarationsId pf ^.. folded . _2 . to DD.toDataDecl))
          <&> \DD.DataDeclaration {annotation, constructors'} ->
            annotation Cofree.:< (constructors' <&> \(ann, _v, typ) -> ann Cofree.:< [rangesForTerm typ])
  pure (termRanges <> declRanges)

toSelectionRange :: L.Pos -> [Cofree [] Ann] -> Maybe SelectionRange
toSelectionRange pos defs = do
  defs & altMap \(a Cofree.:< rest) -> do
    guard $ a `Ann.contains` pos
    range <- annToLspRange a
    let parent = SelectionRange {_range = range, _parent = Nothing}
    let child = toSelectionRange pos rest <&> \c -> c {_parent = Just parent}
    (child <|> pure parent)
