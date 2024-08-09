{-# LANGUAGE DataKinds #-}

module Unison.LSP.SelectionRange where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as Cofree
import Control.Lens hiding (List)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types as LSP
import Unison.ABT qualified as ABT
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.LSP.Conversions (annToLspRange, lspToUPos)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Lexer.Pos qualified as L
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.UnisonFile.Type qualified as UF

selectionRangeHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentSelectionRange -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentSelectionRange) -> Lsp ()) -> Lsp ()
selectionRangeHandler m respond =
  respond . Right . InL . fromMaybe mempty =<< runMaybeT do
    ranges <- rangesForFile (m ^. LSP.params . LSP.textDocument . LSP.uri)
    let results =
          (m ^. LSP.params . LSP.positions) & fmap \position ->
            fromMaybe emptyRange $ toSelectionRange (lspToUPos position) ranges
    -- Debug.debugM Debug.LSP "selectionRanges" results
    pure results
  where
    -- According to the lsp specification, send an empty range if the position doesn't have a
    -- valid range.
    emptyRange = SelectionRange {_range = LSP.Range {_start = Position 0 0, _end = Position 0 0}, _parent = Nothing}

rangesForTerm :: (Foldable f, Functor f) => (ABT.Term f v a) -> Cofree [] a
rangesForTerm =
  ABT.cata \a abt ->
    a Cofree.:< toList abt

rangesForFile :: Uri -> MaybeT Lsp [Cofree [] Ann]
rangesForFile uri = do
  FileAnalysis {parsedFile} <- getFileAnalysis uri
  pf <- MaybeT $ pure parsedFile
  let watchTerms =
        UF.watches pf
          & toListOf (folded . folded . to (\(_v, a, t) -> (a, t)))
  let terms =
        UF.terms pf
          & toListOf folded
  let termRanges =
        (terms <> watchTerms)
          <&> \(ann, term) -> ann Cofree.:< [rangesForTerm term]
  let declRanges =
        ((UF.dataDeclarationsId pf ^.. folded . _2) <> (UF.effectDeclarationsId pf ^.. folded . _2 . to DD.toDataDecl))
          <&> \DD.DataDeclaration {annotation, constructors'} ->
            annotation Cofree.:< (constructors' <&> \(ann, _v, typ) -> ann Cofree.:< [rangesForTerm typ])
  Debug.debugM Debug.LSP "selectionRanges" (termRanges <> declRanges)
  pure (termRanges <> declRanges)

-- | Given a position and an annotation tree, return the selection range tree that contains the position, discarding duplicate ranges.
--
-- >>> toSelectionRange (L.Pos 2 3) [Ann.Ann (L.Pos 1 1) (L.Pos 3 10) Cofree.:< []]
-- Just (SelectionRange {_range = Range {_start = Position {_line = 0, _character = 0}, _end = Position {_line = 2, _character = 9}}, _parent = Nothing})
--
-- >>> toSelectionRange (L.Pos 2 3) [Ann.Ann (L.Pos 1 1) (L.Pos 3 10) Cofree.:< [(Ann.Ann (L.Pos 2 1) (L.Pos 2 5) Cofree.:< [])]]
-- Just (SelectionRange {_range = Range {_start = Position {_line = 1, _character = 0}, _end = Position {_line = 1, _character = 4}}, _parent = Just (SelectionRange {_range = Range {_start = Position {_line = 0, _character = 0}, _end = Position {_line = 2, _character = 9}}, _parent = Nothing})})
--
-- >>> toSelectionRange (L.Pos 2 3) [Ann.Ann (L.Pos 1 1) (L.Pos 3 10) Cofree.:< [(Ann.Ann (L.Pos 2 1) (L.Pos 2 5) Cofree.:< [(Ann.Ann (L.Pos 2 2) (L.Pos 2 4) Cofree.:< [])])]]
-- Just (SelectionRange {_range = Range {_start = Position {_line = 1, _character = 1}, _end = Position {_line = 1, _character = 3}}, _parent = Just (SelectionRange {_range = Range {_start = Position {_line = 1, _character = 0}, _end = Position {_line = 1, _character = 4}}, _parent = Just (SelectionRange {_range = Range {_start = Position {_line = 0, _character = 0}, _end = Position {_line = 2, _character = 9}}, _parent = Nothing})})})
toSelectionRange :: L.Pos -> [Cofree [] Ann] -> Maybe SelectionRange
toSelectionRange pos defs = do
  defs & altMap \(a Cofree.:< rest) -> do
    guard $ a `Ann.contains` pos
    range <- annToLspRange a
    let parent = SelectionRange {_range = range, _parent = Nothing}
    let child = toSelectionRange pos rest <&> setParent parent
    case child of
      Nothing -> pure parent
      Just child
        | child ^. LSP.range == parent ^. LSP.range -> pure child
        | otherwise -> pure child
  where
    -- Set the parent of the most deeply nested range to the parent range.
    setParent :: SelectionRange -> SelectionRange -> SelectionRange
    setParent parent range@(SelectionRange {_parent}) =
      case _parent of
        Nothing -> range {_parent = Just parent}
        Just p -> range {_parent = Just (setParent parent p)}
