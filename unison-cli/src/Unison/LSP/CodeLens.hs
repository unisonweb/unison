{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.CodeLens where

import Control.Lens hiding (List)
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Unison.ABT as ABT
import qualified Unison.Debug as Debug
import qualified Unison.LSP.Commands as Commands
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
import qualified Unison.UnisonFile as UF

-- | Computes code lenses for a document.
codeLensHandler :: RequestMessage 'TextDocumentCodeLens -> (Either ResponseError (ResponseResult 'TextDocumentCodeLens) -> Lsp ()) -> Lsp ()
codeLensHandler m respond = do
  respond . maybe (Right mempty) (Right . List) =<< runMaybeT do
    FileAnalysis {typecheckedFile} <- MaybeT $ getFileAnalysis (m ^. params . textDocument . uri)
    tf <- MaybeT $ pure typecheckedFile
    let watchLenses = codeLensesForWatches tf
    Debug.debugM Debug.LSP "Code Lenses" watchLenses
    pure watchLenses

codeLensesForWatches :: UF.TypecheckedUnisonFile Symbol Ann -> [CodeLens]
codeLensesForWatches tf =
  let watchLenses =
        UF.watchComponents tf
          & foldMap (\(_wk, watches) -> watches)
          & foldMap
            \(_v, trm, _typ) ->
              maybeToList (watchCodeLens <$> annToRange (ABT.annotation trm))
   in watchLenses

watchCodeLens :: Range -> CodeLens
watchCodeLens range =
  CodeLens {_range = range, _command = Just Commands.refreshWatchesCommand, _xdata = Nothing}

-- | Runs a selected code lens
codeLensResolveHandler :: RequestMessage 'CodeLensResolve -> (Either ResponseError (ResponseResult 'CodeLensResolve) -> Lsp ()) -> Lsp ()
codeLensResolveHandler _m respond = respond (Left $ error "unsupported")
