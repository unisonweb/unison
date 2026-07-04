-- | goToDeclaration, goToDefinition, and goToImplementation are equivalent except for the input/return wrappers
module Unison.LSP.GoToDefinition
  ( goToDefinitionHandler,
    goToDeclarationHandler,
    goToImplementationHandler,

    -- * Exposed for tests
    locationInfo,
  )
where

import Control.Lens hiding (List)
import Data.IntervalMap.Lazy qualified as IM
import Data.Map.Strict qualified as Map
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis qualified as FileAnalysis
import Unison.LSP.Types
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.UnisonFile.Summary (FileSummary (..))

-- | Go to Definition handler
goToDefinitionHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentDefinition -> (Either (Msg.TResponseError m) (Msg.MessageResult 'Msg.Method_TextDocumentDefinition) -> Lsp ()) -> Lsp ()
goToDefinitionHandler m respond = do
  respond . Right . maybe (InR . InL $ []) (InR . InL) =<< runMaybeT do
    let pos = (m ^. params . position)
    targetRange <- locationInfo (m ^. params . textDocument . uri) pos
    let originLoc = Nothing -- Just use default
    let targetUri = m ^. params . textDocument . uri
    pure $
      [DefinitionLink (LocationLink originLoc targetUri targetRange targetRange)]

-- | Go to Declaration handler
goToDeclarationHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentDeclaration -> (Either (Msg.TResponseError m) (Msg.MessageResult 'Msg.Method_TextDocumentDeclaration) -> Lsp ()) -> Lsp ()
goToDeclarationHandler m respond = do
  respond . Right . maybe (InR . InL $ []) (InR . InL) =<< runMaybeT do
    let pos = (m ^. params . position)
    targetRange <- locationInfo (m ^. params . textDocument . uri) pos
    let originLoc = Nothing -- Just use default
    let targetUri = m ^. params . textDocument . uri
    pure $
      [DeclarationLink (LocationLink originLoc targetUri targetRange targetRange)]

goToImplementationHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentImplementation -> (Either (Msg.TResponseError m) (Msg.MessageResult 'Msg.Method_TextDocumentImplementation) -> Lsp ()) -> Lsp ()
goToImplementationHandler m respond = do
  respond . Right . maybe (InR . InL $ []) (InR . InL) =<< runMaybeT do
    let pos = (m ^. params . position)
    targetRange <- locationInfo (m ^. params . textDocument . uri) pos
    let originLoc = Nothing -- Just use default
    let targetUri = m ^. params . textDocument . uri
    pure $
      [DefinitionLink (LocationLink originLoc targetUri targetRange targetRange)]

locationInfo :: (Lspish m) => Uri -> Position -> MaybeT m Range
locationInfo uri pos =
  -- Try implicit-arg goto-def first; otherwise fall back
  -- to local-binding goto-def (the prior behavior). If the cursor is
  -- on a function whose call has a synthesized implicit argument, we
  -- jump to the resolved given's definition.
  implicitArgLocation <|> localBindingLocation
  where
    hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
    hoistMaybe = MaybeT . pure

    localBindingLocation = do
      FileAnalysis {localBindingInfo} <- FileAnalysis.getFileAnalysis uri
      (_interval, (_typ, range)) <- hoistMaybe $ IM.lookupMin $ IM.intersecting localBindingInfo (IM.ClosedInterval pos pos)
      pure range

    -- For each reference recorded at the cursor's position, look up
    -- its definition site. Currently we only resolve in-file givens
    -- (a 'Reference.DerivedId' pointing at a term defined in this
    -- file); a future revision can plumb cross-file/codebase goto-def
    -- through @Codebase.runTransaction@.
    implicitArgLocation = do
      FileAnalysis {implicitArgInfo, fileSummary} <- FileAnalysis.getFileAnalysis uri
      summary <- hoistMaybe fileSummary
      let refs =
            IM.intersecting implicitArgInfo (IM.ClosedInterval pos pos)
              & IM.toAscList
              & concatMap snd
      hoistMaybe $ asum (refToFileRange summary <$> refs)

    refToFileRange :: FileSummary -> Reference.Reference -> Maybe Range
    refToFileRange summary = \case
      Reference.DerivedId refId -> do
        -- 'termsByReference' is keyed by 'Maybe Reference.Id'; the
        -- 'Just' branch holds typechecked entries.
        let symMap = Map.findWithDefault mempty (Just refId) (termsByReference summary)
        (ann, _trm, _typ) <- listToMaybe (Map.elems symMap)
        annToRange ann
      Reference.Builtin _ -> Nothing
