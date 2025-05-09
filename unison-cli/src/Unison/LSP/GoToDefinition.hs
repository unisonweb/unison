-- | goToDeclaration, goToDefinition, and goToImplementation are equivalent except for the input/return wrappers
module Unison.LSP.GoToDefinition
  ( goToDefinitionHandler,
    goToDeclarationHandler,
    goToImplementationHandler,
  )
where

import Control.Lens hiding (List)
import Data.IntervalMap.Lazy qualified as IM
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.LSP.FileAnalysis qualified as FileAnalysis
import Unison.LSP.Types
import Unison.Prelude

-- | Go to Definition handler
goToDefinitionHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentDefinition -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentDefinition) -> Lsp ()) -> Lsp ()
goToDefinitionHandler m respond = do
  respond . Right . maybe (InR . InL $ []) (InR . InL) =<< runMaybeT do
    let pos = (m ^. params . position)
    targetRange <- locationInfo (m ^. params . textDocument . uri) pos
    let originLoc = Nothing -- Just use default
    let targetUri = m ^. params . textDocument . uri
    pure $
      [DefinitionLink (LocationLink originLoc targetUri targetRange targetRange)]

-- | Go to Declaration handler
goToDeclarationHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentDeclaration -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentDeclaration) -> Lsp ()) -> Lsp ()
goToDeclarationHandler m respond = do
  respond . Right . maybe (InR . InL $ []) (InR . InL) =<< runMaybeT do
    let pos = (m ^. params . position)
    targetRange <- locationInfo (m ^. params . textDocument . uri) pos
    let originLoc = Nothing -- Just use default
    let targetUri = m ^. params . textDocument . uri
    pure $
      [DeclarationLink (LocationLink originLoc targetUri targetRange targetRange)]

goToImplementationHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentImplementation -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentImplementation) -> Lsp ()) -> Lsp ()
goToImplementationHandler m respond = do
  respond . Right . maybe (InR . InL $ []) (InR . InL) =<< runMaybeT do
    let pos = (m ^. params . position)
    targetRange <- locationInfo (m ^. params . textDocument . uri) pos
    let originLoc = Nothing -- Just use default
    let targetUri = m ^. params . textDocument . uri
    pure $
      [DefinitionLink (LocationLink originLoc targetUri targetRange targetRange)]

locationInfo :: Uri -> Position -> MaybeT Lsp Range
locationInfo uri pos = do
  FileAnalysis {localBindingInfo} <- FileAnalysis.getFileAnalysis uri
  (_interval, (_typ, range)) <- hoistMaybe $ IM.lookupMin $ IM.intersecting localBindingInfo (IM.ClosedInterval pos pos)
  pure range
  where
    hoistMaybe :: Maybe a -> MaybeT Lsp a
    hoistMaybe = MaybeT . pure
