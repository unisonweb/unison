module Unison.Server.Share.NamespaceDetails (namespaceDetails) where

import Control.Monad.Except
import Data.Set qualified as Set
import Servant.OpenApi ()
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.Parser.Ann (Ann)
import Unison.Server.Backend
import Unison.Server.Backend qualified as Backend
import Unison.Server.Share.RenderDoc qualified as RenderDoc
import Unison.Server.Types
  ( NamespaceDetails (..),
    v2CausalBranchToUnisonHash,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

namespaceDetails ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Path.Path ->
  CausalHash ->
  Maybe Width ->
  Backend IO NamespaceDetails
namespaceDetails runtime codebase namespacePath rootCausalHash mayWidth = do
  causalHashAtPath <- liftIO $ Codebase.runTransaction codebase do
    causalBranch <- Backend.resolveCausalHashV2 (Just rootCausalHash)
    namespaceCausal <- Codebase.getShallowCausalAtPath namespacePath (Just causalBranch)
    let causalHashAtPath = v2CausalBranchToUnisonHash namespaceCausal
    pure causalHashAtPath
  mayReadme <- RenderDoc.findAndRenderDoc readmeNames runtime codebase namespacePath rootCausalHash mayWidth
  pure $ NamespaceDetails namespacePath causalHashAtPath mayReadme
  where
    readmeNames = Set.fromList ["README", "Readme", "ReadMe", "readme"]
