module Unison.Codebase.Editor.HandleInput.DebugHashValidate (debugHashValidate) where

import Control.Monad.Reader
import Data.Map qualified as Map
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase.Type qualified as Codebase
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2.Convert (hashTermComponents)
import Unison.Prelude
import Unison.Symbol (symbol)

-- debugHashValidate :: Hash32 -> Cli ()
-- debugHashValidate hash = do
--   entity <- Cli.runTransaction $ expectEntity hash
--   Cli.respond . HashValidationResult $ EV.validateEntity hash entity

debugHashValidate :: Hash32 -> Cli ()
debugHashValidate hash = do
  Cli.Env {codebase} <- ask
  mayComponent <- Cli.runTransaction $ Codebase.getTermComponentWithTypes codebase (Hash32.toHash hash)
  case mayComponent of
    Nothing -> liftIO $ putStrLn "No component found"
    Just component ->
      zip (symbol . tShow @Int <$> [1 ..]) component
        & fmap (\(v, (trm, typ)) -> (v, (trm, typ, ())))
        & Map.fromList
        & hashTermComponents
        & liftIO . print
