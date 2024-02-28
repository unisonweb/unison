module Unison.Codebase.Editor.HandleInput.DebugHashValidate (debugHashValidate) where

import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Hash32 (Hash32)
import Unison.Sync.Common qualified as Common
import Unison.Sync.EntityValidation qualified as EV

debugHashValidate :: Hash32 -> Cli ()
debugHashValidate hash = do
  entity <- Cli.runTransaction $ Common.expectEntity hash
  Cli.respond . HashValidationResult $ EV.validateEntity hash entity

-- debugHashValidate :: Hash32 -> Cli ()
-- debugHashValidate hash = do
--   Cli.Env {codebase} <- ask
--   mayComponent <- Cli.runTransaction $ Codebase.getTermComponentWithTypes codebase (Hash32.toHash hash)
--   case mayComponent of
--     Nothing -> liftIO $ putStrLn "No component found"
--     Just component ->
--       zip (symbol . tShow @Int <$> [1 ..]) component
--         & fmap (\(v, (trm, typ)) -> (v, (trm, typ, ())))
--         & Map.fromList
--         & hashTermComponents
--         & liftIO . pPrint
