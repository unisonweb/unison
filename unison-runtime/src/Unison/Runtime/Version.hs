{-# LANGUAGE TemplateHaskell #-}

-- | The version of the runtime, which a compiled program (a .uc file)
-- records and which has to match the version running it: its MCode is
-- only readable by the runtime that wrote it. ucm (compile, run.compiled)
-- and unisonrt both use this one, so two programs built in the same tree
-- agree. It is made the way ucm makes its own version
-- (unison-cli-main/unison/Version.hs), from git when this module is
-- compiled.
module Unison.Runtime.Version (runtimeVersion) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH (Exp (LitE, TupE), Lit (StringL), runIO)
import System.Process (readProcess)

-- | E.g. @release/1.0.0 (built on 2026-11-08)@
runtimeVersion :: Text
runtimeVersion = Text.pack tag <> " (built on " <> Text.pack date <> ")"
  where
    (tag, date) :: (String, String) =
      $( runIO $ do
           let git args fallback =
                 either (\(_ :: SomeException) -> fallback) (takeWhile (/= '\n'))
                   <$> try (readProcess "git" args "")
           date <- git ["show", "-s", "--format=%cs"] ""
           tag <- git ["describe", "--tags", "--always", "--dirty='"] "unknown"
           pure (TupE [Just (LitE (StringL tag)), Just (LitE (StringL date))])
       )
