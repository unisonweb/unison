{-# LANGUAGE OverloadedStrings #-}

-- | This module is distinct from "Unison.Runtime.Exception" because that depends on "Unison.Runtime.Stack", which would
--   cause an import cycle.
module Unison.Runtime.InternalError
  ( CompileExn (CE),
    internalBug,
    prettyCompileExn,
  )
where

import Control.Exception (throw)
import GHC.Stack (CallStack, callStack)
import Unison.Prelude
import Unison.Util.Pretty as Pretty

data CompileExn = CE CallStack [Word] String

prettyCompileExn :: CompileExn -> Pretty Pretty.ColorText
prettyCompileExn (CE _ issues err) =
  Pretty.fatalCallout $
    Pretty.lines $
      [ Pretty.wrap "You’ve discovered an internal bug in the Unison runtime!",
        "",
        Pretty.indentN 2 $ Pretty.string err,
        ""
      ]
        <> if null issues
          then [Pretty.wrap "Please report it at https://github.com/unisonweb/unison/issues."]
          else
            [ Pretty.wrap $
                "See if one of these issues at https://github.com/unisonweb/unison/issues reflects what you’re seeing."
                  <> "If not, please open a new one:",
              Pretty.bulleted $ Pretty.string . show <$> issues
            ]

-- | __TODO__: With GHC 9.10, this implementation can be moved to `displayException` on the `Exception` instance, and
--             this instance can be derived again (see haskell/core-libraries-committee#198).
instance Show CompileExn where
  show = Pretty.toPlainUnbroken . prettyCompileExn

instance Exception CompileExn

internalBug :: (HasCallStack) => [Word] -> String -> a
internalBug issues = throw . CE callStack issues
