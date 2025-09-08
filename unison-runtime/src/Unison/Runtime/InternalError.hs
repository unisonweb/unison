{-# LANGUAGE OverloadedStrings #-}

-- | This module is distinct from "Unison.Runtime.Exception" because that depends on "Unison.Runtime.Stack", which would
--   cause an import cycle.
module Unison.Runtime.InternalError
  ( CompileExn (CE),
    githubTitleForIssue,
    internalBug,
    issueUrl,
    prettyCompileExn,
  )
where

import Control.Exception (throw)
import GHC.Stack (CallStack, callStack)
import GitHub qualified as GH
import Unison.Prelude
import Unison.Util.Pretty as Pretty

data CompileExn = CE CallStack [Word] String

issueUrl :: Word -> Pretty Pretty.ColorText
issueUrl = Pretty.string . ("https://github.com/unisonweb/unison/issues/" <>) . show

githubTitleForIssue :: Word -> IO (Either GH.Error Text)
githubTitleForIssue =
  fmap (fmap GH.issueTitle) . GH.github' GH.issueR "unisonweb" "unison" . GH.IssueNumber . fromIntegral

prettyCompileExn' ::
  (Applicative f) => (Word -> f (Pretty Pretty.ColorText)) -> CompileExn -> f (Pretty Pretty.ColorText)
prettyCompileExn' issueFn (CE _ issues err) = do
  issueMessages <- traverse issueFn issues
  pure $
    Pretty.fatalCallout . Pretty.lines $
      [ Pretty.wrap "Sorry – I've encountered a bug in the Unison runtime.",
        "",
        Pretty.indentN 2 $ Pretty.string err,
        ""
      ]
        <> if null issues
          then [Pretty.wrap "Please report it at https://github.com/unisonweb/unison/issues/new/choose."]
          else
            [ Pretty.wrap "Please check if one of these known issues matches your situation:",
              "",
              Pretty.bulleted issueMessages,
              "",
              Pretty.wrap "If not, please open a new one: https://github.com/unisonweb/unison/issues/new/choose"
            ]

prettyCompileExn :: CompileExn -> IO (Pretty Pretty.ColorText)
prettyCompileExn =
  prettyCompileExn'
    ( \i -> do
        mtitle <- githubTitleForIssue i
        pure $ either (const $ issueUrl i) (\title -> Pretty.wrap $ Pretty.text title <> " " <> issueUrl i) mtitle
    )

-- | __TODO__: With GHC 9.10, this implementation can be moved to `displayException` on the `Exception` instance, and
--             this instance can be derived again (see haskell/core-libraries-committee#198).
instance Show CompileExn where
  show = Pretty.toPlain 0 . runIdentity . prettyCompileExn' (pure . issueUrl)

instance Exception CompileExn

internalBug :: (HasCallStack) => [Word] -> String -> a
internalBug issues = throw . CE callStack issues
