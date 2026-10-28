module Unison.Test.Cli.ScratchFile
  ( test,
  )
where

import Data.Char (isHexDigit)
import Data.Text qualified as Text
import EasyTest
import Unison.Cli.ScratchFile (scratchFileName)

test :: Test ()
test =
  (scope "Unison.Cli.ScratchFile" . tests)
    [ scope "is <action>-<8 hex characters>.u" do
        let name = scratchFileName "merge" ["alice", "bob"]
        expect ("merge-" `Text.isPrefixOf` name)
        expect (".u" `Text.isSuffixOf` name)
        let hash = Text.dropEnd 2 (Text.drop (Text.length "merge-") name)
        expectEqual' 8 (Text.length hash)
        expect (Text.all isHexDigit hash)
        ok,
      scope "is the same for the same action and arguments" do
        expectEqual' (scratchFileName "update" ["a", "b"]) (scratchFileName "update" ["a", "b"])
        ok,
      scope "differs by action" do
        expect (scratchFileName "merge" ["a"] /= scratchFileName "pull" ["a"])
        ok,
      scope "differs by argument" do
        expect (scratchFileName "update" ["a"] /= scratchFileName "update" ["b"])
        ok,
      scope "does not confuse where one argument ends and the next begins" do
        expect (scratchFileName "update" ["ab", "c"] /= scratchFileName "update" ["a", "bc"])
        ok
    ]
