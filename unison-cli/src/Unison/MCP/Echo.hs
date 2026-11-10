-- | The reply to a transcript repeats it, with UCM's output filled in. The long @unison@ blocks are what the caller
-- wrote a moment ago, so by default they are left out, with a note of where they are.
module Unison.MCP.Echo (maxEchoedLines, elideLongBlocks) where

import CMark qualified
import Data.Text qualified as Text
import Unison.Codebase.Transcript
import Unison.MCP.LineNumbers (unisonBlockStarts)
import Unison.Prelude

-- | A @unison@ block of more lines than this is not repeated in the reply.
maxEchoedLines :: Int
maxEchoedLines = 10

-- | Replace the code of each inline @unison@ block of more than the given number of lines with a note of where it is:
-- which code block it is, and which lines of the source. Takes what to call the source, the source, and the transcript
-- that was run. If the blocks can't be matched up with the source, nothing is changed.
elideLongBlocks :: Int -> Text -> Text -> Transcript -> Transcript
elideLongBlocks maxLines sourceName source transcript@Transcript {stanzas}
  | length starts /= length userUnisonBlocks = transcript
  | not changed = transcript
  | otherwise = transcript {stanzas = elided ++ [note]}
  where
    starts = unisonBlockStarts source
    userUnisonBlocks = [() | Right (Unison tags _) <- stanzas, not (generated tags)]
    (elided, changed) = go 1 starts stanzas
    note =
      Left . CMark.Node Nothing CMark.PARAGRAPH $
        [ code "unison",
          text (" blocks of more than " <> tShow maxLines <> " lines are not repeated above, since you wrote them. Set "),
          code "echo",
          text " to true to include them."
        ]
    text t = CMark.Node Nothing (CMark.TEXT t) []
    code t = CMark.Node Nothing (CMark.CODE t) []

    -- The number of the next code block of the transcript, as the allowlist counts them (generated ones, which UCM added,
    -- don't count), and where the next `unison` block starts.
    go :: Int -> [Int] -> [Stanza] -> ([Stanza], Bool)
    go _ _ [] = ([], False)
    go n starts' (stanza : rest) =
      case stanza of
        Right (Unison tags code)
          | not (generated tags),
            start : starts'' <- starts' ->
              let len = length (Text.lines code)
                  (others, changedLater) = go (n + 1) starts'' rest
               in if len > maxLines
                    then (Right (Unison tags (stub n start len)) : others, True)
                    else (stanza : others, changedLater)
        Right block
          | not (generated (getCommonInfoTags block)) ->
              let (others, changedLater) = go (n + 1) starts' rest in (stanza : others, changedLater)
        _ ->
          let (others, changedLater) = go n starts' rest in (stanza : others, changedLater)

    stub n start len =
      "-- code block "
        <> tShow n
        <> ": your "
        <> tShow len
        <> " lines, lines "
        <> tShow start
        <> "-"
        <> tShow (start + len - 1)
        <> " of "
        <> sourceName
        <> "\n"
