-- | UCM quotes the line it is complaining about with a number that counts from the start of the code block it was
-- given (@2 | double n = n * "two"@). Rewrite those numbers to count from the start of the transcript, so that the caller
-- can go straight to the line without adding anything up.
--
-- Only messages about the transcript's own @unison@ blocks are touched. Messages about a file that a command loaded,
-- like @load foo.u@, already have that file's line numbers and are left alone.
module Unison.MCP.LineNumbers (absolutizeLineNumbers) where

import CMark qualified
import Data.Char (isDigit)
import Data.Text qualified as Text
import Unison.Codebase.Transcript
import Unison.Prelude

-- | The transcript that was run (its output), the source it was run from, and what to call that source (for the note
-- this adds when it changes something). If the blocks can't be matched up with the source, nothing is changed.
absolutizeLineNumbers :: Text -> Text -> Transcript -> Transcript
absolutizeLineNumbers sourceName source transcript@Transcript {stanzas}
  | length starts /= length userUnisonBlocks = transcript
  | not changed = transcript
  | otherwise = transcript {stanzas = rewritten ++ [note]}
  where
    -- Where each `unison` block's code starts in the source, 1-based: the line after its opening fence.
    starts =
      [ startLine pos + 1
      | CMark.Node _ CMark.DOCUMENT blocks <- [CMark.commonmarkToNode [CMark.optSourcePos] source],
        CMark.Node (Just pos) (CMark.CODE_BLOCK info _) _ <- blocks,
        ("unison" : _) <- [Text.words info]
      ]
    startLine CMark.PosInfo {CMark.startLine = line} = line
    userUnisonBlocks = [() | Right (Unison tags _) <- stanzas, not (generated tags)]
    (rewritten, changed) = go starts Nothing False stanzas
    note =
      Left . CMark.Node Nothing CMark.PARAGRAPH $
        [CMark.Node Nothing (CMark.TEXT ("Line numbers in the messages above are lines of " <> sourceName <> ".")) []]

    -- The pending state is where the code of the `unison` block whose messages we are in starts, and how long it is.
    go :: [Int] -> Maybe (Int, Int) -> Bool -> [Stanza] -> ([Stanza], Bool)
    go _ _ _ [] = ([], False)
    go remaining pending afterMarker (stanza : rest) =
      case stanza of
        Right (Unison tags code)
          | not (generated tags),
            start : remaining' <- remaining ->
              continue remaining' (Just (start, length (Text.lines code))) False stanza
        -- What UCM said about the block that was just loaded.
        Right (Ucm tags ucmLines)
          | generated tags,
            Just (start, len) <- pending ->
              let rewrites = rewriteUcmLine start len <$> ucmLines
               in prepend (Right (Ucm tags (fst <$> rewrites))) (any snd rewrites) (go remaining Nothing False rest)
        Right _ -> continue remaining Nothing False stanza
        -- A failed transcript ends with a marker, then the error, which for a failing `unison` block is about that block.
        Left (CMark.Node _ CMark.PARAGRAPH [CMark.Node _ (CMark.TEXT "🛑") _]) -> continue remaining pending True stanza
        Left (CMarkCodeBlock pos "" body)
          | afterMarker,
            Just (start, len) <- pending ->
              let (body', didChange) = rewriteText start len body
               in prepend (Left (CMarkCodeBlock pos "" body')) didChange (go remaining pending afterMarker rest)
        Left _ -> continue remaining pending afterMarker stanza
      where
        continue remaining' pending' afterMarker' s = prepend s False (go remaining' pending' afterMarker' rest)
        prepend s didChange (others, changedLater) = (s : others, didChange || changedLater)

    rewriteUcmLine start len = \case
      UcmOutputLine text -> let (text', didChange) = rewriteText start len text in (UcmOutputLine text', didChange)
      line -> (line, False)

-- | Rewrite the lines of a message that quote a line of the block: @<spaces><n> | <source>@, where @n@ is a line of a
-- block of the given length that starts at the given line of the transcript.
rewriteText :: Int -> Int -> Text -> (Text, Bool)
rewriteText start len text =
  let rewrites = rewriteLine <$> Text.splitOn "\n" text
   in (Text.intercalate "\n" (fst <$> rewrites), any snd rewrites)
  where
    rewriteLine line =
      let (spaces, afterSpaces) = Text.span (== ' ') line
          (digits, afterDigits) = Text.span isDigit afterSpaces
       in case readMaybe (Text.unpack digits) of
            Just n
              | n >= 1,
                n <= len,
                " |" `Text.isPrefixOf` afterDigits ->
                  let new = tShow (start + n - 1)
                      -- keep the `|`s lined up
                      spaces' = Text.replicate (max 0 (Text.length spaces - (Text.length new - Text.length digits))) " "
                   in (spaces' <> new <> afterDigits, True)
            _ -> (line, False)
