-- | Parse and print CommonMark (like Github-flavored Markdown) transcripts.
module Unison.Codebase.Transcript.Parser
  ( -- * printing
    formatAPIRequest,
    formatUcmLine,
    formatStanza,
    formatNode,
    formatProcessedBlock,

    -- * conversion
    processedBlockToNode,

    -- * parsing
    stanzas,
    ucmLine,
    apiRequest,
    fenced,
    hidden,
    expectingError,
    language,
  )
where

import CMark qualified
import Data.Char qualified as Char
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Unison.Codebase.Transcript
import Unison.Prelude
import Unison.Project (fullyQualifiedProjectAndBranchNamesParser)

formatAPIRequest :: APIRequest -> Text
formatAPIRequest = \case
  GetRequest txt -> "GET " <> txt
  APIComment txt -> "-- " <> txt

formatUcmLine :: UcmLine -> Text
formatUcmLine = \case
  UcmCommand context txt -> formatContext context <> "> " <> txt
  UcmComment txt -> "--" <> txt
  where
    formatContext (UcmContextProject projectAndBranch) = into @Text projectAndBranch

formatStanza :: Stanza -> Text
formatStanza = either formatNode formatProcessedBlock

formatNode :: CMark.Node -> Text
formatNode = (<> "\n") . CMark.nodeToCommonmark [] Nothing

formatProcessedBlock :: ProcessedBlock -> Text
formatProcessedBlock = formatNode . processedBlockToNode

processedBlockToNode :: ProcessedBlock -> CMark.Node
processedBlockToNode = \case
  Ucm _ _ cmds -> CMarkCodeBlock Nothing "ucm" $ foldr ((<>) . formatUcmLine) "" cmds
  Unison _hide _ fname txt ->
    CMarkCodeBlock Nothing "unison" $ maybe txt (\fname -> Text.unlines ["---", "title: " <> fname, "---", txt]) fname
  API apiRequests -> CMarkCodeBlock Nothing "api" $ Text.unlines $ formatAPIRequest <$> apiRequests

type P = P.Parsec Void Text

stanzas :: FilePath -> Text -> Either (P.ParseErrorBundle Text Void) [Stanza]
stanzas srcName =
  -- TODO: Internal warning if `_DOCUMENT` isn’t `CMark.DOCUMENT`.
  (\(CMark.Node _ _DOCUMENT blocks) -> traverse stanzaFromNode blocks)
    . CMark.commonmarkToNode [CMark.optSourcePos]
  where
    stanzaFromNode :: CMark.Node -> Either (P.ParseErrorBundle Text Void) Stanza
    stanzaFromNode node = case node of
      CMarkCodeBlock (Just CMark.PosInfo {startLine, startColumn}) info body ->
        maybe (Left node) pure <$> snd (P.runParser' fenced $ fencedState srcName startLine startColumn info body)
      _ -> pure $ Left node

ucmLine :: P UcmLine
ucmLine = ucmCommand <|> ucmComment
  where
    ucmCommand :: P UcmLine
    ucmCommand =
      UcmCommand
        <$> fmap UcmContextProject (P.try $ fullyQualifiedProjectAndBranchNamesParser <* lineToken (word ">"))
        <*> P.takeWhileP Nothing (/= '\n')
        <* spaces

    ucmComment :: P UcmLine
    ucmComment =
      P.label "comment (delimited with “--”)" $
        UcmComment <$> (word "--" *> P.takeWhileP Nothing (/= '\n')) <* spaces

apiRequest :: P APIRequest
apiRequest = do
  apiComment <|> getRequest
  where
    getRequest = do
      word "GET"
      spaces
      path <- P.takeWhile1P Nothing (/= '\n')
      spaces
      pure (GetRequest path)
    apiComment = do
      word "--"
      comment <- P.takeWhileP Nothing (/= '\n')
      spaces
      pure (APIComment comment)

-- | Parses the info string and contents of a fenced code block.
fenced :: P (Maybe ProcessedBlock)
fenced = do
  fenceType <- lineToken (word "ucm" <|> word "unison" <|> word "api" <|> language)
  case fenceType of
    "ucm" -> do
      hide <- hidden
      err <- expectingError
      pure . Ucm hide err <$> (spaces *> P.manyTill ucmLine P.eof)
    "unison" -> do
      -- todo: this has to be more interesting
      -- ``` unison :hide
      -- ``` unison
      -- ``` unison :hide:all scratch.u
      hide <- lineToken hidden
      err <- lineToken expectingError
      fileName <- optional untilSpace1
      P.single '\n'
      pure . Unison hide err fileName <$> P.getInput
    "api" -> pure . API <$> (spaces *> P.manyTill apiRequest P.eof)
    _ -> pure Nothing

word :: Text -> P Text
word txt = P.try $ do
  chs <- P.takeP (Just $ show txt) (Text.length txt)
  guard (chs == txt)
  pure txt

lineToken :: P a -> P a
lineToken p = p <* nonNewlineSpaces

nonNewlineSpaces :: P ()
nonNewlineSpaces = void $ P.takeWhileP Nothing (\ch -> ch == ' ' || ch == '\t')

hidden :: P Hidden
hidden =
  (HideAll <$ word ":hide:all")
    <|> (HideOutput <$ word ":hide")
    <|> pure Shown

expectingError :: P Result
expectingError =
  (Error <$ word ":error")
    <|> (Failure <$ word ":failure")
    <|> (Incorrect <$ word ":incorrect")
    <|> pure Success

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

language :: P Text
language = P.takeWhileP Nothing (\ch -> Char.isDigit ch || Char.isLower ch || ch == '_')

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace

-- | Create a parser state that has source locations that match the file (as opposed to being relative to the start of
--   the individual fenced code block).
--
--  __NB__: If a code block has a fence longer than the minimum (three backticks), the columns for parse errors in the
--          info string will be slightly off (but the printed code excerpt will match the reported positions).
--
--  __NB__: Creating custom states is likely simpler starting with Megaparsec 9.6.0.
fencedState ::
  -- | file containing the fenced code block
  FilePath ->
  -- | `CMark.startLine` for the block
  Int ->
  -- | `CMark.startColumn` for the block`
  Int ->
  -- | info string from the block
  Text ->
  -- | contents of the code block
  Text ->
  P.State Text e
fencedState name startLine startColumn info body =
  let -- This is the most common opening fence, so we assume it’s the right one. I don’t think there’s any way to get
      -- the actual size of the fence from "CMark", so this can be wrong sometimes, but it’s probably the approach
      -- that’s least likely to confuse users.
      openingFence = "``` "
      -- Glue the info string and body back together, as if they hadn’t been split by "CMark". This keeps the position
      -- info in sync.
      s = info <> "\n" <> body
   in P.State
        { stateInput = s,
          stateOffset = 0,
          statePosState =
            P.PosState
              { pstateInput = s,
                pstateOffset = 0,
                -- `CMark.startColumn` marks the beginning of the fence, not the beginning of the info string, so we
                -- adjust it for the fence that precedes it.
                pstateSourcePos = P.SourcePos name (P.mkPos startLine) . P.mkPos $ startColumn + length openingFence,
                pstateTabWidth = P.defaultTabWidth,
                -- Ensure we print the fence as part of the line if there’s a parse error in the info string.
                pstateLinePrefix = openingFence
              },
          stateParseErrors = []
        }
