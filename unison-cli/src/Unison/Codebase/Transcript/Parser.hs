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
stanzas srcName = (\(CMark.Node _ _DOCUMENT blocks) -> traverse stanzaFromNode blocks) . CMark.commonmarkToNode []
  where
    stanzaFromNode :: CMark.Node -> Either (P.ParseErrorBundle Text Void) Stanza
    stanzaFromNode node = case node of
      CMarkCodeBlock _ info body -> maybe (Left node) pure <$> P.parse (fenced info) srcName body
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

-- | Produce the correct parser for the code block based on the provided info string.
fenced :: Text -> P (Maybe ProcessedBlock)
fenced info = do
  body <- P.getInput
  P.setInput info
  fenceType <- lineToken (word "ucm" <|> word "unison" <|> word "api" <|> language)
  case fenceType of
    "ucm" -> do
      hide <- hidden
      err <- expectingError
      P.setInput body
      pure . Ucm hide err <$> (spaces *> P.manyTill ucmLine P.eof)
    "unison" ->
      do
        -- todo: this has to be more interesting
        -- ```unison:hide
        -- ```unison
        -- ```unison:hide:all scratch.u
        hide <- lineToken hidden
        err <- lineToken expectingError
        fileName <- optional untilSpace1
        P.setInput body
        pure . Unison hide err fileName <$> (spaces *> P.getInput)
    "api" -> do
      P.setInput body
      pure . API <$> (spaces *> P.manyTill apiRequest P.eof)
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

expectingError :: P ExpectingError
expectingError = isJust <$> optional (word ":error")

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

language :: P Text
language = P.takeWhileP Nothing (\ch -> Char.isDigit ch || Char.isLower ch || ch == '_')

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace
