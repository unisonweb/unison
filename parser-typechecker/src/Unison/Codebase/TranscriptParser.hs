{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}
{-# Language ViewPatterns #-}

module Unison.Codebase.TranscriptParser (
  Stanza(..), FenceType, ExpectingError, HideOutput, Err, UcmCommand(..),
  parse, parseFile)
  where

import Unison.Prelude
import qualified Text.Megaparsec as P
-- import qualified Text.Megaparsec.Char as P
import qualified Data.Char as Char
import qualified Data.Text as Text
import Unison.Codebase.Path as Path

type ExpectingError = Bool
type HideOutput = Bool
type Err = String
type ScratchFileName = Text

type FenceType = Text

data UcmCommand = UcmCommand Path.Absolute Text deriving Show

data Stanza
  = Ucm HideOutput [UcmCommand]
  | Unison HideOutput (Maybe ScratchFileName) Text
  | UnprocessedFence FenceType Text
  | Unfenced Text
  deriving Show

parseFile :: FilePath -> IO (Either Err [Stanza])
parseFile filePath = do
  txt <- readUtf8 filePath
  pure $ parse filePath txt

parse :: String -> Text -> Either Err [Stanza]
parse srcName txt = case P.parse (stanzas <* P.eof) srcName txt of
  Right a -> Right a
  Left e -> Left (show e)

type P = P.Parsec () Text

stanzas :: P [Stanza]
stanzas = P.many (fenced <|> unfenced)

ucmCommand :: P UcmCommand
ucmCommand = do
  P.lookAhead (word ".")
  path <- P.takeWhile1P Nothing (/= '>')
  void $ word ">"
  line <- P.takeWhileP Nothing (/= '\n') <* spaces
  path <- case Path.parsePath' (Text.unpack path) of
    Right (Path.unPath' -> Left abs) -> pure abs
    Right _ -> fail "expected absolute path"
    Left e -> fail e
  pure $ UcmCommand path line

fenced :: P Stanza
fenced = do
  fence
  fenceType <- word "ucm" <|> word "unison" <|> untilSpace1
  stanza <-
    if fenceType == "ucm" then do
      hideOutput <- hideOutput
      cmds <- many ucmCommand
      pure $ Ucm hideOutput cmds
    else if fenceType == "unison" then do
      hideOutput <- hideOutput
      fileName <- optional untilSpace1
      blob <- untilFence
      pure $ Unison hideOutput fileName blob
    else UnprocessedFence fenceType <$> untilFence
  fence
  pure stanza

-- Three backticks, consumes trailing spaces too
-- ```
fence :: P ()
fence = P.try $ do void (word "```"); spaces

-- Parses up until next fence
unfenced :: P Stanza
unfenced = Unfenced <$> untilFence

untilFence :: P Text
untilFence = do
  _ <- P.lookAhead (P.takeP Nothing 1)
  go mempty
  where
  go :: Seq Text -> P Text
  go !acc = do
    f <- P.lookAhead (P.optional fence)
    case f of
      Nothing -> do
        oneOrTwoBackticks <- optional (word' "``" <|> word' "`")
        let start = fromMaybe "" oneOrTwoBackticks
        txt <- P.takeWhileP (Just "unfenced") (/= '`')
        eof <- P.lookAhead (P.optional P.eof)
        case eof of
          Just _ -> pure $ foldMap id (acc <> pure txt)
          Nothing -> go (acc <> pure start <> pure txt)
      Just _ -> pure $ foldMap id acc

word' :: Text -> P Text
word' txt = P.try $ do
  chs <- P.takeP (Just $ show txt) (Text.length txt)
  guard (chs == txt)
  pure txt

word :: Text -> P Text
word txt = word' txt <* spaces

hideOutput :: P HideOutput
hideOutput = isJust <$> (optional (word ":hide") <* spaces)

untilSpace1 :: P Text
untilSpace1 = P.takeWhile1P Nothing (not . Char.isSpace)

spaces :: P ()
spaces = void $ P.takeWhileP (Just "spaces") Char.isSpace

-- single :: Char -> P Char
-- single t = P.satisfy (== t)
