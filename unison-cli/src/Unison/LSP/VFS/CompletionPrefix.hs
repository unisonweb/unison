{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: Alan Zimmerman, 2016-2021
-- License: MIT
--
-- This code was removed from the lsp package in version 2.4.0.0. It’s vendored here until it’s replaced with a Unison-
-- specific version.
module Unison.LSP.VFS.CompletionPrefix
  ( PosPrefixInfo (..),
    getCompletionPrefix,
  )
where

import Data.Char (isAlphaNum, isUpper)
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Utf16.Rope.Mixed qualified as Rope
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.VFS

-- | Describes the line at the current cursor position
data PosPrefixInfo = PosPrefixInfo
  { -- | The full contents of the line the cursor is at
    fullLine :: !T.Text,
    -- | If any, the module name that was typed right before the cursor position.
    --  For example, if the user has typed "Data.Maybe.from", then this property
    --  will be "Data.Maybe"
    prefixModule :: !T.Text,
    -- | The word right before the cursor position, after removing the module part.
    -- For example if the user has typed "Data.Maybe.from",
    -- then this property will be "from"
    prefixText :: !T.Text,
    -- | The cursor position
    cursorPos :: !J.Position
  }
  deriving (Show, Eq)

getCompletionPrefix :: (Monad m) => J.Position -> VirtualFile -> m (Maybe PosPrefixInfo)
getCompletionPrefix pos@(J.Position l c) (VirtualFile _ _ ropetext) =
  return $ Just $ fromMaybe (PosPrefixInfo "" "" "" pos) $ do
    -- Maybe monad
    let lastMaybe [] = Nothing
        lastMaybe xs = Just $ last xs

    let curRope = fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (fromIntegral l) ropetext
    let beforePos = Rope.toText . fst $ Rope.charSplitAt (fromIntegral c) curRope
    curWord <-
      if
        | T.null beforePos -> Just ""
        | T.last beforePos == ' ' -> Just "" -- don't count abc as the curword in 'abc '
        | otherwise -> lastMaybe (T.words beforePos)

    let parts =
          T.split (== '.') $
            T.takeWhileEnd (\x -> isAlphaNum x || x `elem` ("._'" :: String)) curWord
    case reverse parts of
      [] -> Nothing
      (x : xs) -> do
        let modParts =
              dropWhile (not . isUpper . T.head) $
                reverse $
                  filter (not . T.null) xs
            modName = T.intercalate "." modParts
        -- curRope is already a single line, but it may include an enclosing '\n'
        let curLine = T.dropWhileEnd (== '\n') $ Rope.toText curRope
        return $ PosPrefixInfo curLine modName x pos
