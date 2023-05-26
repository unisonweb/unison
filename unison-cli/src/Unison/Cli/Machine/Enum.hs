module Unison.Cli.Machine.Enum
  ( enum,
  )
where

import Data.Array (bounds, listArray, (!))
import Data.Char (ord)
import System.Console.Haskeline
import Unison.Cli.Machine
import qualified Unison.Util.Pretty as P

enumInput :: String -> InputT IO (Maybe Int)
enumInput prompt = handleCtrlC do
  getInputChar prompt >>= \case
    Nothing -> pure Nothing
    Just c ->
      let i = ord c - 48
       in case 0 <= i && i <= 9 of
            True -> pure (Just i)
            False -> enumInput prompt

enum :: [(P.Pretty P.ColorText, a)] -> Machine IO () (Maybe a)
enum input =
  let options = listArray (1, length input) input
      pretties = map fst input
      lu i = case bounds options of
        (lb, ub) -> case lb <= i && i <= ub of
          True -> Just (options ! i)
          False -> Nothing
   in Machine
        ( \() -> do
            putStrLn (P.toAnsiUnbroken $ (P.numberedList pretties) <> P.newline)
            runInputTBehavior preferTerm defaultSettings (enumInput "")
        )
        \() mint -> do
          case mint of
            Nothing -> pure (Return () Nothing)
            Just i -> case lu i of
              Nothing -> pure (Continue ())
              Just (_, x) -> pure (Return () (Just x))
