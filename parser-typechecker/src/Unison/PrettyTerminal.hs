module Unison.PrettyTerminal where

import qualified Unison.Util.Pretty            as P
import qualified Unison.Util.ColorText         as CT
import qualified System.Console.Terminal.Size  as Terminal

-- like putPrettyLn' but prints a blank line before and after.
putPrettyLn :: P.Pretty CT.ColorText -> IO ()
putPrettyLn p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ P.border 2 p

putPrettyLn' :: P.Pretty CT.ColorText -> IO ()
putPrettyLn' p = do
  width <- getAvailableWidth
  putStrLn . P.toANSI width $ P.indentN 2 p

clearCurrentLine :: IO ()
clearCurrentLine = do
  width <- getAvailableWidth
  putStr "\r"
  putStr . replicate width $ ' '
  putStr "\r"

putPretty' :: P.Pretty CT.ColorText -> IO ()
putPretty' p = do
  width <- getAvailableWidth
  putStr . P.toANSI width $ p

getAvailableWidth :: IO Int
getAvailableWidth =
  maybe 80 (\s -> 100 `min` Terminal.width s) <$> Terminal.size
