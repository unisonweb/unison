{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text          (unpack)
import qualified Data.Text.IO
import           System.Environment (getArgs)
import Unison.Runtime.Rt0 (parseAndNormalize')

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile]             -> go sourceFile
    _                        -> do
      putStrLn "usage:"
      putStrLn "  haskellruntimetest <in-file.u>"
 where
  go :: String -> IO ()
  go sourceFile = do
    source             <- unpack <$> Data.Text.IO.readFile sourceFile
    putStrLn . parseAndNormalize' $ source
