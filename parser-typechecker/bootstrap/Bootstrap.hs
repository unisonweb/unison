{-# Language OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Unison.FileParsers as FileParsers
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile, outputFile] -> do
      unisonFile <- Parsers.unsafeReadAndParseFile Parser.penv0 sourceFile
      let r = FileParsers.serializeUnisonFile unisonFile
          f (_unisonFile', typ, bs) = do
            putStrLn ("typechecked as " ++ show typ)
            BS.writeFile outputFile bs
      either print f r

    _ -> putStrLn "usage: bootstrap <in-file.u> <out-file.ub>"
