{-# Language OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (die)
import qualified Unison.FileParsers as FileParsers
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Data.ByteString as BS
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import Data.Text (unpack)
import qualified Data.Text.IO
import Unison.Util.Monoid
import Unison.PrintError (printNoteWithSource, env0)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile, outputFile] -> do
      unisonFile <- Parsers.unsafeReadAndParseFile Parser.penv0 sourceFile
      source <- unpack <$> Data.Text.IO.readFile sourceFile
      let r = Result.toEither $ FileParsers.serializeUnisonFile unisonFile
          f (_unisonFile', typ, bs) = do
            putStrLn ("typechecked as " ++ show typ)
            BS.writeFile outputFile bs
          showNote :: [Result.Note Symbol Parser.Ann] -> String
          showNote notes =
            intercalateMap "\n\n" (printNoteWithSource env0 source) notes
      either (die . showNote . reverse) f r

    _ -> putStrLn "usage: bootstrap <in-file.u> <out-file.ub>"
