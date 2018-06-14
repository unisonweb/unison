{-# Language OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Unison.Codecs as Codecs
import Data.Bytes.Put (runPutS)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Control.Monad.State (evalStateT)
import qualified Unison.Builtin as B
import qualified Unison.FileParsers as FileParsers
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Unison.Term as Term
import qualified Unison.Typechecker as Typechecker
import qualified Unison.UnisonFile as UF
import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map

import Unison.DataDeclaration (DataDeclaration)
import qualified Unison.Note as Note
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile (UnisonFile)
import Unison.Var (Var)
import Debug.Trace (trace)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile, outputFile] -> do
      unisonFile <- Parsers.unsafeReadAndParseFile Parser.penv0 sourceFile
      let r = FileParsers.synthesizeFile unisonFile
          f typ = do
            putStrLn ("typechecked as " ++ show typ)
            let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile
            BS.writeFile outputFile bs
      either (putStrLn . show) (f . snd) r

    _ -> putStrLn "usage: bootstrap <in-file.u> <out-file.ub>"
