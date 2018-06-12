{-# Language OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Unison.Codecs as Codecs
import Data.Bytes.Put (runPutS)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Control.Monad.State (evalStateT)
import qualified Unison.Builtin as B
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
import Unison.Type (Type)
import Debug.Trace (trace)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile, outputFile] -> do
      unisonFile <- Parsers.unsafeReadAndParseFile Parser.penv0 sourceFile
      let dataDecls = Map.fromList . toList $ UF.dataDeclarations unisonFile
      let t = B.resolveBuiltins B.builtinTerms Term.builtin $ UF.term unisonFile
      typ <- Note.run $ Typechecker.synthesize termLookup (dataDeclLookup dataDecls) t
      putStrLn ("typechecked as " ++ show typ)
      let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile
      BS.writeFile outputFile bs

    _ -> putStrLn "usage: bootstrap <in-file.u> <out-file.ub>"

termLookup :: Applicative f => Reference -> Noted f (Type Symbol)
termLookup h = fromMaybe (missing h) (pure <$> Map.lookup h B.builtins)

dataDeclLookup :: Applicative f
               => Map Reference (DataDeclaration Symbol)
               -> Reference
               -> Noted f (DataDeclaration Symbol)
dataDeclLookup dataDecls h =
  let _ = trace $ "dataDeclLookup: " ++ show h in
  fromMaybe (missingD h) (pure <$> Map.lookup h dataDecls)

missing h = Note.failure $ "no match looking up type of term reference: " ++ show h
missingD h = Note.failure $ "no match looking up type of data declaration reference: " ++ show h
