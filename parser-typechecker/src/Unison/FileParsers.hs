{-# Language OverloadedStrings #-}
{-# Language TupleSections     #-}

module Unison.FileParsers where

import           Control.Monad.State (evalStateT)
import           Data.Bifunctor (second)
import           Data.ByteString (ByteString)
import           Data.Bytes.Put (runPutS)
import qualified Data.Foldable as Foldable
import           Data.Functor.Identity (runIdentity)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Trace
import qualified Unison.Builtin as B
import qualified Unison.Codecs as Codecs
import           Unison.DataDeclaration (DataDeclaration, toDataDecl)
import           Unison.Note (Noted)
import qualified Unison.Note as Note
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import           Unison.Reference (Reference)
import           Unison.Term (Term)
import           Unison.Type (Type)
import qualified Unison.Typechecker as Typechecker
import           Unison.UnisonFile (UnisonFile(..))
import qualified Unison.UnisonFile as UF
import           Unison.Var (Var)

parseAndSynthesizeAsFile :: Var v => FilePath -> String
                         -> Either String (Term v, Type v)
parseAndSynthesizeAsFile filename s = do
  file <- Parsers.parseFile filename s Parser.penv0
  synthesizeFile file

synthesizeFile :: Var v => UnisonFile v -> Either String (Term v, Type v)
synthesizeFile unisonFile =
  let (UnisonFile d e t) =
        UF.bindBuiltins B.builtinTerms B.builtinTypes unisonFile
      dataDecls =
        Map.union (Map.fromList . Foldable.toList $
                     Map.union d (second toDataDecl <$> e))
                  B.builtinDataDecls
      n = Note.attemptRun $
            Typechecker.synthesize [] termLookup (dataDeclLookup dataDecls) $ t
  in (t,) <$> runIdentity n

synthesizeUnisonFile :: Var v
                     => UnisonFile v
                     -> Either String (UnisonFile v, Type v)
synthesizeUnisonFile unisonFile@(UnisonFile d e _t) = do
  (t', typ) <- synthesizeFile unisonFile
  pure $ (UnisonFile d e t', typ)

serializeUnisonFile :: Var v => UnisonFile v
                             -> Either String (UnisonFile v, Type v, ByteString)
serializeUnisonFile unisonFile =
  let r = synthesizeUnisonFile unisonFile
      f (unisonFile', typ) =
        let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile'
        in (unisonFile', typ, bs)
  in f <$> r


termLookup :: (Applicative f, Var v) => Reference -> Noted f (Type v)
termLookup h = Maybe.fromMaybe (missing h) (pure <$> Map.lookup h B.builtins)

dataDeclLookup :: Applicative f
               => Map Reference (DataDeclaration v)
               -> Reference
               -> Noted f (DataDeclaration v)
dataDeclLookup dataDecls h =
  let _ = Trace.trace $ "dataDeclLookup: " ++ show h in
  Maybe.fromMaybe (missingD h) (pure <$> Map.lookup h dataDecls)

missing :: (Applicative m, Show a) => a -> Noted m b
missing h = Note.failure $ "no match looking up type of term reference: " ++ show h

missingD :: (Applicative m, Show a) => a -> Noted m b
missingD h = Note.failure $ "no match looking up type of data declaration reference: " ++ show h
