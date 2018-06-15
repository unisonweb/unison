{-# Language OverloadedStrings #-}
{-# Language TupleSections     #-}

module Unison.FileParsers where

import Data.Map (Map)
import Data.Functor.Identity (runIdentity)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Trace
import qualified Unison.Builtin as B
import qualified Unison.Note as Note
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Unison.Term as Term
import qualified Unison.Typechecker as Typechecker
import qualified Unison.UnisonFile as UF
import Unison.DataDeclaration (DataDeclaration)
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile (UnisonFile(..))
import Unison.Var (Var)

parseAndSynthesizeAsFile :: Var v => FilePath -> String
                         -> Either String (Term v, Type v)
parseAndSynthesizeAsFile filename s = do
  file <- Parsers.parseFile filename s Parser.penv0
  synthesizeFile file

synthesizeFile :: Var v => UnisonFile v -> Either String (Term v, Type v)
synthesizeFile unisonFile =
  let dataDecls =
        Map.union (Map.fromList . Foldable.toList $ UF.dataDeclarations unisonFile)
                  B.builtinDataDecls
      t = Term.bindBuiltins B.builtinTerms B.builtinTypes $ UF.term unisonFile
      n = Note.attemptRun $ Typechecker.synthesize termLookup (dataDeclLookup dataDecls) $ t
  in (t,) <$> runIdentity n

synthesizeUnisonFile :: Var v => UnisonFile v -> Either String (UnisonFile v, Type v)
synthesizeUnisonFile unisonFile@(UnisonFile d e _t) = do
  (t', typ) <- synthesizeFile unisonFile
  pure $ (UnisonFile d e t', typ)

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
