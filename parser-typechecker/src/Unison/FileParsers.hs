{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language UnicodeSyntax     #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.FileParsers where

import           Control.Monad.State (evalStateT)
import           Data.ByteString (ByteString)
import           Data.Bytes.Put (runPutS)
import qualified Data.Foldable as Foldable
import           Data.Functor.Identity (runIdentity)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Tuple (swap)
import qualified Unison.Builtin as B
import qualified Unison.Codecs as Codecs
import           Unison.DataDeclaration (DataDeclaration')
import           Unison.Parser (Ann(..))
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Unison.PrintError as PrintError
import           Unison.Reference (Reference(..))
import           Unison.Result (Result(..), Note)
import qualified Unison.Result as Result
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import qualified Unison.Typechecker as Typechecker
import           Unison.UnisonFile (pattern UnisonFile)
import qualified Unison.UnisonFile as UF
import           Unison.Var (Var)
import qualified Unison.Var as Var

type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type UnisonFile v = UF.UnisonFile v Ann

parseAndSynthesizeAsFile
  :: Var v
  => FilePath
  -> String
  -> Result (Note v Ann) (PrintError.Env, Maybe (Term v, Type v))
parseAndSynthesizeAsFile filename s = do
  (errorEnv, file) <- Result.fromParsing
    $ Parsers.parseFile filename s Parser.penv0
  let (Result notes' r) = synthesizeFile file
  Result notes' $ Just (errorEnv, r)

synthesizeFile
  :: forall v . Var v => UnisonFile v -> Result (Note v Ann) (Term v, Type v)
synthesizeFile unisonFile
  = let
      (UnisonFile dds0 eds0 term) =
        UF.bindBuiltins B.builtinTerms B.builtinTypes unisonFile
      dds :: Map Reference (DataDeclaration v)
      dds     = Map.fromList $ Foldable.toList dds0
      eds     = Map.fromList $ Foldable.toList eds0
      datas   = Map.union dds B.builtinDataDecls -- `Map.union` is left-biased
      effects = Map.union eds B.builtinEffectDecls
      env0    = Typechecker.Env Intrinsic
                                []
                                typeOf
                                dataDeclaration
                                effectDeclaration
                                unqualifiedLookup
      n = Typechecker.synthesizeAndResolve env0 term
      die s h = error $ "unknown " ++ s ++ " reference " ++ show h
      typeOf r =
        pure . fromMaybe (error $ "unknown reference " ++ show r)
          $ Map.lookup r typeSigs
      dataDeclaration r = pure $ fromMaybe (die "data" r) $ Map.lookup r datas
      effectDeclaration r =
        pure $ fromMaybe (die "effect" r) $ Map.lookup r effects
      unqualified = last . Text.splitOn "."
      typeSigs    = Map.fromList $ fmap
        (\(v, (_tm, typ)) -> (Builtin (Var.name v), typ))
        B.builtinTypedTerms
      unqualifiedLookup = Map.fromListWith mappend $ fmap
        (\(v, (_tm, typ)) ->
          ( unqualified $ Var.name v
          , [Typechecker.NamedReference (Var.name v) typ]
          )
        )
        B.builtinTypedTerms
    in
      swap <$> runIdentity n

synthesizeUnisonFile :: Var v
                     => UnisonFile v
                     -> Result (Note v Ann) (UnisonFile v, Type v)
synthesizeUnisonFile unisonFile@(UnisonFile d e _t) = do
  (t', typ) <- synthesizeFile unisonFile
  pure $ (UnisonFile d e t', typ)

serializeUnisonFile :: Var v => UnisonFile v
                             -> Result (Note v Ann)
                                       (UnisonFile v, Type v, ByteString)
serializeUnisonFile unisonFile =
  let r = synthesizeUnisonFile unisonFile
      f (unisonFile', typ) =
        let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile'
        in (unisonFile', typ, bs)
  in f <$> r
