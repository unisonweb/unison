{-# LANGUAGE TupleSections #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language UnicodeSyntax     #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.FileParsers where

import           Control.Monad.Writer (tell)
import qualified Unison.Term as Term
import qualified Unison.ABT as ABT
import           Control.Monad (foldM)
import           Control.Monad.State (runStateT, evalStateT)
import           Data.Bytes.Put (runPutS)
import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import           Data.Functor.Identity (runIdentity, Identity(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Text (Text, unpack)
import qualified Unison.Builtin as B
import qualified Unison.Codecs as Codecs
import           Unison.DataDeclaration (DataDeclaration')
import           Unison.Parser (Ann(Intrinsic))
import qualified Unison.Parsers as Parsers
import           Unison.Reference (Reference, pattern Builtin)
import           Unison.Result (Result(..), Note(..))
import qualified Unison.Result as Result
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Typechecker.Context as Context
import           Unison.UnisonFile (pattern UnisonFile)
import qualified Unison.UnisonFile as UF
import           Unison.Var (Var)
import qualified Unison.Var as Var
import Unison.Names (Names(..))
import qualified Unison.Names as Names
import qualified Unison.PrettyPrintEnv as PPE

type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type UnisonFile v = UF.UnisonFile v Ann

convertNotes :: Typechecker.Notes v ann -> Seq (Note v ann)
convertNotes (Typechecker.Notes es is) =
  (TypeError <$> es) <> (TypeInfo <$> is)

parseAndSynthesizeFile
  :: Var v
  => Names v Ann
  -> FilePath
  -> Text
  -> Result
       (Seq (Note v Ann))
       (PPE.PrettyPrintEnv, Maybe (UnisonFile v))
parseAndSynthesizeFile names filePath src = do
  (errorEnv, parsedUnisonFile) <- Result.fromParsing
    $ Parsers.parseFile filePath (unpack src) names
  let (Result notes' r) = synthesizeUnisonFile names parsedUnisonFile
  Result notes' $ Just (errorEnv, fst <$> r)

synthesizeFile
  :: forall v
   . Var v
  => Names v Ann
  -> UnisonFile v
  -> Result (Seq (Note v Ann)) (Term v, Type v)
synthesizeFile names unisonFile
  = let
      (UnisonFile dds0 eds0 term) = UF.bindBuiltins B.names unisonFile
      dds :: Map Reference (DataDeclaration v)
      dds     = Map.fromList $ Foldable.toList dds0
      eds     = Map.fromList $ Foldable.toList eds0
      -- note: `Map.union` is left-biased
      datas   = Map.union dds (Map.fromList $ snd <$> B.builtinDataDecls)
      effects = eds
      env0    = Typechecker.Env Intrinsic
                                []
                                typeOf
                                dataDeclaration
                                effectDeclaration
                                unqualifiedLookup
      n = Typechecker.synthesizeAndResolve env0
      die s h = error $ "unknown " ++ s ++ " reference " ++ show h
      typeOf r =
        pure . fromMaybe (error $ "unknown reference " ++ show r) $ Map.lookup
          r
          typeSigs
      dataDeclaration r = pure $ fromMaybe (die "data" r) $ Map.lookup r datas
      effectDeclaration r =
        pure $ fromMaybe (die "effect" r) $ Map.lookup r effects
      typeSigs = Map.fromList $ fmap
        (\(v, (_tm, typ)) -> (Builtin (Var.name v), typ))
        B.builtinTypedTerms
      unqualifiedLookup = Map.fromListWith mappend $ fmap
        (\(v, (_tm, typ)) ->
          ( Var.unqualified v
          , [Typechecker.NamedReference (Var.name v) typ True]
          )
        )
        B.builtinTypedTerms
      (Result notes mayType, _) = runIdentity $ runStateT n term
      decisions =
        [ (v, loc, fqn) |
          Context.Decision v loc fqn <- Foldable.toList $ Typechecker.infos notes ]
      substedTerm = foldM go term decisions
      go term (v, loc, fqn) = ABT.visit (resolve v loc fqn) term
      resolve v loc fqn t@(Term.Var' v') | ABT.annotation t == loc && v == v' =
        case Names.lookupTerm names fqn of
          Nothing ->
            Just $ (tell . pure $ ResolvedNameNotFound v loc fqn) *> pure t
          Just ref -> Just $ pure (const loc <$> ref)
      resolve _ _ _ _ = Nothing
   in do
     t <- substedTerm
     Result (convertNotes notes) ((t,) <$> mayType)

synthesizeUnisonFile :: Var v
                     => Names v Ann
                     -> UnisonFile v
                     -> Result (Seq (Note v Ann)) (UnisonFile v, Type v)
synthesizeUnisonFile names unisonFile@(UnisonFile d e _t) = do
  (t', typ) <- synthesizeFile names unisonFile
  pure $ (UnisonFile d e t', typ)

serializeUnisonFile
  :: Var v
  => Names v Ann
  -> UnisonFile v
  -> Result (Seq (Note v Ann)) (UnisonFile v, Type v, ByteString)
serializeUnisonFile names unisonFile =
  let r = synthesizeUnisonFile names unisonFile
      f (unisonFile', typ) =
        let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile'
        in  (unisonFile', typ, bs)
  in  f <$> r

