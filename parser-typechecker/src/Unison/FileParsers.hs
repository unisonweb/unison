{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Unison.FileParsers where

import           Control.Monad              (foldM, join)
import           Control.Monad.State        (evalStateT)
import           Data.Bytes.Put             (runPutS)
import           Data.ByteString            (ByteString)
import qualified Data.Foldable              as Foldable
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Sequence              (Seq)
import           Data.Text                  (Text, unpack)
import           Debug.Trace
import qualified Unison.ABT                 as ABT
import qualified Unison.Blank               as Blank
import qualified Unison.Builtin             as B
import qualified Unison.Codecs              as Codecs
import           Unison.DataDeclaration     (DataDeclaration',
                                             EffectDeclaration')
import           Unison.Names               (Name, Names (..))
import qualified Unison.Names               as Names
import           Unison.Parser              (Ann (Intrinsic))
import qualified Unison.Parsers             as Parsers
import qualified Unison.PrettyPrintEnv      as PPE
import           Unison.Reference           (pattern Builtin, Reference)
import           Unison.Result              (Note (..), Result, pattern Result)
import qualified Unison.Result              as Result
import           Unison.Term                (AnnotatedTerm)
import qualified Unison.Term                as Term
import           Unison.Type                (AnnotatedType)
import qualified Unison.Typechecker         as Typechecker
import qualified Unison.Typechecker.Context as Context
import           Unison.UnisonFile          (pattern UnisonFile)
import qualified Unison.UnisonFile          as UF
import           Unison.Var                 (Var)
import qualified Unison.Var                 as Var

type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type EffectDeclaration v = EffectDeclaration' v Ann
type UnisonFile v = UF.UnisonFile v Ann
type NamedReference v = Typechecker.NamedReference v Ann

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
       (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile' v Ann))
parseAndSynthesizeFile names filePath src = do
  (errorEnv, parsedUnisonFile) <- Result.fromParsing
    $ Parsers.parseFile filePath (unpack src) names
  let (Result notes' r) = synthesizeFile names parsedUnisonFile
  Result notes' $ Just (errorEnv, r)

synthesizeFile
  :: forall v
   . Var v
  => Names v Ann
  -> UnisonFile v
  -> Result (Seq (Note v Ann)) (UF.TypecheckedUnisonFile' v Ann)
synthesizeFile builtinNames unisonFile = do
  let
    uf@(UnisonFile dds0 eds0 term0) = UF.bindBuiltins builtinNames unisonFile
    ufDeclNames                     = UF.toNames uf
    allTheNames                     = ufDeclNames <> builtinNames
    term                            = Names.bindTerm allTheNames term0
    tdnrTerm                        = Term.prepareTDNR $ term
    -- merge dds from unisonFile with dds from Unison.Builtin
    -- note: `Map.union` is left-biased
    datas :: Map v (Reference, DataDeclaration v)
    datas = Map.union dds0 $ Map.fromList B.builtinDataDecls
    -- same, but there are no eds in Unison.Builtin yet.
    effects = Map.union eds0 $ Map.fromList B.builtinEffectDecls
    env0 = Typechecker.Env Intrinsic [] typeOf lookupData lookupEffect builtins
     where
      lookupData r = pure $ fromMaybe (die "data" r) $ Map.lookup r datasr
      lookupEffect r =
        pure $ fromMaybe (die "effect" r) $ Map.lookup r effectsr
      die s h = error $ "unknown " ++ s ++ " reference " ++ show h
      builtins :: Map Name [Typechecker.NamedReference v Ann]
      builtins = Map.fromListWith mappend (fmap toNamedRef B.builtinTypedTerms)
      toNamedRef (v, (_tm, typ)) =
        (Var.unqualified v, [Typechecker.NamedReference (Var.name v) typ True])
      datasr :: Map Reference (DataDeclaration v)
      datasr = Map.fromList $ Foldable.toList datas
      effectsr :: Map Reference (EffectDeclaration v)
      effectsr = Map.fromList $ Foldable.toList effects
      typeOf r =
        pure . fromMaybe (error $ "unknown reference " ++ show r) $ Map.lookup
          r
          typeSigs
      typeSigs = Map.fromList $ fmap go B.builtinTypedTerms
        where go (v, (_tm, typ)) = (Builtin (Var.name v), typ)
    Result notes mayType =
      evalStateT (Typechecker.synthesizeAndResolve env0) tdnrTerm
    infos = Foldable.toList $ Typechecker.infos notes
  topLevelComponents <-
    let
      components :: Map Name (Term v)
      components          = Map.mapKeys Var.name $ extractComponents term
      tlcsFromTypechecker = [ t | Context.TopLevelComponent t <- infos ]
      substTLC (v, typ, redundant) = do
        tm <- case Map.lookup (Var.name v) components of
          Nothing ->
            Result.compilerBug $ Result.TopLevelComponentNotFound v term
          Just (Term.Ann' x _) | redundant -> pure x
          Just x                           -> pure x
        pure (v, tm, typ)
    in
      traverse (traverse substTLC) tlcsFromTypechecker
  let
    names       = allTheNames <> Names.fromTermsV' (join topLevelComponents)
    substedTerm = (\x -> traceShow ("substedTerm" :: String, x) x)
      $ foldM go tdnrTerm decisions
     where
      go term (v, loc, fqn)
        | traceShow ("go" :: String, term, (v, loc, fqn)) False = undefined
      go term (v, loc, fqn) = ABT.visit (resolve v loc fqn) term
      decisions = [ (v, loc, fqn) | Context.Decision v loc fqn <- infos ]
      -- resolve (v,loc) in a matching Blank to whatever `fqn` maps to in `names`
      resolve v loc fqn t
        | traceShow ("resolve" :: String, v, loc, fqn, t) False = undefined
      resolve v loc fqn t = case t of
        Term.Blank' (Blank.Recorded (Blank.Resolve loc' name))
          | loc' == loc && Var.nameStr v == name
          -> trace "BLANK" $ case Names.lookupTerm names fqn of
            Nothing -> Just . Result.compilerBug $ traceShowId
              (Result.ResolvedNameNotFound v loc fqn)
            Just ref -> Just $ pure (const loc <$> ref)
        _ -> Nothing
  t <- substedTerm
  Result
    (convertNotes notes)
    (UF.TypecheckedUnisonFile' datas effects topLevelComponents t <$> mayType)

extractComponents :: Var v => Term v -> Map v (Term v)
extractComponents (Term.LetRecNamed' bs _) = Map.fromList bs
extractComponents _                        = Map.empty

synthesizeAndSerializeUnisonFile
  :: Var v
  => Names v Ann
  -> UnisonFile v
  -> Result
       (Seq (Note v Ann))
       (UF.TypecheckedUnisonFile' v Ann, ByteString)
synthesizeAndSerializeUnisonFile names unisonFile =
  let r = (\x -> traceShow ("synthesizedFile"::String, x) x) $ synthesizeFile names unisonFile
      f unisonFile' =
        let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile
              (UF.discardTypes' unisonFile')
        in  (unisonFile', bs)
  in  f <$> r
