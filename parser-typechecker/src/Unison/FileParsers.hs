{-# LANGUAGE TupleSections #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language UnicodeSyntax     #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.FileParsers where

import           Control.Monad (foldM, join)
import           Control.Monad.State (evalStateT)
import           Control.Monad.Writer (tell)
import           Data.Bytes.Put (runPutS)
import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Text (Text, unpack)
import qualified Unison.ABT as ABT
import qualified Unison.Blank as Blank
import qualified Unison.Builtin as B
import qualified Unison.Codecs as Codecs
import           Unison.DataDeclaration (DataDeclaration')
import           Unison.Names (Names(..))
import qualified Unison.Names as Names
import           Unison.Parser (Ann(Intrinsic))
import qualified Unison.Parsers as Parsers
import qualified Unison.PrettyPrintEnv as PPE
import           Unison.Reference (pattern Builtin)
import           Unison.Result (pattern Result, Result, Note(..))
import qualified Unison.Result as Result
import qualified Unison.Term as Term
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (AnnotatedType)
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Typechecker.Context as Context
import           Unison.UnisonFile (pattern UnisonFile)
import qualified Unison.UnisonFile as UF
import           Unison.Var (Var)
import qualified Unison.Var as Var
import Debug.Trace
import Unison.Util.Monoid

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
synthesizeFile names0 unisonFile = do
  let
    uf@(UnisonFile dds0 eds0 term0) = UF.bindBuiltins names0 unisonFile
    names1                          = UF.toNames uf
    names2                          = names1 <> names0
    term                            = Names.bindTerm names2 term0
    tdnrTerm                        = Term.prepareTDNR $ term
    dds                             = Map.fromList $ Map.toList dds0
    eds                             = Map.fromList $ Map.toList eds0
    -- merge dds from unisonFile with dds from Unison.Builtin
    -- note: `Map.union` is left-biased
    datas = Map.union dds $ Map.fromList B.builtinDataDecls
    datasr                          = Map.fromList $ Foldable.toList datas
    -- same, but there are no eds in Unison.Builtin yet.
    effects                         = eds
    effectsr                        = Map.fromList $ Foldable.toList effects
    env0                            = Typechecker.Env Intrinsic
                                                      []
                                                      typeOf
                                                      dataDeclaration
                                                      effectDeclaration
                                                      unqualifiedLookup
    die s h = error $ "unknown " ++ s ++ " reference " ++ show h
    typeOf r =
      pure . fromMaybe (error $ "unknown reference " ++ show r) $ Map.lookup
        r
        typeSigs
    dataDeclaration r = pure $ fromMaybe (die "data" r) $ Map.lookup r datasr
    effectDeclaration r =
      pure $ fromMaybe (die "effect" r) $ Map.lookup r effectsr
    typeSigs = Map.fromList $ fmap
      (\(v, (_tm, typ)) -> (Builtin (Var.name v), typ))
      B.builtinTypedTerms
    unqualifiedLookup = Map.fromListWith mappend $ fmap
      (\(v, (_tm, typ)) ->
        (Var.unqualified v, [Typechecker.NamedReference (Var.name v) typ True])
      )
      B.builtinTypedTerms
    Result notes mayType =
      evalStateT (Typechecker.synthesizeAndResolve env0) tdnrTerm
    infos               = Foldable.toList $ Typechecker.infos notes
    components          = trace ("---components:---\n" ++ cs) extractComponents term
      where cs = intercalateMap "\n" show ([(1::Int)..] `zip` (Map.toList $ extractComponents term))
    tlcsFromTypechecker = [ t | Context.TopLevelComponent t <- infos ]
    substTLC (v, typ, redundant) = do
      tm <- case Map.lookup v components of
        Nothing ->
          Result.tellAndFail
            . Result.CompilerBug
            $ Result.TopLevelComponentNotFound v term
        Just (Term.Ann' x _) | redundant -> pure x
        Just x                           -> pure x
      pure (v, tm, typ)
  topLevelComponents <- traverse (traverse substTLC) tlcsFromTypechecker
  let
    names       = names2 <> Names.fromTermsV' (join topLevelComponents)
    decisions   = [ (v, loc, fqn) | Context.Decision v loc fqn <- infos ]
    substedTerm = foldM go term decisions
      where go term (v, loc, fqn) = ABT.visit (resolve v loc fqn) term
    resolve v loc fqn t@(Term.Blank' (Blank.Recorded (Blank.Resolve loc' v')))
      | loc' == loc && Var.nameStr v == v' = case Names.lookupTerm names fqn of
        Nothing ->
          Just
            $  (tell . pure . Result.CompilerBug $ Result.ResolvedNameNotFound
                 v
                 loc
                 fqn
               )
            *> pure t
        Just ref -> Just $ pure (const loc <$> ref)
    resolve _ _ _ _ = Nothing
  t <- substedTerm
  Result
    (convertNotes notes)
    (UF.TypecheckedUnisonFile' datas effects topLevelComponents t <$> mayType)

extractComponents :: Var v => Term v -> Map v (Term v)
extractComponents (Term.LetRecNamed' bs _) = Map.fromList bs
extractComponents _ = Map.empty

synthesizeAndSerializeUnisonFile
  :: Var v
  => Names v Ann
  -> UnisonFile v
  -> Result
       (Seq (Note v Ann))
       (UF.TypecheckedUnisonFile' v Ann, ByteString)
synthesizeAndSerializeUnisonFile names unisonFile =
  let r = synthesizeFile names unisonFile
      f unisonFile' =
        let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile
              (UF.discardTypes' unisonFile')
        in  (unisonFile', bs)
  in  f <$> r
