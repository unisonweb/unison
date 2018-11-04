{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Unison.FileParsers where

import           Control.Monad              (foldM)
import           Control.Monad.State        (evalStateT)
import           Data.Bytes.Put             (runPutS)
import           Data.ByteString            (ByteString)
import qualified Data.Foldable              as Foldable
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.Maybe
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text, unpack)
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
type Result' v = Result (Seq (Note v Ann))

-- move to Unison.Util.List
-- prefers earlier copies
uniqueBy :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy f as = wrangle' (Foldable.toList as) Set.empty where
  wrangle' [] _ = []
  wrangle' (a:as) seen =
    if Set.member b seen
    then wrangle' as seen
    else a : wrangle' as (Set.insert b seen)
    where b = f a

-- prefers later copies
uniqueBy' :: (Foldable f, Ord b) => (a -> b) -> f a -> [a]
uniqueBy' f = reverse . uniqueBy f . reverse . Foldable.toList

convertNotes :: Ord v => Typechecker.Notes v ann -> Seq (Note v ann)
convertNotes (Typechecker.Notes es is) =
  (TypeError <$> es) <> (TypeInfo <$> Seq.fromList is') where
  is' = snd <$> uniqueBy' f ([(1::Word)..] `zip` Foldable.toList is)
  f (_, (Context.TopLevelComponent cs)) = Right [ v | (v,_,_) <- cs ]
  f (i, _) = Left i
  -- each round of TDNR emits its own TopLevelComponent notes, so we remove
  -- duplicates (based on var name and location), preferring the later note as
  -- that will have the latest typechecking info

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
    -- substitute builtins into the datas/effects/body of unisonFile
    uf@(UnisonFile dds0 eds0 term0) = UF.bindBuiltins builtinNames unisonFile
    -- map Names to Term.constructor values for UF data/effect ctors
    ufDeclNames                     = UF.toNames uf
    allTheNames                     = ufDeclNames <> builtinNames
    -- substitute locally defined constructors for Vars into the UF body
    term                            = Names.bindTerm ufDeclNames term0
    -- substitute Blanks for any remaining free vars in UF body
    tdnrTerm                        = Term.prepareTDNR $ term
    -- merge dds from unisonFile with dds from Unison.Builtin
    -- note: `Map.union` is left-biased
    datas :: Map v (Reference, DataDeclaration v)
    datas = Map.union dds0 $ Map.fromList B.builtinDataDecls
    -- same, but there are no eds in Unison.Builtin yet.
    effects = Map.union eds0 $ Map.fromList B.builtinEffectDecls
    env0 =
      (Typechecker.Env
        Intrinsic
        []
        typeOf
        lookupData
        lookupEffect
        fqnsByShortName
      )
     where
      lookupData :: Applicative f => Reference -> f (DataDeclaration v)
      lookupData r = pure $ fromMaybe (die "data" r) $ Map.lookup r datasr
        where datasr = Map.fromList $ Foldable.toList datas
      lookupEffect :: Applicative f => Reference -> f (EffectDeclaration v)
      lookupEffect r =
        pure $ fromMaybe (die "effect" r) $ Map.lookup r effectsr
          where effectsr = Map.fromList $ Foldable.toList effects
      die s h = error $ "unknown " ++ s ++ " reference " ++ show h
      fqnsByShortName :: Map Name [Typechecker.NamedReference v Ann]
      fqnsByShortName = Map.fromListWith mappend
                                         (fmap toKV B.builtinTypedTerms)
       where
        toKV (v, (_, typ)) =
          ( Var.unqualified v
          , [Typechecker.NamedReference (Var.name v) typ True]
          )
      typeOf :: Applicative f => Reference -> f (Type v)
      typeOf r = pure . fromMaybe (error $ "unknown reference " ++ show r) $
        Map.lookup r typeSigs
       where
        typeSigs = Map.fromList $ fmap go B.builtinTypedTerms
        go (v, (_tm, typ)) = (Builtin (Var.name v), typ)
    Result notes mayType =
      evalStateT (Typechecker.synthesizeAndResolve env0) tdnrTerm
  Result (convertNotes notes) mayType >>= \typ -> do
    let infos = Foldable.toList $ Typechecker.infos notes
    topLevelComponents <- -- :: [[(v, Term v, Type v)]]
      let
        topLevelBindings :: Map Name (Term v)
        topLevelBindings = Map.mapKeys Var.name $ extractTopLevelBindings term
        extractTopLevelBindings (Term.LetRecNamed' bs _) = Map.fromList bs
        extractTopLevelBindings _                        = Map.empty
        tlcsFromTypechecker = uniqueBy' (fmap vars) [ t | Context.TopLevelComponent t <- infos ]
        vars (v, _, _) = Var.name v
        strippedTopLevelBinding (v, typ, redundant) = do
          tm <- case Map.lookup (Var.name v) topLevelBindings of
            Nothing ->
              Result.compilerBug $ Result.TopLevelComponentNotFound v term
            Just (Term.Ann' x _) | redundant -> pure x
            Just x                           -> pure x
          pure (v, tm, typ)
      in
        -- use tlcsFromTypechecker to inform annotation-stripping decisions
        traverse (traverse strippedTopLevelBinding) tlcsFromTypechecker
    let tlcNames = Names.varsFromComponents topLevelComponents
        doTdnr   = applyTdnrDecisions infos (tlcNames <> allTheNames)
        doTdnrInComponent (v, t, tp) = (\t -> (v, t, tp)) <$> doTdnr t
    t <- doTdnr tdnrTerm
    tdnredTlcs <- (traverse . traverse) doTdnrInComponent topLevelComponents
    pure (UF.TypecheckedUnisonFile' dds0 eds0 tdnredTlcs t typ)
 where
  applyTdnrDecisions
    :: [Context.InfoNote v Ann] -> Names v Ann -> Term v -> Result' v (Term v)
  applyTdnrDecisions infos names tdnrTerm = foldM go tdnrTerm decisions
   where
    -- UF data/effect ctors + builtins + TLC Term.vars
    go term _decision@(shortv, loc, fqn) =
      ABT.visit (resolve shortv loc fqn) term
    decisions = [ (v, loc, fqn) | Context.Decision v loc fqn <- infos ]
    -- resolve (v,loc) in a matching Blank to whatever `fqn` maps to in `names`
    resolve shortv loc fqn t = case t of
      Term.Blank' (Blank.Recorded (Blank.Resolve loc' name))
        | loc' == loc && Var.nameStr shortv == name
        -> case Names.lookupTerm names fqn of
          Nothing -> Just . Result.compilerBug $
            Result.ResolvedNameNotFound shortv loc fqn
          Just ref -> Just $ pure (const loc <$> ref)
      _ -> Nothing

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
