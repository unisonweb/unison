{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Unison.FileParsers where

import           Control.Monad              (foldM)
import           Control.Monad.Trans        (lift)
import           Control.Monad.State        (evalStateT)
import Control.Monad.Writer (tell)
import           Data.Bytes.Put             (runPutS)
import           Data.ByteString            (ByteString)
import qualified Data.Foldable              as Foldable
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import Data.Set (Set)
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
import           Unison.Names               (Name, Names)
import qualified Unison.Names               as Names
import           Unison.Parser              (Ann (Intrinsic, External))
import qualified Unison.Parsers             as Parsers
import qualified Unison.PrettyPrintEnv      as PPE
import           Unison.Reference           (pattern Builtin, Reference)
import qualified Unison.Reference as Reference
import           Unison.Result              (Note (..), Result, pattern Result, ResultT)
import qualified Unison.Result              as Result
import           Unison.Term                (AnnotatedTerm)
import qualified Unison.Term                as Term
import           Unison.Type                (AnnotatedType)
import qualified Unison.Typechecker         as Typechecker
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Typechecker.Context as Context
import           Unison.UnisonFile          (pattern UnisonFile)
import qualified Unison.UnisonFile          as UF
import           Unison.Var                 (Var)
import qualified Unison.Var                 as Var
import qualified Unison.Codebase as Codebase

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
  :: (Var v, Monad m)
  => (Set Reference -> Set Reference.Id -> m (TL.TypeLookup v Ann))
  -> Names
  -> FilePath
  -> Text
  -> ResultT
       (Seq (Note v Ann))
       m
       (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile' v Ann))
parseAndSynthesizeFile typeLookupf names filePath src = do
  (errorEnv, parsedUnisonFile) <- Result.fromParsing
    $ Parsers.parseFile filePath (unpack src) names
  let (termRefs, typeRefs) = UF.dependencies parsedUnisonFile names
  typeLookup <- lift . lift $ typeLookupf termRefs typeRefs
  let (Result notes' r) = synthesizeFile typeLookup names parsedUnisonFile
  tell notes' *> pure (errorEnv, r)

synthesizeFile
  :: forall v
   . Var v
  => TL.TypeLookup v Ann
  -> Names
  -> UnisonFile v
  -> Result (Seq (Note v Ann)) (UF.TypecheckedUnisonFile' v Ann)
synthesizeFile lookupType preexistingNames unisonFile = do
  let
    -- substitute builtins into the datas/effects/body of unisonFile
    uf@(UnisonFile dds0 eds0 term0) = unisonFile
    localNames = UF.toNames uf
    localTypes = UF.toTypeLookup uf
    term = Names.bindTerm (localNames <> preexistingNames) term0
    -- substitute Blanks for any remaining free vars in UF body
    tdnrTerm                        = Term.prepareTDNR $ term
    lookupTypes' = localTypes <> lookupType
    -- merge dds from unisonFile with dds from Unison.Builtin
    -- note: `Map.union` is left-biased
    env0 = (Typechecker.Env Intrinsic [] lookupTypes' fqnsByShortName)
     where
      lookupData :: Applicative f => Reference -> f (DataDeclaration v)
      lookupData r = pure $ fromMaybe (die "data" r) $ Map.lookup r datasr
        where datasr = Map.fromList $ Foldable.toList datas
      lookupEffect :: Applicative f => Reference -> f (EffectDeclaration v)
      lookupEffect r = pure $ fromMaybe (die "effect" r) $ Map.lookup
        r
        effectsr
        where effectsr = Map.fromList $ Foldable.toList effects
      die s h = error $ "unknown " ++ s ++ " reference " ++ show h
      fqnsByShortName :: Map Name [Typechecker.NamedReference v Ann]
      fqnsByShortName =
        Map.fromListWith mappend . fmap toKV . Map.toList $ Names.termNames
          allTheNames
       where
        toKV (name, (_, typ)) =
          ( Var.unqualified (Var.named @v name)
          , [Typechecker.NamedReference name typ True]
          )
      typeOf r =
        pure . fromMaybe (error $ "unknown reference " ++ show r) $ Map.lookup
          r
          typeOfRef
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
        tlcsFromTypechecker =
          uniqueBy' (fmap vars) [ t | Context.TopLevelComponent t <- infos ]
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
    let lookupName =
              (MaybeT $ flip Map.lookup tlcMap)
          <|> (MaybeT $ Names.lookupTerm External allTheNames)
          where go (v, t, tp) = (Var.name v, (Term.var (ABT.annotation t) v, tp))
                tlcMap = Map.fromList . fmap go $ join topLevelComponents
        doTdnr   = applyTdnrDecisions lookupName infos
        doTdnrInComponent (v, t, tp) = (\t -> (v, t, tp)) <$> doTdnr t
    t          <- doTdnr tdnrTerm
    tdnredTlcs <- (traverse . traverse) doTdnrInComponent topLevelComponents
    pure (UF.TypecheckedUnisonFile' dds0 eds0 tdnredTlcs t typ)
 where
  applyTdnrDecisions
    :: (Name -> Maybe (Term v))
    -> [Context.InfoNote v Ann]
    -> Term v
    -> Result' v (Term v)
  applyTdnrDecisions names infos tdnrTerm = foldM go tdnrTerm decisions
   where
    -- UF data/effect ctors + builtins + TLC Term.vars
    go term _decision@(shortv, loc, fqn) =
      ABT.visit (resolve shortv loc fqn) term
    decisions = [ (v, loc, fqn) | Context.Decision v loc fqn <- infos ]
    -- resolve (v,loc) in a matching Blank to whatever `fqn` maps to in `names`
    resolve shortv loc fqn t = case t of
      Term.Blank' (Blank.Recorded (Blank.Resolve loc' name))
        | loc' == loc && Var.nameStr shortv == name -> case names fqn of
          Nothing -> Just . Result.compilerBug $ Result.ResolvedNameNotFound
            shortv
            loc
            fqn
          Just ref -> Just $ pure (const loc <$> ref)
      _ -> Nothing

synthesizeAndSerializeUnisonFile
  :: Var v
  => Names
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
