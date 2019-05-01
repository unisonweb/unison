{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Unison.FileParsers where

import qualified Unison.Parser as Parser
import           Control.Monad              (foldM)
import           Control.Monad.Trans        (lift)
import           Control.Monad.State        (evalStateT)
import Control.Monad.Writer (tell)
import           Data.Bifunctor             ( first )
import qualified Data.Foldable              as Foldable
import           Data.Maybe                 (fromMaybe)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import Data.List (partition)
import Data.Set (Set)
import qualified Data.Set                   as Set
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text, unpack)
import qualified Unison.ABT                 as ABT
import qualified Unison.Blank               as Blank
import           Unison.DataDeclaration     (DataDeclaration',
                                             EffectDeclaration')
import qualified Unison.Name                as Name
import qualified Unison.Names               as Names
import           Unison.Parser              (Ann (Intrinsic))
import qualified Unison.Parsers             as Parsers
import qualified Unison.PrettyPrintEnv      as PPE
import           Unison.Reference           (Reference)
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
import qualified Unison.Util.List           as List
import           Unison.Var                 (Var)
import qualified Unison.Var                 as Var
-- import Debug.Trace

type Term v = AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type EffectDeclaration v = EffectDeclaration' v Ann
type UnisonFile v = UF.UnisonFile v Ann
type NamedReference v = Typechecker.NamedReference v Ann
type Result' v = Result (Seq (Note v Ann))
type Name = Text

convertNotes :: Ord v => Typechecker.Notes v ann -> Seq (Note v ann)
convertNotes (Typechecker.Notes es is) =
  (TypeError <$> es) <> (TypeInfo <$> Seq.fromList is') where
  is' = snd <$> List.uniqueBy' f ([(1::Word)..] `zip` Foldable.toList is)
  f (_, (Context.TopLevelComponent cs)) = Right [ v | (v,_,_) <- cs ]
  f (i, _) = Left i
  -- each round of TDNR emits its own TopLevelComponent notes, so we remove
  -- duplicates (based on var name and location), preferring the later note as
  -- that will have the latest typechecking info

parseAndSynthesizeFile
  :: (Var v, Monad m)
  => [Type v]
  -> (Set Reference -> m (TL.TypeLookup v Ann))
  -> Parser.ParsingEnv
  -> FilePath
  -> Text
  -> ResultT
       (Seq (Note v Ann))
       m
       (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile v Ann))
parseAndSynthesizeFile ambient typeLookupf names filePath src = do
  (errorEnv, parsedUnisonFile) <- Result.fromParsing
    $ Parsers.parseFile filePath (unpack src) names
  let refs = UF.dependencies parsedUnisonFile (snd names)
  typeLookup <- lift . lift $ typeLookupf refs
  let (Result notes' r) =
        synthesizeFile ambient typeLookup names parsedUnisonFile
  tell notes' *> pure (errorEnv, r)

synthesizeFile
  :: forall v
   . Var v
  => [Type v]
  -> TL.TypeLookup v Ann
  -> Parser.ParsingEnv
  -> UnisonFile v
  -> Result (Seq (Note v Ann)) (UF.TypecheckedUnisonFile v Ann)
synthesizeFile ambient preexistingTypes preexistingNames unisonFile = do
  let
    -- substitute builtins into the datas/effects/body of unisonFile
    uf@(UnisonFile dds0 eds0 _terms _watches) = unisonFile
    term0 = UF.typecheckingTerm uf
    localNames = UF.toNames uf
    localTypes = UF.declsToTypeLookup uf
    -- this is the preexisting terms and decls plus the local decls
    allTheNames = localNames <> snd preexistingNames
    ctorType r =
      fromMaybe
        (error $ "no constructor type in synthesizeFile for " <> show r)
        (TL.constructorType (localTypes <> preexistingTypes) r)
    term = Names.bindTerm ctorType allTheNames term0
    -- substitute Blanks for any remaining free vars in UF body
    tdnrTerm = Term.prepareTDNR $ term
    lookupTypes = localTypes <> preexistingTypes
    env0 = (Typechecker.Env Intrinsic ambient lookupTypes fqnsByShortName)
     where
      fqnsByShortName :: Map Name [Typechecker.NamedReference v Ann]
      fqnsByShortName = Map.fromListWith mappend
         [ (Names.unqualified' name,
            [Typechecker.NamedReference name typ (Right r)]) |
           (name', r) <- Map.toList $ Names.termNames allTheNames,
           let name = Name.toText name',
           typ <- Foldable.toList $ TL.typeOfReferent lookupTypes  r ]
    Result notes mayType =
      evalStateT (Typechecker.synthesizeAndResolve env0) tdnrTerm
  -- If typechecking succeeded, reapply the TDNR decisions to user's term:
  Result (convertNotes notes) mayType >>= \_typ -> do
    let infos = Foldable.toList $ Typechecker.infos notes
    (topLevelComponents :: [[(v, Term v, Type v)]]) <-
      let
        topLevelBindings :: Map v (Term v)
        topLevelBindings = Map.mapKeys Var.reset $ extractTopLevelBindings tdnrTerm
        extractTopLevelBindings (Term.LetRecNamedAnnotatedTop' True _ bs body) =
          Map.fromList (first snd <$> bs) <> extractTopLevelBindings body
        extractTopLevelBindings _                        = Map.empty
        tlcsFromTypechecker =
          List.uniqueBy' (fmap vars)
            [ t | Context.TopLevelComponent t <- infos ]
          where vars (v, _, _) = v
        strippedTopLevelBinding (v, typ, redundant) = do
          tm <- case Map.lookup v topLevelBindings of
            Nothing ->
              Result.compilerBug $ Result.TopLevelComponentNotFound v term
            Just (Term.Ann' x _) | redundant -> pure x
            Just x                           -> pure x
          -- The Var.reset removes any freshening added during typechecking
          pure (Var.reset v, tm, typ)
      in
        -- use tlcsFromTypechecker to inform annotation-stripping decisions
        traverse (traverse strippedTopLevelBinding) tlcsFromTypechecker
    let doTdnr = applyTdnrDecisions infos
        doTdnrInComponent (v, t, tp) = (\t -> (v, t, tp)) <$> doTdnr t
    _          <- doTdnr tdnrTerm
    tdnredTlcs <- (traverse . traverse) doTdnrInComponent topLevelComponents
    let (watches', terms') = partition isWatch tdnredTlcs
        isWatch = all (\(v,_,_) -> Set.member v watchedVars)
        watchedVars = Set.fromList [ v | (v, _) <- UF.allWatches uf ]
        tlcKind [] = error "empty TLC, should never occur"
        tlcKind tlc@((v,_,_):_) = let
          hasE k = any (== v) . fmap fst $ Map.findWithDefault [] k (UF.watches uf)
          in case Foldable.find hasE (Map.keys $ UF.watches uf) of
               Nothing -> error "wat"
               Just kind -> (kind, tlc)
    pure $ UF.TypecheckedUnisonFile dds0 eds0 terms' (map tlcKind watches')
 where
  applyTdnrDecisions
    :: [Context.InfoNote v Ann]
    -> Term v
    -> Result' v (Term v)
  applyTdnrDecisions infos tdnrTerm = foldM go tdnrTerm decisions
   where
    -- UF data/effect ctors + builtins + TLC Term.vars
    go term _decision@(shortv, loc, replacement) =
      ABT.visit (resolve shortv loc replacement) term
    decisions = [ (v, loc, replacement) | Context.Decision v loc replacement <- infos ]
    -- resolve (v,loc) in a matching Blank to whatever `fqn` maps to in `names`
    resolve shortv loc replacement t = case t of
      Term.Blank' (Blank.Recorded (Blank.Resolve loc' name))
        | loc' == loc && Var.nameStr shortv == name ->
          -- loc of replacement already chosen correctly by whatever made the Decision
          pure . pure $ replacement
      _ -> Nothing
