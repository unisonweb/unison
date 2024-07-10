module Unison.FileParsers
  ( ShouldUseTndr (..),
    computeTypecheckingEnvironment,
    synthesizeFile,
  )
where

import Control.Lens
import Control.Monad.State (evalStateT)
import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Blank qualified as Blank
import Unison.Builtin qualified as Builtin
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference (Reference)
import Unison.Referent qualified as Referent
import Unison.Result (CompilerBug (..), Note (..), ResultT, pattern Result)
import Unison.Result qualified as Result
import Unison.Syntax.Name qualified as Name (unsafeParseVar)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.Extractor (RedundantTypeAnnotation)
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnisonFile (definitionLocation)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.List qualified as List
import Unison.Util.Relation qualified as Rel
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind (WatchKind)

type Term v = Term.Term v Ann

type Type v = Type.Type v Ann

type UnisonFile v = UF.UnisonFile v Ann

-- each round of TDNR emits its own TopLevelComponent notes, so we remove
-- duplicates (based on var name and location), preferring the later note as
-- that will have the latest typechecking info
convertNotes :: (Ord v) => Typechecker.Notes v ann -> Seq (Note v ann)
convertNotes (Typechecker.Notes bugs es is) =
  (CompilerBug . TypecheckerBug <$> bugs) <> (TypeError <$> es) <> (TypeInfo <$> Seq.fromList is')
  where
    is' = snd <$> List.uniqueBy' f ([(1 :: Word) ..] `zip` Foldable.toList is)
    f (_, Context.TopLevelComponent cs) = Right [v | (v, _, _) <- cs]
    f (i, _) = Left i

-- | Should we use type-directed name resolution?
data ShouldUseTndr m
  = ShouldUseTndr'No
  | ShouldUseTndr'Yes (Parser.ParsingEnv m)

-- | Compute a typechecking environment, given:
--
--     * Whether or not to use type-directed name resolution during type checking.
--     * The abilities that are considered to already have ambient handlers.
--     * A function to compute a @TypeLookup@ for the given set of type- or term-references.
--     * The parsing environment that was used to parse the parsed Unison file.
--     * The parsed Unison file for which the typechecking environment is applicable.
computeTypecheckingEnvironment ::
  (Var v, Monad m) =>
  ShouldUseTndr m ->
  [Type v] ->
  (Set Reference -> m (TL.TypeLookup v Ann)) ->
  UnisonFile v ->
  m (Typechecker.Env v Ann)
computeTypecheckingEnvironment shouldUseTndr ambientAbilities typeLookupf uf =
  case shouldUseTndr of
    ShouldUseTndr'No -> do
      tl <- typeLookupf (UF.dependencies uf)
      pure
        Typechecker.Env
          { ambientAbilities = ambientAbilities,
            typeLookup = tl,
            termsByShortname = Map.empty
          }
    ShouldUseTndr'Yes parsingEnv -> do
      let preexistingNames = Parser.names parsingEnv
          tm = UF.typecheckingTerm uf
      -- possibleDeps =
      --   [ (name, shortname, r)
      --     | (name, r) <- Rel.toList (Names.terms preexistingNames),
      --       v <- Set.toList (Term.freeVars tm),
      --       let shortname = Name.unsafeParseVar v,
      --       name `Name.endsWithReverseSegments` List.NonEmpty.toList (Name.reverseSegments shortname)
      --   ]

      let possibleDeps = do
            v <- Set.toList (Term.freeVars tm)
            (name, r) <- Rel.toList (Names.terms preexistingNames)
            let shortname = Name.unsafeParseVar v
            guard $ name `Name.endsWithReverseSegments` List.NonEmpty.toList (Name.reverseSegments shortname)
            pure (name, shortname, r)
      let possibleRefs = Referent.toReference . view _3 <$> possibleDeps
      tl <- fmap (UF.declsToTypeLookup uf <>) (typeLookupf (UF.dependencies uf <> Set.fromList possibleRefs))
      -- For populating the TDNR environment, we pick definitions
      -- from the namespace and from the local file whose full name
      -- has a suffix that equals one of the free variables in the file.
      -- Example, the namespace has [foo.bar.baz, qux.quaffle] and
      -- the file has definitons [utils.zonk, utils.blah] and
      -- the file has free variables [bar.baz, zonk].
      --
      -- In this case, [foo.bar.baz, utils.zonk] are used to create
      -- the TDNR environment.
      let fqnsByShortName =
            List.multimap $
              -- external TDNR possibilities
              [ (shortname, nr)
                | (name, shortname, r) <- possibleDeps,
                  typ <- toList $ TL.typeOfReferent tl r,
                  let nr = Typechecker.NamedReference name typ (Context.ReplacementRef r)
              ]
                <>
                -- local file TDNR possibilities
                [ (shortname, nr)
                  | (name, r) <- Rel.toList (Names.terms $ UF.toNames uf),
                    v <- Set.toList (Term.freeVars tm),
                    let shortname = Name.unsafeParseVar v,
                    name `Name.endsWithReverseSegments` List.NonEmpty.toList (Name.reverseSegments shortname),
                    typ <- toList $ TL.typeOfReferent tl r,
                    let nr = Typechecker.NamedReference name typ (Context.ReplacementRef r)
                ]
      pure
        Typechecker.Env
          { ambientAbilities = ambientAbilities,
            typeLookup = tl,
            termsByShortname = fqnsByShortName
          }

synthesizeFile ::
  forall m v.
  (Monad m, Var v) =>
  Typechecker.Env v Ann ->
  UnisonFile v ->
  ResultT (Seq (Note v Ann)) m (UF.TypecheckedUnisonFile v Ann)
synthesizeFile env0 uf = do
  let term = UF.typecheckingTerm uf
      -- substitute Blanks for any remaining free vars in UF body
      tdnrTerm = Term.prepareTDNR term
      unisonFilePPE = PPE.makePPE (PPE.hqNamer 10 (Names.shadowing (UF.toNames uf) Builtin.names)) PPE.dontSuffixify
      Result notes mayType =
        evalStateT (Typechecker.synthesizeAndResolve unisonFilePPE env0) tdnrTerm
  -- If typechecking succeeded, reapply the TDNR decisions to user's term:
  Result.makeResult (convertNotes notes) mayType >>= \_typ -> do
    let infos = Foldable.toList $ Typechecker.infos notes
    (topLevelComponents :: [[(v, Term v, Type v)]]) <-
      let topLevelBindings :: Map v (Term v)
          topLevelBindings = Map.mapKeys Var.reset $ extractTopLevelBindings tdnrTerm
          extractTopLevelBindings :: (Term.Term v a -> Map v (Term.Term v a))
          extractTopLevelBindings (Term.LetRecNamedAnnotatedTop' True _ bs body) =
            Map.fromList (first snd <$> bs) <> extractTopLevelBindings body
          extractTopLevelBindings _ = Map.empty
          tlcsFromTypechecker :: [[(v, Type.Type v Ann, RedundantTypeAnnotation)]]
          tlcsFromTypechecker =
            List.uniqueBy'
              (fmap vars)
              [t | Context.TopLevelComponent t <- infos]
            where
              vars (v, _, _) = v
          addTypesToTopLevelBindings :: (v, c, c1) -> ResultT (Seq (Note v Ann)) m (v, Term v, c)
          addTypesToTopLevelBindings (v, typ, _redundant) = do
            tm <- case Map.lookup v topLevelBindings of
              Nothing -> Result.compilerBug $ Result.TopLevelComponentNotFound v term
              Just x -> pure x
            -- The Var.reset removes any freshening added during typechecking
            pure (Var.reset v, tm, typ)
       in traverse (traverse addTypesToTopLevelBindings) tlcsFromTypechecker
    let doTdnr = applyTdnrDecisions infos
    let doTdnrInComponent (v, t, tp) = (v, doTdnr t, tp)
    let tdnredTlcs =
          topLevelComponents
            & (fmap . fmap)
              ( \vtt ->
                  vtt
                    & doTdnrInComponent
                    & \(v, t, tp) -> (v, fromMaybe (error $ "Symbol from typechecked file not present in parsed file" <> show v) (definitionLocation v uf), t, tp)
              )
    let (watches', terms') = partition isWatch tdnredTlcs
        isWatch = all (\(v, _, _, _) -> Set.member v watchedVars)
        watchedVars = Set.fromList [v | (v, _a, _) <- UF.allWatches uf]
        tlcKind [] = error "empty TLC, should never occur"
        tlcKind tlc@((v, _, _, _) : _) =
          let hasE :: WatchKind -> Bool
              hasE k = elem v . fmap (view _1) $ Map.findWithDefault [] k (UF.watches uf)
           in case Foldable.find hasE (Map.keys $ UF.watches uf) of
                Nothing -> error "wat"
                Just kind -> (kind, tlc)
    pure $
      UF.typecheckedUnisonFile
        (UF.dataDeclarationsId uf)
        (UF.effectDeclarationsId uf)
        terms'
        (map tlcKind watches')
  where
    applyTdnrDecisions ::
      [Context.InfoNote v Ann] ->
      Term v ->
      Term v
    applyTdnrDecisions infos tdnrTerm = ABT.visitPure resolve tdnrTerm
      where
        decisions = Map.fromList [((Var.nameStr v, loc), replacement) | Context.Decision v loc replacement <- infos]
        -- resolve (v,loc) in a matching Blank to whatever `fqn` maps to in `names`
        resolve t = case t of
          Term.Blank' (Blank.Recorded (Blank.Resolve loc' name))
            | Just replacement <- Map.lookup (name, loc') decisions ->
                -- loc of replacement already chosen correctly by whatever made the
                -- Decision
                Just $ replacement
          _ -> Nothing
