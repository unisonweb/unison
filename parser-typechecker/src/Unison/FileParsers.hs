module Unison.FileParsers
  ( ShouldUseTndr (..),
    computeTypecheckingEnvironment,
    synthesizeFile,
    givenDeclVars,
  )
where

import Control.Lens
import Control.Monad.Fail qualified as Fail
import Control.Monad.State (evalStateT)
import Data.Foldable qualified as Foldable
import Data.List (partition)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Ord (clamp)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Blank qualified as Blank
import Unison.Builtin qualified as Builtin
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.Names.ResolvesTo (ResolvesTo (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result (CompilerBug (..), Note (..), ResultT, pattern Result)
import Unison.Result qualified as Result
import Unison.Syntax.Name qualified as Name (parseText, toText, unsafeParseText, unsafeParseVar)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.Extractor (RedundantTypeAnnotation)
import Unison.Typechecker.GivenApply qualified as GivenApply
import Unison.Typechecker.GivenElaborator qualified as GivenElaborator
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.Variance qualified as Variance
import Unison.UnisonFile (definitionLocation)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.List qualified as List
import Unison.Util.Map qualified as Map (upsert)
import Unison.Util.Relation (Relation)
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
  forall m v.
  (Var v, Monad m) =>
  ShouldUseTndr m ->
  [Type v] ->
  (DefnsF Set TermReference TypeReference -> m (TL.TypeLookup v Ann)) ->
  -- | Ambient givens harvested from the namespace. Each entry is a
  -- top-level term tagged via 'Unison.Codebase.Givens.givenSentinel'
  -- together with its declared type; 'ambientPool' decomposes the
  -- type into the resolver's @(tyVars, premises, conclusion)@ shape.
  -- Pass @[]@ when there is no namespace context (e.g. a fresh test
  -- environment).
  [GivenElaborator.AmbientGiven v Ann] ->
  UnisonFile v ->
  m (Typechecker.Env v Ann)
computeTypecheckingEnvironment shouldUseTndr ambientAbilities typeLookupf ambientGivens0 uf =
  -- Names of @given@ bindings declared in this scratch file. Used to
  -- drop codebase ambient givens of the same name so the file's
  -- version shadows the codebase's during this typecheck, avoiding a
  -- spurious "ambiguous" diagnostic when the user is effectively
  -- updating an existing given in place.
  let ambientGivens =
        let fileGivenNames :: Set Name
            fileGivenNames =
              Set.fromList
                [ name
                | v <- Set.toList (UF.givenBindings uf),
                  Just name <- [Name.parseText (Var.name (Var.reset v))]
                ]
         in GivenElaborator.filterAmbientByName fileGivenNames ambientGivens0
   in case shouldUseTndr of
    ShouldUseTndr'No -> do
      tl <- typeLookupf (UF.dependencies uf)
      pure
        Typechecker.Env
          { ambientAbilities = ambientAbilities,
            typeLookup = tl,
            termsByShortname = Map.empty,
            freeNameToFuzzyTermsByShortName = Map.empty,
            topLevelComponents = Map.empty,
            variances = Variance.fromTypeLookup tl,
            ambientGivens = GivenElaborator.ambientPool ambientGivens,
            -- Thread the parser-collected @given@-bound variable
            -- names through to the typechecker.
            givenBindings = uf.givenBindings
          }
    ShouldUseTndr'Yes parsingEnv -> do
      let resolveName :: Name -> Relation Name (ResolvesTo Referent)
          resolveName =
            Names.resolveNameIncludingNames
              (Names.shadowing1 (Names.terms (UF.toNames uf)) (Names.terms (Parser.names parsingEnv)))
              localNames

          localNames = Set.map Name.unsafeParseVar (UF.toTermAndWatchNames uf)
          -- We exclude names from indirect dependencies for fuzzy searching during name resolution,
          -- that is dependencies under lib.*.lib for performance
          -- TODO: We may consider exposing user configuration to enable searching through indirect dependencies
          globalNamesShadowed = excludeNamesFromIndirectDeps $ Names.shadowing (UF.toNames uf) (Parser.names parsingEnv)
            where
              excludeNamesFromIndirectDeps = Names.filter (Name.classifyNameLocation >>> excludeIndirectDeps)
              excludeIndirectDeps = (\case Name.NameLocation'IndirectDep -> False; _otherwise -> True)

          freeNames :: [Name]
          freeNames =
            Name.unsafeParseVar <$> Set.toList (Term.freeVars $ UF.typecheckingTerm uf)

          possibleDepsExact :: [(Name, Name, ResolvesTo Referent)]
          possibleDepsExact = do
            freeName <- freeNames
            (name, ref) <- Rel.toList (resolveName freeName)
            [(name, freeName, ref)]

          getFreeNameDepsFuzzy :: Name -> [(Name, Name, ResolvesTo Referent)]
          getFreeNameDepsFuzzy freeName = do
            let wantedTopNFuzzyMatches = 3
            -- We use fuzzy matching by edit distance here because it is usually more appropriate
            -- than FZF-style fuzzy finding for offering suggestions for typos or other user errors.
            let fuzzyMatches =
                  take wantedTopNFuzzyMatches $
                    fuzzyFindByEditDistanceRanked globalNamesShadowed localNames freeName

            let names = fuzzyMatches ^.. each . _2
            let resolvedNames = Rel.toList . resolveName =<< names
            let getShortName longname = Name.unsafeParseText (NameSegment.toUnescapedText $ Name.lastSegment longname)

            map (\(longname, ref) -> (longname, getShortName longname, ref)) resolvedNames

          freeNameDepsFuzzy :: Map Name [(Name, Name, ResolvesTo Referent)]
          freeNameDepsFuzzy =
            Map.fromList [(freeName, getFreeNameDepsFuzzy freeName) | freeName <- freeNames]

          getPossibleRefs :: [(Name, Name, ResolvesTo Referent)] -> Defns (Set TermReference) (Set TypeReference)
          getPossibleRefs =
            List.foldl'
              ( \acc -> \case
                  (_, _, ResolvesToNamespace ref0) ->
                    case ref0 of
                      Referent.Con ref _ -> acc & over #types (Set.insert (ref ^. ConstructorReference.reference_))
                      Referent.Ref ref -> acc & over #terms (Set.insert ref)
                  (_, _, ResolvesToLocal _) -> acc
              )
              (Defns Set.empty Set.empty)

      typeLookup <-
        fmap
          (UF.declsToTypeLookup uf <>)
          ( typeLookupf
              ( UF.dependencies uf
                  <> getPossibleRefs possibleDepsExact
                  <> getPossibleRefs (join $ Map.elems freeNameDepsFuzzy)
              )
          )

      let getTermsByShortname :: [(Name, Name, ResolvesTo Referent)] -> Map Name [Either Name (Typechecker.NamedReference v Ann)]
          getTermsByShortname =
            List.foldl'
              ( \acc -> \case
                  (name, shortname, ResolvesToLocal _) -> let v = Left name in Map.upsert (maybe [v] (v :)) shortname acc
                  (name, shortname, ResolvesToNamespace ref) ->
                    case TL.typeOfReferent typeLookup ref of
                      Just ty ->
                        let v = Right (Typechecker.NamedReference name ty (Context.ReplacementRef ref))
                         in Map.upsert (maybe [v] (v :)) shortname acc
                      Nothing -> acc
              )
              Map.empty

      let termsByShortname = getTermsByShortname possibleDepsExact
      let freeNameToFuzzyTermsByShortName = Map.mapWithKey (\_ v -> getTermsByShortname v) freeNameDepsFuzzy

      pure
        Typechecker.Env
          { ambientAbilities,
            typeLookup,
            termsByShortname,
            freeNameToFuzzyTermsByShortName,
            topLevelComponents = Map.empty,
            variances = Variance.fromTypeLookup typeLookup,
            ambientGivens = GivenElaborator.ambientPool ambientGivens,
            -- Thread the parser-collected @given@-bound variable
            -- names through to the typechecker.
            givenBindings = uf.givenBindings
          }

-- | 'fuzzyFindByEditDistanceRanked' finds matches for the given 'name' within 'names' by edit distance.
--
-- Returns a list of 3-tuples composed of an edit-distance Score, a Name, and a List of term and type references.
--
-- Adapted from Unison.Server.Backend.fuzzyFind
--
-- TODO: Consider moving to Unison.Names
--
-- TODO: Take type similarity into account when ranking matches
fuzzyFindByEditDistanceRanked ::
  Names.Names ->
  Set Name ->
  Name ->
  [(Int, Name)]
fuzzyFindByEditDistanceRanked globalNames localNames name =
  let query =
        (Text.unpack . nameToText) name

      -- Use 'nameToTextFromLastNSegments' so edit distance is not biased towards shorter fully-qualified names
      -- and the name being queried is only partially qualified.
      fzfGlobalNames =
        Names.queryEditDistances nameToTextFromLastNSegments query globalNames
      fzfLocalNames =
        Names.queryEditDistances' nameToTextFromLastNSegments query localNames
      fzfNames = fzfGlobalNames ++ fzfLocalNames

      -- Keep only matches with a sufficiently low edit-distance score
      filterByScore = filter (\(score, _, _) -> score < maxScore)

      -- Prefer lower edit distances and then prefer shorter names by segment count
      rank (score, name, _) = (score, length $ Name.segments name)

      -- Remove dupes based on refs
      dedupe =
        List.nubOrdOn (\(_, _, refs) -> refs)

      dropRef = map (\(x, y, _) -> (x, y))

      refine =
        dropRef . dedupe . sortOn rank . filterByScore
   in refine fzfNames
  where
    nNameSegments = max 1 $ NonEmpty.length $ Name.segments name

    takeLast :: Int -> NonEmpty.NonEmpty a -> [a]
    takeLast n xs = NonEmpty.drop (NonEmpty.length xs - n) xs
    nameFromLastNSegments =
      Name.fromSegments
        . NonEmpty.fromList
        . takeLast nNameSegments
        . Name.segments

    -- Convert to lowercase for case-insensitive fuzzy matching
    nameToText = Text.toLower . Name.toText
    nameToTextFromLastNSegments = nameToText . nameFromLastNSegments

    ceilingDiv :: Int -> Int -> Int
    ceilingDiv x y = (x + 1) `div` y
    -- Expect edit distances (number of typos) to be about half the length of the name being queried
    -- But clamp max edit distance to work well with very short names
    -- and keep ranking reasonably fast when a verbose name is queried
    maxScore = clamp (3, 16) $ Text.length (nameToText name) `ceilingDiv` 2

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
    -- Drive given-resolution from the 'ConstraintGoal' info notes the
    -- typechecker emitted, then walk each top-level term and
    -- substitute resolution trees into 'App' nodes. The ambient pool
    -- comes from 'env0.ambientGivens', built by
    -- 'computeTypecheckingEnvironment' from the namespace's
    -- 'given'-tagged definitions.
    --
    -- We additionally promote every file-local @given@-tagged
    -- top-level binding into the ambient pool. The per-binding
    -- 'extendLexicalGivenFromBinding' machinery only makes a given
    -- visible to siblings that 'minimize' happened to order /after/
    -- it in the resulting let-chain; siblings that come earlier in
    -- the chain (because 'minimize' reordered independent SCCs) have
    -- a 'ConstraintGoal' whose 'pgScope' snapshot is missing the
    -- given. Top-level givens are conceptually file-global, so we
    -- expose them to every goal via the ambient pool, sidestepping
    -- minimize's ordering entirely.
    let fileLocalGivens :: [GivenElaborator.AmbientGiven v Ann]
        fileLocalGivens =
          [ GivenElaborator.AmbientGiven
              { GivenElaborator.ambientName =
                  Reference.Builtin ("Local.given." <> Var.name (Var.reset v)),
                GivenElaborator.ambientType = t,
                -- File-local givens carry their own name; we don't
                -- need it for shadowing (file givens shadow codebase
                -- givens, not the other way around).
                GivenElaborator.ambientUserName = Nothing
              }
          | tlc <- topLevelComponents,
            (v, _, t) <- tlc,
            Set.member (Var.reset v) (UF.givenBindings uf)
          ]
        ambient =
          GivenElaborator.mergePool
            (GivenElaborator.ambientPool fileLocalGivens)
            (Typechecker.ambientGivens env0)
    let infosWithImplicits = GivenElaborator.elaborateInfoNotes ambient infos
    -- Surface implicit-resolution failures as user diagnostics. Each
    -- 'SolvedImplicit' note whose decision is 'Left _' becomes a
    -- 'Result.UnresolvedImplicit' note. We tell them all and fail the
    -- result if any were emitted, so the compiler does not silently
    -- proceed with an under-applied term.
    let implicitFailures =
          [ Result.UnresolvedImplicit loc ty err
          | (loc, ty, Left err) <- GivenElaborator.implicitDecisions infosWithImplicits
          ]
    Result.tellNotes (Seq.fromList implicitFailures)
    when (not (null implicitFailures)) (Fail.fail "implicit resolution failed")
    -- Surface 'SolvedImplicit' info notes as 'TypeInfo' so downstream
    -- consumers (LSP, tests, transcripts) can observe which given was
    -- chosen for each apply-site. The 'GivenElaborator' adds these as
    -- plain 'Context.InfoNote's appended to the typechecker's stream;
    -- they need an explicit 'tell' here to become part of the file's
    -- 'Result' notes.
    let solvedImplicitNotes =
          [ note'
          | note'@(Context.SolvedImplicit {}) <-
              drop (length infos) infosWithImplicits
          ]
    for_ solvedImplicitNotes (Result.tell1 . Result.TypeInfo)
    let doTdnr = applyTdnrDecisions infos
    -- Apply TDNR first, then implicit-dictionary insertion. The
    -- implicit insertion runs over /all/ bindings as a single pass
    -- so the decision queue stays aligned with the typechecker's
    -- emission order — per-binding fresh queues caused bindings to
    -- pop decisions intended for a sibling and inject the sibling's
    -- locally-bound dictionary into a scope where it isn't bound.
    let tdnredTerms :: [[(v, Term v, Type v)]]
        tdnredTerms =
          (fmap . fmap) (\(v, t, tp) -> (v, doTdnr t, tp)) topLevelComponents
        flatBindings :: [(Int, (v, Term v, Type v))]
        flatBindings = zip [0 ..] (concat tdnredTerms)
        (rewrittenTerms, implicitNotesAll) =
          GivenApply.applyGivenDecisionsAll
            infosWithImplicits
            (Typechecker.typeLookup env0)
            (map (\(_, (_, t, _)) -> t) flatBindings)
        idxToRewritten :: Map Int (Term v)
        idxToRewritten = Map.fromList (zip (map fst flatBindings) rewrittenTerms)
    let tdnredTlcs :: [[(v, Ann, Term v, Type v)]]
        tdnredTlcs =
          let go i (v, _t, tp) =
                let t' = idxToRewritten Map.! i
                    a = fromMaybe (error $ "Symbol from typechecked file not present in parsed file" <> show v) (definitionLocation v uf)
                 in (v, a, t', tp)
              indexed :: [[(Int, (v, Term v, Type v))]]
              indexed = snd $ List.mapAccumL stepComp 0 tdnredTerms
              stepComp i comp =
                let n = length comp
                 in (i + n, zip [i .. i + n - 1] comp)
           in (fmap . fmap) (\(i, vtt) -> go i vtt) indexed
    -- Emit the 'ImplicitArgRef' notes as 'TypeInfo' so that
    -- 'FileAnalysis' (and any other 'Note'-consumer) can pick them up.
    for_ implicitNotesAll (Result.tell1 . Result.TypeInfo)
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
        uf.givenBindings
        uf.classBindings
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

-- | Extract the @given@-declaration metadata from the typechecker's
-- info-notes stream.
--
-- A 'Context.GivenDecl' note is emitted for every top-level binding
-- whose declared type begins (after stripping any leading 'Forall')
-- with one or more @=>@ constraint parameters. This helper projects
-- those notes into a @Map v (declaredType, conclusion)@, with
-- 'Var.reset' applied so callers see the user-written variable name
-- (un-freshened).
--
-- Consumers: the UCM command surface, which marks each named
-- definition as a given via 'Unison.Codebase.Givens.markGivenAt'
-- after the file's hashes are computed; the elaborator, which
-- substitutes resolved dictionary terms into apply sites.
--
-- The namespace is the instance set; this helper does not itself
-- touch any namespace metadata. It only provides the data the caller
-- needs to do the marking.
givenDeclVars ::
  (Var v) =>
  [Context.InfoNote v Ann] ->
  Map v (Type v, Type v)
givenDeclVars infos =
  -- The typechecker's 'GivenDecl' note carries 'Context.Type'
  -- (TypeVar-tagged), so lower each type back to surface form before
  -- handing the map to consumers — they will compare it against
  -- 'Type.Type v Ann'-shaped values from the parser / namespace.
  -- 'generalizeAndUnTypeVar' is the same lowering 'TopLevelComponent'
  -- uses for the publicly-exposed type field.
  Map.fromList
    [ ( Var.reset v,
        ( Context.generalizeAndUnTypeVar declared,
          Context.generalizeAndUnTypeVar conclusion
        )
      )
    | Context.GivenDecl _ v declared conclusion <- infos
    ]
