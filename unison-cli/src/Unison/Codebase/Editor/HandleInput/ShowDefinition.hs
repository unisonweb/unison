module Unison.Codebase.Editor.HandleInput.ShowDefinition
  ( handleShowDefinition,
    showDefinitions,
    renderToFile,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State qualified as State
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as Builtin
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Classes qualified as Classes
import Unison.Codebase.Editor.DisplayObject (DisplayObject (UserObject))
import Unison.Codebase.Editor.DisplayObject qualified as DisplayObject
import Unison.Codebase.Editor.Input (OutputLocation (..), RelativeToFold (..), ShowDefinitionScope (..))
import Unison.Codebase.Editor.Output
import Unison.Codebase.Givens qualified as Givens
import Unison.DataDeclaration (Decl)
import Unison.HashQualified qualified as HQ
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (Intrinsic))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference, TermReferenceId, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Backend qualified as Backend
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toVar)
import Unison.Syntax.NamePrinter (SyntaxText)
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Typechecker.GivenApply qualified as GivenApply
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.Defns (Defns (..))
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Set qualified as Set
import Unison.WatchKind qualified as WatchKind

-- | Handle a @ShowDefinitionI@ input command, i.e. `view` or `edit`.
handleShowDefinition :: OutputLocation -> ShowDefinitionScope -> List.NonEmpty (HQ.HashQualified Name) -> Cli ()
handleShowDefinition outputLoc showDefinitionScope originalQuery = do
  env <- ask

  let originalQuerySet :: Set (HQ.HashQualified Name)
      originalQuerySet =
        Set.fromList (List.NonEmpty.toList originalQuery)

  -- Take the user's original query, de-dupe (unlikely that they repeated something), and maybe add docs per above.
  let query :: Set (HQ.HashQualified Name)
      query =
        Foldable.foldl'
          ( \acc hqName ->
              acc
                & Set.insert hqName
                & case hqName of
                  HQ.NameOnly name
                    | Name.lastSegment name /= NameSegment.docSegment ->
                        Set.insert (HQ.NameOnly (Name.snoc name NameSegment.docSegment))
                  _ -> id
          )
          Set.empty
          originalQuery

  let hasAbsoluteQuery = any (any Name.isAbsolute) query
  (names, unbiasedPPED) <- case (hasAbsoluteQuery, showDefinitionScope) of
    -- TODO: We should instead print each definition using the names from its project-branch root.
    (True, _) -> do
      root <- Cli.getCurrentProjectRoot
      let root0 = Branch.head root
      let names = Names.makeAbsolute (Branch.toNames root0)
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
      pure (names, pped)
    (_, ShowDefinitionGlobal) -> do
      -- TODO: Maybe rewrite to be properly global
      root <- Cli.getCurrentProjectRoot
      let root0 = Branch.head root
      let names = Names.makeAbsolute $ Branch.toNames root0
      let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
      pure (names, pped)
    (_, ShowDefinitionLocal) -> do
      currentNames <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 currentNames) (suffixify currentNames)
      pure (currentNames, pped)
  let pped = PPED.biasTo (mapMaybe HQ.toName (Set.toList query)) unbiasedPPED
  Backend.DefinitionResults terms types misses0 <- do
    let nameSearch = NameSearch.makeNameSearch 10 names
    Cli.runTransaction $
      Backend.definitionsByName
        env.codebase
        nameSearch
        includeCycles
        Names.IncludeSuffixes
        query
  -- Removed missed docs that the user didn't ask for from `misses`
  let misses =
        -- Unlikely that both original query list and misses list are both very long, but make a set out of original
        -- query anyway, to replace pathological O(n^2) with O(n log n)
        filter (`Set.member` originalQuerySet) misses0
  -- Compute the set of term references in the result that are tagged
  -- as givens in the current namespace. The renderer will prefix
  -- these with the `given` marker.
  currentBranch0 <- Cli.getCurrentBranch0
  -- 'Givens.isGiven' only inspects the supplied branch's own
  -- metadata; given-marked terms living in sub-namespaces (the
  -- common case for class accessors and dot-qualified givens like
  -- @Monoid.nat@) wouldn't be detected. Walk the whole tree once
  -- and collect every given-marked referent.
  let givenReferents :: Set Referent =
        let go b acc =
              let here = Set.filter (\r -> Givens.isGiven r b) (Branch.deepReferents b)
                  acc' = acc <> here
                  children = b ^. Branch.children_
               in foldr (\child a -> go (Branch.head child) a) acc' (Map.elems children)
         in go currentBranch0 Set.empty
      isGivenRef :: TermReference -> Bool
      isGivenRef r = Set.member (Referent.Ref r) givenReferents
      -- Walk the whole tree to collect every class-marked type, so
      -- the printer renders them with the @class@ keyword and
      -- record-style field syntax.
      classTypeRefs :: Set TypeReference =
        let go b acc =
              let here = Set.filter (\r -> Classes.isClass r b) (Branch.deepTypeReferences b)
                  acc' = acc <> here
                  children = b ^. Branch.children_
               in foldr (\child a -> go (Branch.head child) a) acc' (Map.elems children)
         in go currentBranch0 Set.empty
      isClassRef :: TypeReference -> Bool
      isClassRef r = Set.member r classTypeRefs
  showDefinitions outputLoc (`Set.member` originalQuerySet) isGivenRef isClassRef pped terms types misses
  where
    suffixify =
      case outputLoc of
        ConsoleLocation -> PPE.suffixifyByHash
        FileLocation _ _ -> PPE.suffixifyByHashName
        LatestFileLocation _ -> PPE.suffixifyByHashName

    -- `view`: don't include cycles; `edit`: include cycles
    includeCycles =
      case outputLoc of
        ConsoleLocation -> Backend.DontIncludeCycles
        FileLocation _ _ -> Backend.IncludeCycles
        LatestFileLocation _ -> Backend.IncludeCycles

-- | Show the provided definitions to console or scratch file.
-- The caller is responsible for ensuring that the definitions include cycles if that's
-- the desired behavior.
showDefinitions ::
  OutputLocation ->
  (HQ.HashQualified Name -> Bool) ->
  -- | Predicate: is this term reference tagged as a given? Tagged
  -- terms are rendered with a leading @given@ marker so the output
  -- round-trips with the parser-side @given@ keyword.
  (TermReference -> Bool) ->
  -- | Predicate: is this type tagged as a class? Class-tagged types
  -- render with the @class@ keyword and record-style field syntax.
  (TypeReference -> Bool) ->
  PPED.PrettyPrintEnvDecl ->
  Map TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann)) ->
  Map TypeReference (DisplayObject () (Decl Symbol Ann)) ->
  [HQ.HashQualified Name] ->
  Cli ()
showDefinitions outputLoc nameInOriginalQuery isGivenRef isClassRef pped terms0 types misses = do
  Cli.Env {codebase, writeSource} <- ask
  -- Default elide-mode: pre-process every term being shown by
  -- dropping apply-site arguments that fill leading @=>@ slots in
  -- the function's declared type. The type lookup goes through the
  -- codebase (terms loaded by 'view' have their source annotations
  -- stripped, so the in-memory 'Ann.Synthetic' marker is no longer
  -- present; the only reliable signal is the function's declared
  -- type).
  -- Term references whose types we need to look up to strip
  -- implicit-fill arguments: every direct reference inside a body
  -- (so we know which apply-site args are implicit) /and/ every
  -- binding's own reference (so we can strip the parser-injected
  -- leading lambda for the binding's @=>@ parameters).
  let refsInTerms :: Set TermReference
      refsInTerms =
        Map.foldlWithKey'
          ( \acc bindingRef dt -> case dt of
              UserObject tm ->
                let deps :: Set LD.LabeledDependency
                    deps = Term.labeledDependencies tm
                    trs =
                      Set.fromList
                        [ r | LD.TermReference r <- Set.toList deps
                        ]
                 in acc <> Set.insert bindingRef trs
              _ -> Set.insert bindingRef acc
          )
          Set.empty
          terms0
  typesByRef <-
    Cli.runTransaction $
      Foldable.foldlM
        ( \acc r -> case r of
            Reference.DerivedId rid ->
              Codebase.getTypeOfTerm codebase r >>= \case
                Just ty -> pure (Map.insert (Reference.DerivedId rid) ty acc)
                Nothing -> pure acc
            Reference.Builtin _ ->
              -- The @summon@ builtin (and any future builtin with a
              -- @=>@ in its declared type) needs to be known to
              -- 'stripImplicitArgsByType' so the elaborator-filled
              -- dictionary argument can be elided from @view@ output.
              -- 'Builtin.termRefTypes' carries unit annotations;
              -- promote them to 'Intrinsic' so they fit the
              -- @Ann@-typed map.
              case Map.lookup r Builtin.termRefTypes of
                Just ty -> pure (Map.insert r ((const Intrinsic) <$> ty) acc)
                Nothing -> pure acc
        )
        Map.empty
        (Set.toList refsInTerms)
  let lookupTermType :: Term Symbol Ann -> Maybe (Type Symbol Ann)
      lookupTermType t = case ABT.out t of
        ABT.Tm (Term.Ref r) -> Map.lookup r typesByRef
        ABT.Tm (Term.Ann _ ty) -> Just ty
        _ -> Nothing
      stripTerm :: Term Symbol Ann -> Term Symbol Ann
      stripTerm = GivenApply.stripImplicitArgsByType isGivenRef lookupTermType
      -- A top-level binding whose declared type starts with
      -- @=>@ has a parser-injected @\\_implicit_<name>_<i> -> …@
      -- prefix in its body. Strip the corresponding leading lambdas
      -- before rendering so the surface form matches what the user
      -- wrote.
      terms :: Map TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann))
      terms =
        Map.mapWithKey
          ( \ref dt -> case dt of
              UserObject tm ->
                let tm' = stripTerm tm
                    tm'' = case Map.lookup ref typesByRef of
                      Just ty -> GivenApply.stripLeadingImplicitLambdas ty tm'
                      Nothing -> tm'
                 in UserObject tm''
              other -> other
          )
          terms0
  outputPath <- getOutputPath
  case outputPath of
    _ | null terms && null types -> pure ()
    Nothing -> renderToConsole nameInOriginalQuery isGivenRef isClassRef pped terms types
    Just (fp, relToFold) -> do
      mayTF <- use #latestTypecheckedFile
      numRendered <- renderToFile codebase nameInOriginalQuery isGivenRef isClassRef writeSource mayTF fp relToFold pped terms types

      when (numRendered > 0) do
        -- We set latestFile to be programmatically generated, if we
        -- are viewing these definitions to a file - this will skip the
        -- next update for that file (which will happen immediately)
        #latestFile ?= (fp, True)
      Cli.respond $ LoadedDefinitionsToSourceFile fp numRendered

  when (not (null misses)) (Cli.respond (SearchTermsNotFound misses))
  where
    -- Get the file path to send the definition(s) to. `Nothing` means the terminal.
    getOutputPath :: Cli (Maybe (FilePath, RelativeToFold))
    getOutputPath =
      case outputLoc of
        ConsoleLocation -> pure Nothing
        FileLocation path relToFold -> pure (Just (path, relToFold))
        LatestFileLocation relToFold -> do
          loopState <- State.get
          pure case loopState ^. #latestFile of
            Nothing -> Just ("scratch.u", relToFold)
            Just (path, _) -> Just (path, relToFold)

renderCodePretty ::
  (HQ.HashQualified Name -> Bool) ->
  -- | Predicate: is this term reference a given? See 'showDefinitions'.
  (TermReference -> Bool) ->
  -- | Predicate: is this type a class? See 'showDefinitions'.
  (TypeReference -> Bool) ->
  PPED.PrettyPrintEnvDecl ->
  Bool ->
  (TermReferenceId -> Bool) ->
  Map TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann)) ->
  Map TypeReference (DisplayObject () (Decl Symbol Ann)) ->
  Defns (Set Symbol) (Set Symbol) ->
  -- Result is Nothing if nothing was rendered
  Maybe (Pretty Pretty.ColorText, Int)
renderCodePretty nameInOriginalQuery isGivenRef isClassRef pped isSourceFile isTest terms types excludeNames =
  let -- Associate each term and type with their best unsuffixified name
      namedTerms :: Map (HQ.HashQualified Name) (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann))
      namedTerms =
        nameTerms pped.unsuffixifiedPPE excludeNames.terms terms

      namedTypes :: Map (HQ.HashQualified Name) (TypeReference, DisplayObject () (Decl Symbol Ann))
      namedTypes =
        nameTypes pped.unsuffixifiedPPE excludeNames.types types

      -- Partition those into two groups: those that end in a .doc segment, and those that don't
      -- Note that the doc-named terms aren't necessarily docs (though they they likely all are)
      docNamedTerms :: Map (HQ.HashQualified Name) (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann))
      notDocNamedTerms :: Map (HQ.HashQualified Name) (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann))
      (docNamedTerms, notDocNamedTerms) =
        Map.partitionWithKey
          ( \hqName _ ->
              case hqName of
                HQ.NameOnly name -> Name.lastSegment name == NameSegment.docSegment
                _ -> False
          )
          namedTerms

      -- Define a helper that resolves a name like `foo.bar` to its rendered doc at `foo.bar.doc` (if there is one)
      lookupDocForName :: HQ.HashQualified Name -> Maybe (Pretty SyntaxText)
      lookupDocForName hqName = do
        name <- HQ.asNameOnly hqName
        (_, DisplayObject.UserObject docTerm) <-
          Map.lookup (HQ.NameOnly (Name.snoc name NameSegment.docSegment)) docNamedTerms
        TermPrinter.prettyDoc2 pped.suffixifiedPPE docTerm

      -- For each of the not-doc terms, e.g. foo.bar, pair with its doc, i.e. foo.bar.doc (if it's there)
      termsWithMaybeDocs ::
        Map
          (HQ.HashQualified Name)
          ( (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)),
            Maybe (Pretty SyntaxText)
          )
      termsWithMaybeDocs =
        notDocNamedTerms & Map.mapWithKey \name term ->
          (term, lookupDocForName name)

      -- Same for types - pair with their docs as well
      typesWithMaybeDocs ::
        Map
          (HQ.HashQualified Name)
          ( (TypeReference, DisplayObject () (Decl Symbol Ann)),
            Maybe (Pretty SyntaxText)
          )
      typesWithMaybeDocs =
        namedTypes & Map.mapWithKey \name typ ->
          (typ, lookupDocForName name)

      -- Now we can identify all of the doc-named things that didn't get paired up with a type or term. Very commonly,
      -- these will be due to the user simply having asked for the doc of something but not its term, e.g.
      -- `edit foo.doc`. We might also have doc-named things that just aren't docs.
      docNamedTermsWithNoAssociatedDefinition ::
        Map (HQ.HashQualified Name) (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann))
      docNamedTermsWithNoAssociatedDefinition =
        let namesOfDocsAssociatedWithDefns :: Map (HQ.HashQualified Name) (defn, Maybe doc) -> Set (HQ.HashQualified Name)
            namesOfDocsAssociatedWithDefns =
              Map.foldlWithKey'
                ( \acc name -> \case
                    (_, Just _) -> Set.insert ((`Name.snoc` NameSegment.docSegment) <$> name) acc
                    _ -> acc
                )
                Set.empty
         in Map.withoutKeys
              docNamedTerms
              ( Set.union
                  (namesOfDocsAssociatedWithDefns termsWithMaybeDocs)
                  (namesOfDocsAssociatedWithDefns typesWithMaybeDocs)
              )

      -- And now we can add those docs back into `termsWithTheirDocs`, themselves without docs of course
      --
      -- Here's also where we end up using our magic "should include this" predicate from the sky. Here's the
      -- motivation, by example:
      --
      -- 1. The user has `lib.base.List.sort` and `List.sort.doc` (you might think that's nuts but we have a transcript
      --    that does this!)
      -- 2. The user types `view List.sort`
      -- 3. This gets auto-expanded to `view List.sort List.sort.doc`
      -- 3. We therefore find `base.List.sort` and `List.sort.doc`
      --
      -- In this case, we have a doc with no associated definition (`List.sort.doc`), but the user didn't ask for it.
      -- So, we probably shouldn't show it. This is also highlighting a potential issue with this "add .doc to
      -- everything the user asked for" idea for this implementation. Perhaps instead we should do the normal query
      -- the user asked for, and then chase down those things' docs in a follow-up.
      termsWithMaybeDocs1 ::
        Map
          (HQ.HashQualified Name)
          ( (TermReference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)),
            Maybe (Pretty SyntaxText)
          )
      termsWithMaybeDocs1 =
        docNamedTermsWithNoAssociatedDefinition
          & Map.filterWithKey (\name _ -> nameInOriginalQuery name)
          & Map.map (,Nothing)
          & Map.union termsWithMaybeDocs

      prettyTypes :: [Pretty SyntaxText]
      prettyTypes =
        typesWithMaybeDocs
          & Map.toList
          & List.sortBy (\(n0, _) (n1, _) -> Name.compareAlphabetical n0 n1)
          & map \(name, ((ref, typ), maybeDoc)) ->
            maybe mempty (<> Pretty.newline) maybeDoc
              <> Pretty.prettyTypeWithClasses isClassRef pped (name, ref, typ)

      prettyTerms :: [Pretty SyntaxText]
      prettyTerms =
        termsWithMaybeDocs1
          & Map.toList
          & List.sortBy (\(n0, _) (n1, _) -> Name.compareAlphabetical n0 n1)
          & map \(name, ((ref, term), maybeDoc)) ->
            -- 'prettyTerm' itself emits the leading @given@ keyword
            -- when the binding is a given; we just pass the flag.
            maybe mempty (<> Pretty.newline) maybeDoc
              <> Pretty.prettyTerm pped isSourceFile (maybe False isTest (Reference.toId ref)) (isGivenRef ref) (name, ref, term)
   in NEL.nonEmpty (prettyTypes ++ prettyTerms)
        $> (Pretty.syntaxToColor (Pretty.sep "\n\n" (prettyTypes ++ prettyTerms)), length prettyTerms + length prettyTypes)

renderToConsole ::
  (HQ.HashQualified Name -> Bool) ->
  (TermReference -> Bool) ->
  (TypeReference -> Bool) ->
  PPED.PrettyPrintEnvDecl ->
  Map TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann)) ->
  Map TypeReference (DisplayObject () (Decl Symbol Ann)) ->
  Cli ()
renderToConsole nameInOriginalQuery isGivenRef isClassRef pped terms types = do
  -- If we're writing to console we don't add test-watch syntax
  let isTest _ = False
  let isSourceFile = False
  -- No filepath, render code to console.
  let renderedCodePretty =
        fst
          <$> renderCodePretty
            nameInOriginalQuery
            isGivenRef
            isClassRef
            pped
            isSourceFile
            isTest
            terms
            types
            (Defns Set.empty Set.empty)
  Cli.respond $ DisplayDefinitions (fromMaybe mempty renderedCodePretty)

-- | Render definitions to a file.
-- Returns whether anything was rendered.
-- Definitions can be obtained via definitionsByName
renderToFile ::
  (MonadIO m, Monoid a) =>
  Codebase IO Symbol a ->
  (HQ.HashQualified Name -> Bool) ->
  (TermReference -> Bool) ->
  (TypeReference -> Bool) ->
  (Text -> Text -> Bool -> IO ()) ->
  Maybe (Either (UnisonFile.UnisonFile Symbol Ann) (UnisonFile.TypecheckedUnisonFile Symbol a)) ->
  FilePath ->
  RelativeToFold ->
  PPED.PrettyPrintEnvDecl ->
  Map TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann)) ->
  Map TypeReference (DisplayObject () (Decl Symbol Ann)) ->
  (m Int)
renderToFile codebase nameInOriginalQuery isGivenRef isClassRef writeSource mayTF fp relToFold pped terms types = do
  -- Of all the names we were asked to show, if this is a `WithinFold` showing, then exclude the ones that are
  -- already bound in the file
  let excludeNames =
        case relToFold of
          AboveFold -> Defns Set.empty Set.empty
          WithinFold ->
            case mayTF of
              Nothing -> Defns Set.empty Set.empty
              Just (Left unisonFile) ->
                let boundTermNames = Map.keysSet unisonFile.terms
                    boundTestWatchNames =
                      Map.toList unisonFile.watches
                        & foldMap \case
                          (WatchKind.TestWatch, watches) -> Set.fromList (map (view _1) watches)
                          _ -> Set.empty
                    boundDataDeclNames = Map.keysSet unisonFile.dataDeclarationsId
                    boundEffectDeclNames = Map.keysSet unisonFile.effectDeclarationsId
                 in Defns
                      { terms = boundTermNames <> boundTestWatchNames,
                        types = boundDataDeclNames <> boundEffectDeclNames
                      }
              Just (Right typecheckedUnisonFile) -> UnisonFile.namespaceBindings typecheckedUnisonFile

  -- We build an 'isTest' check to prepend "test>" to tests in a scratch file.
  testRefs <-
    liftIO $ Codebase.runTransaction codebase do
      Codebase.filterTermsByReferenceIdHavingType
        codebase
        (DD.testResultListType mempty)
        (Map.keysSet terms & Set.mapMaybe Reference.toId)
  let isTest r = Set.member r testRefs
  let isSourceFile = True
  let mayRenderedCodePretty = renderCodePretty nameInOriginalQuery isGivenRef isClassRef pped isSourceFile isTest terms types excludeNames
  case mayRenderedCodePretty of
    Just (renderedCodePretty, numRendered) -> do
      let (renderedCodeText) = Pretty.toPlain 80 renderedCodePretty
      liftIO $
        writeSource (Text.pack fp) renderedCodeText case relToFold of
          AboveFold -> True
          WithinFold -> False
      pure numRendered
    Nothing -> pure 0

-- | `nameTerms ppe excludeNames terms` keys each term in `terms` by its best name in `ppe`, but terms whose best name
-- is in the set `exclude` are thrown away.
nameTerms ::
  PPE.PrettyPrintEnv ->
  Set Symbol ->
  Map TermReference term ->
  Map (HQ.HashQualified Name) (TermReference, term)
nameTerms ppe =
  nameDefns (PPE.termName ppe . Referent.Ref)

-- | `nameTypes ppe excludeNames types` keys each type in `types` by its best name in `ppe`, but types whose best name
-- is in the set `exclude` are thrown away.
nameTypes ::
  PPE.PrettyPrintEnv ->
  Set Symbol ->
  Map TypeReference typ ->
  Map (HQ.HashQualified Name) (TypeReference, typ)
nameTypes ppe =
  nameDefns (PPE.typeName ppe)

nameDefns ::
  forall defn ref.
  (ref -> HQ.HashQualified Name) ->
  Set Symbol ->
  Map ref defn ->
  Map (HQ.HashQualified Name) (ref, defn)
nameDefns toName exclude =
  Map.foldlWithKey' f Map.empty
  where
    f ::
      Map (HQ.HashQualified Name) (ref, defn) ->
      ref ->
      defn ->
      Map (HQ.HashQualified Name) (ref, defn)
    f acc ref term =
      case HQ.toName hqName of
        Just name | Set.member (Name.toVar name) exclude -> acc
        _ -> Map.insert hqName (ref, term) acc
      where
        hqName =
          toName ref
