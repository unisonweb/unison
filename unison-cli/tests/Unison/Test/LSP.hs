{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.LSP (test) where

import Control.Monad.Reader
import Crypto.Random qualified as Random
import Data.IntervalMap.Lazy qualified as IM
import Data.List.Extra (firstJust)
import Data.Map.Strict qualified as Map
import Data.String.Here.Uninterpolated (here)
import Data.Text hiding (show)
import Data.Text qualified as Text
import EasyTest
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.VFS qualified as VFS
import System.IO.Temp qualified as Temp
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls (unitRef)
import Unison.Cli.TypeCheck qualified as Typecheck
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Init qualified as Codebase.Init
import Unison.Codebase.SqliteCodebase qualified as SC
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.FileParsers qualified as FileParsers
import Unison.LSP.Conversions
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.FileAnalysis qualified as FileAnalysis
import Unison.LSP.FileAnalysis.UnusedBindings qualified as UnusedBindings
import Unison.LSP.GoToDefinition qualified as GoToDefinition
import Unison.LSP.Hover qualified as Hover
import Unison.LSP.Queries qualified as LSPQ
import Unison.LSP.Types (FileAnalysis (..))
import Unison.LSP.Types qualified as ULSP
import Unison.Lexer.Pos qualified as Lexer
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Summary (FileSummary (..))
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Recursion
import Unison.Var qualified as Var
import UnliftIO qualified

test :: Test ()
test = do
  scope "annotations" $
    tests
      [ refFinding,
        annotationNesting
      ]
  scope "diagnostics" $
    tests
      [ unusedBindingLocations,
        typeMismatchLocations
      ]
  scope "implicit-resolution" $
    tests
      [ implicitResolveDiagnosticCodes,
        implicitResolveNoGivenCodeAction
      ]
  scope "hover" $
    tests
      [ localBindingHoverTest,
        implicitArgHoverTest
      ]
  scope "goto-def" $
    tests
      [ implicitArgGotoDefTest
      ]

newtype TestLsp a = TestLsp {unTestLsp :: ReaderT ULSP.Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader ULSP.Env)

runTestLsp :: TestLsp a -> Test a
runTestLsp action = do
  withTestCodebase \codebase -> do
    projPath <- Codebase.runTransaction codebase $ do
      Codebase.expectCurrentProjectPath
    checkedFilesVar <- UnliftIO.newTVarIO mempty
    lastTouchedFileVar <- UnliftIO.newTVarIO Nothing

    vfsVar <- UnliftIO.newMVar VFS.emptyVFS
    let env =
          ULSP.Env
            { lspContext = error "test runner is missing lspContext",
              codebase,
              lastTouchedFileVar,
              currentNamesCache = pure mempty,
              ppedCache = pure PPED.empty,
              nameSearchCache = error "test runner is missing nameSearchCache",
              currentProjectPathCache = pure projPath,
              vfsVar,
              runtime = error "test runner is missing runtime",
              checkedFilesVar,
              dirtyFilesVar = error "test runner is missing dirtyFilesVar",
              cancellationMapVar = error "test runner is missing cancellationMapVar",
              completionsVar = error "test runner is missing completionsVar",
              scope = error "test runner is missing scope"
            }
    runReaderT (unTestLsp action) env

trm :: Term.F Symbol () () (ABT.Term (Term.F Symbol () ()) Symbol ()) -> LSPQ.SourceNode ()
trm = LSPQ.TermNode . ABT.tm

typ :: Type.F (ABT.Term Type.F Symbol ()) -> LSPQ.SourceNode ()
typ = LSPQ.TypeNode . ABT.tm

pat :: Pattern.Pattern () -> LSPQ.SourceNode ()
pat = LSPQ.PatternNode

-- | Test that we can find the correct reference for a given cursor position.
refFinding :: Test ()
refFinding =
  scope "refs" . tests . fmap makeNodeSelectionTest $
    [ ( "Binary Op lhs",
        [here|term = tr^ue && false|],
        True,
        trm (Term.Boolean True)
      ),
      ( "Binary Op rhs",
        [here|term = true && fa^lse|],
        True,
        trm (Term.Boolean False)
      ),
      ( "Custom Op lhs",
        [here|
a &&& b = a && b
term = tr^ue &&& false
|],
        True,
        trm (Term.Boolean True)
      ),
      ( "Simple type annotation on non-typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = "this won't typecheck"
|],
        False,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Simple type annotation on typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = This
|],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations within bindings for do-block elements",
        [here|
term = do
  first = false
  second = tr^ue
  first && second
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations within bindings for let-block elements",
        [here|
term = let
  first = false
  second = tr^ue
  first && second
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations within actions for let-block elements",
        [here|
term = let
  first = false
  first && tr^ue
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations for blocks with destructuring binds",
        [here|
structural type Identity a = Identity a
term = let
  (Identity a) = Identity tr^ue
  a
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations for destructuring tuples (they have a special parser)",
        [here|
term = let
  (true, fal^se)
        |],
        True,
        trm (Term.Boolean False)
      ),
      ( "Test annotations within pattern binds",
        [here|
term = let
  (third, (^)) = (false, ())
  true
  |],
        True,
        pat (Pattern.Constructor () (ConstructorReference unitRef 0) [])
      ),
      ( "Test annotations for types with arrows",
        [here|
structural type Thing = This | That

term : Thing -> Thing -> Thi^ng
term a b = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for types with effects",
        [here|
unique ability Foo a where
    foo : a

unique ability Bar b where
    bar : b

structural type Thing = This | That

term : (Thing -> {Foo a, Bar b} Th^ing) -> {Foo a, Bar b} Thing
term f = f This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for effects themselves",
        [here|
structural ability Foo a where
    foo : a

structural type Thing = This | That

term : () -> {F^oo a} Thing
term _ = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#h4uhcub76va4tckj1iccnsb07rh0fhgpigqapb4jh5n07s0tugec4nm2vikuv973mab7oh4ne07o6armcnnl7mbfjtb4imphgrjgimg"))
      ),
      ( "Test annotations for types with arrows",
        [here|
structural type Thing = This | That

term : Thing -> Thing -> Thi^ng
term a b = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for types with effects",
        [here|
unique ability Foo a where
    foo : a

unique ability Bar b where
    bar : b

structural type Thing = This | That

term : (Thing -> {Foo a, Bar b} Th^ing) -> {Foo a, Bar b} Thing
term f = f This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for effects themselves",
        [here|
structural ability Foo a where
    foo : a

structural type Thing = This | That

term : () -> {F^oo a} Thing
term _ = This
        |],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#h4uhcub76va4tckj1iccnsb07rh0fhgpigqapb4jh5n07s0tugec4nm2vikuv973mab7oh4ne07o6armcnnl7mbfjtb4imphgrjgimg"))
      ),
      ( "Test annotations for blocks recursive binds",
        [here|
term = let
  f x = g true && x
  g y = f fal^se && y
  f true
        |],
        True,
        trm (Term.Boolean False)
      )
    ]

-- | Test helper which lets you specify a cursor position inline with source text as a '^'.
extractCursor :: Text -> Test (Lexer.Pos, Text)
extractCursor txt =
  case splitOnDelimiter '^' txt of
    Just (before, pos, after) -> pure (pos, before <> after)
    _ -> crash "expected exactly one cursor"

-- | Splits a text on a delimiter, returning the text before and after the delimiter, along with the position of the delimiter.
--
-- >>> splitOnDelimiter '^' "foo b^ar baz"
-- Just ("foo b",Pos {line = 1, column = 5},"ar baz")
splitOnDelimiter :: Char -> Text -> Maybe (Text, Lexer.Pos, Text)
splitOnDelimiter sym txt =
  case second Text.uncons $ Text.breakOn (Text.singleton sym) txt of
    (_before, Nothing) -> Nothing
    (before, Just (_delim, after)) ->
      let col = (Text.length $ Text.takeWhileEnd (/= '\n') before)
          line = Text.count "\n" before + 1
       in Just (before, Lexer.Pos line col, after)

-- | Test helper which lets you specify a relevant block of source inline using specified delimiters
--
-- >>> extractDelimitedBlocks ('{', '}') "foo {bar} baz"
-- Just ("foo bar baz",[(Ann {start = Pos {line = 1, column = 5}, end = Pos {line = 1, column = 8}},"bar")])
--
-- >>> extractDelimitedBlocks ('{', '}') "term =\n  {foo} = 12345"
-- Just ("term =\n  foo = 12345",[(Ann {start = Pos {line = 2, column = 3}, end = Pos {line = 2, column = 6}},"foo")])
--
-- >>> extractDelimitedBlocks ('{', '}') "term =\n  {foo} = {12345} + 10"
-- Just ("term =\n  foo = 12345 + 10",[(Ann {start = Pos {line = 2, column = 3}, end = Pos {line = 2, column = 6}},"foo"),(Ann {start = Pos {line = 3, column = 4}, end = Pos {line = 3, column = 9}},"12345")])
extractDelimitedBlocks :: (Char, Char) -> Text -> Maybe (Text {- entire source text with the delimiters stripped -}, [(Ann {- ann spanning the inside of the delimiters -}, Text {- Text within the delimiters -})])
extractDelimitedBlocks (startDelim, endDelim) txt =
  extractDelimitedBlocksHelper mempty txt
  where
    extractDelimitedBlocksHelper :: Lexer.Pos -> Text -> Maybe (Text, [(Ann, Text)])
    extractDelimitedBlocksHelper offset txt = do
      (beforeStart, startPos, afterStart) <- splitOnDelimiter startDelim txt
      (beforeEnd, endPos, afterEnd) <- splitOnDelimiter endDelim (beforeStart <> afterStart)
      let ann = Ann (offset <> startPos) (offset <> endPos)
      case extractDelimitedBlocksHelper endPos afterEnd of
        Nothing -> pure (beforeEnd <> afterEnd, [(ann, Text.takeWhile (/= endDelim) afterStart)])
        Just (cleanSrc, splits) -> pure $ (beforeEnd <> cleanSrc, (ann, Text.takeWhile (/= endDelim) afterStart) : splits)

makeNodeSelectionTest :: (String, Text, Bool, LSPQ.SourceNode ()) -> Test ()
makeNodeSelectionTest (name, testSrc, testTypechecked, expected) = scope name $ do
  (pos, src) <- extractCursor testSrc
  (pf, mayTypecheckedFile) <- typecheckSrc name src
  scope "parsed file" $ do
    let pfResult =
          UF.terms pf
            & Map.toList
            & firstJust \(_v, (_fileAnn, trm)) ->
              LSPQ.findSmallestEnclosingNode pos trm
    expectEqual (Just expected) (void <$> pfResult)

  when testTypechecked $
    scope "typechecked file" $ do
      tf <- either (\notes -> crash ("Failed to typecheck: " ++ show notes)) pure mayTypecheckedFile
      let tfResult =
            UF.hashTermsId tf
              & toList
              & firstJust \(_fileAnn, _refId, _wk, trm, _typ) ->
                LSPQ.findSmallestEnclosingNode pos trm
      expectEqual (Just expected) (void <$> tfResult)

-- | Tests which assert that the annotation for each ABT node spans at least the span of
-- its children, i.e. all child annotations are contained within the annotation of their parent.
annotationNesting :: Test ()
annotationNesting =
  scope "nesting" . tests . fmap annotationNestingTest $
    [ ( "let blocks",
        [here|
term = let
  x = true
  y = false
  true && false
|]
      ),
      ( "let-rec blocks",
        [here|
term = let
  x a = a && y false
  y b = b && x true
  x true && y true
|]
      ),
      ( "function bindings",
        [here|
term x y = x && y
|]
      )
    ]

annotationNestingTest :: (String, Text) -> Test ()
annotationNestingTest (name, src) = scope name do
  (_, maytf) <- typecheckSrc name src
  tf <- either (\notes -> crash ("Failed to typecheck: " ++ show notes)) pure maytf
  UF.hashTermsId tf
    & toList
    & traverse_ \(_fileAnn, _refId, _wk, trm, _typ) ->
      assertAnnotationsAreNested trm

-- | Asserts that for all nodes in the provided ABT EXCEPT Abs nodes, the annotations of all child nodes are
-- within the span of the parent node.
assertAnnotationsAreNested :: forall f. (Foldable f, Functor f, Show (f (Either String Ann))) => ABT.Term f Symbol Ann -> Test ()
assertAnnotationsAreNested term = do
  case cata alg term of
    Right _ -> pure ()
    Left err -> crash err
  where
    alg :: Algebra (ABT.Term' f Symbol Ann) (Either String Ann)
    alg (ABT.Term' _ ann abt) = do
      childSpan <- abt & foldMapM id
      case abt of
        -- Abs nodes are the only nodes whose annotations are allowed to not contain their children,
        -- they represet the location of the variable being bound instead. Ideally we'd have a separate child
        -- node for that, but we can't add it without editing the ABT or Term types.
        ABT.Abs _ _ ->
          pure (ann <> childSpan)
        _ -> do
          case ann `Ann.encompasses` childSpan of
            -- one of the annotations isn't in the file, don't bother checking.
            Nothing -> pure (ann <> childSpan)
            Just isInFile
              | isInFile -> pure ann
              | otherwise -> Left $ "Containment breach: children aren't contained with the parent:" <> show (ann, abt)

typecheckSrc ::
  String ->
  Text ->
  Test
    ( UF.UnisonFile Symbol Ann,
      Either
        (Seq (Result.Note Symbol Ann))
        (UF.TypecheckedUnisonFile Symbol Ann)
    )
typecheckSrc name src = do
  result <-
    withTestCodebase \codebase -> do
      uniqueName <- Parser.uniqueBase32Namegen <$> Random.getSystemDRG
      let ambientAbilities = []
      let parseNames = mempty
      let parsingEnv =
            Parser.ParsingEnv
              { uniqueNames = uniqueName,
                uniqueTypeGuid = \_ -> pure Nothing,
                names = parseNames,
                maybeNamespace = Nothing,
                localNamespacePrefixedTypesAndConstructors = mempty
              }
      Codebase.runTransaction codebase do
        Parsers.parseFile name (Text.unpack src) parsingEnv >>= \case
          Left err -> pure (Left ("Failed to parse: " ++ show err))
          Right unisonFile -> do
            typecheckingEnv <-
              Typecheck.computeTypecheckingEnvironment
                (FileParsers.ShouldUseTndr'Yes parsingEnv)
                codebase
                ambientAbilities
                []
                unisonFile
            typecheckingResult <-
              Result.runResultT (FileParsers.synthesizeFile typecheckingEnv unisonFile) <&> \case
                (Nothing, notes) -> Left notes
                (Just typecheckedUnisonFile, _) -> Right typecheckedUnisonFile
            pure (Right (unisonFile, typecheckingResult))
  case result of
    Left err -> crash err
    Right val -> pure val

withTestCodebase ::
  (Codebase IO Symbol Ann -> IO r) -> Test r
withTestCodebase action = do
  r <- io do
    tmp <- Temp.getCanonicalTemporaryDirectory
    tmpDir <- Temp.createTempDirectory tmp "lsp-test"
    Codebase.Init.withCreatedCodebase SC.init "lsp-test" tmpDir SC.DontLock action
  either (crash . show) pure r

makeUnusedBindingRangeTest :: (String, Text) -> Test ()
makeUnusedBindingRangeTest (testName, testSrc) = scope testName $ do
  (cleanSrc, ranges) <- case extractDelimitedBlocks ('«', '»') testSrc of
    Nothing -> pure (testSrc, [])
    Just (cleanSrc, ranges) -> pure (cleanSrc, ranges)
  (pf, _mayTypecheckedFile) <- typecheckSrc testName cleanSrc
  UF.terms pf
    & Map.elems
    & \case
      [(_a, trm)] -> do
        let diags = UnusedBindings.analyseTerm (LSP.Uri "test") trm
        matchDiagnostics ranges diags
      _ -> crash "Expected exactly one term"

makeTypecheckerDiagnosticRangeTest :: (String, Text) -> Test ()
makeTypecheckerDiagnosticRangeTest (testName, testSrc) = scope testName $ do
  (cleanSrc, ranges) <- case extractDelimitedBlocks ('«', '»') testSrc of
    Nothing -> pure (testSrc, [])
    Just (cleanSrc, ranges) -> pure (cleanSrc, ranges)
  (_pf, tf) <- typecheckSrc testName cleanSrc
  case tf of
    Left notes -> do
      let codebase = error "unexpected use of codebase"
      let ppe = PPE.empty
      (diags, _codeActions) <- FileAnalysis.analyseNotes codebase (LSP.Uri "test") ppe "test" notes
      matchDiagnostics ranges diags
    Right _ -> crash "Expected typechecking to fail"

matchDiagnostics :: [(Ann, Text)] -> [LSP.Diagnostic] -> Test ()
matchDiagnostics ranges diags = case (ranges, diags) of
  ([], []) -> pure ()
  ([], _ : _) -> crash $ "Got diagnostics that weren't matched: " <> show diags
  (_ : _, []) -> crash $ "Expected diagnostics that weren't provided" <> show ranges
  (range@(ann, _src) : rest, diags) ->
    diags
      & popFind
        ( \diag ->
            let expectedRange = Cv.annToRange ann
                actualRange = Just (diag ^. LSP.range)
             in (expectedRange /= actualRange)
        )
      & \case
        Nothing -> crash $ "Expected diagnostic not found" <> show range <> ", remaining diagnostics: " <> show diags
        Just (_, diags) -> matchDiagnostics rest diags
  where
    popFind :: (a -> Bool) -> [a] -> Maybe (a, [a])
    popFind p = \case
      [] -> Nothing
      x : xs -> if p x then Just (x, xs) else second (x :) <$> popFind p xs

unusedBindingLocations :: Test ()
unusedBindingLocations =
  scope "unused bindings" . tests . fmap makeUnusedBindingRangeTest $
    [ ( "Unused binding in let block",
        [here|term =
  usedOne = true
  «unused» = "unused"
  usedTwo = false
  usedOne && usedTwo
        |]
      ),
      ( "Unused argument",
        [here|term «unused» = 1|]
      ),
      ( "Unused binding in cases block",
        [here|term = cases
  -- Note: the diagnostic _should_ only wrap the unused bindings, but right now it just wraps the whole pattern.
  («unused, used»)
    | used > 0 -> true
    | otherwise -> false
    |]
      ),
      ( "Ignored unused binding in cases block shouldn't error",
        [here|term = cases
  (used, _ignored) -> used
    |]
      )
    ]

makeHoverInfoTest :: (String, Text, Text) -> Test ()
makeHoverInfoTest (name, expected, testSrc) = scope name $ do
  (pos, src) <- extractCursor testSrc
  mayHoverInfo <- runTestLsp . runMaybeT $ do
    let srcName = "test-file"
    let uri = (LSP.Uri srcName)
    fileAnalysis <- (FileAnalysis.checkFileContents uri srcName (0 :: ULSP.FileVersion) src)
    filesVar <- asks ULSP.checkedFilesVar
    liftIO $ UnliftIO.atomically $ do
      files <- UnliftIO.readTVar filesVar
      case Map.lookup uri files of
        Nothing -> do
          mvar <- UnliftIO.newTMVar fileAnalysis
          UnliftIO.modifyTVar' filesVar (Map.insert uri mvar)
        Just mvar -> void $ UnliftIO.putTMVar mvar fileAnalysis
    Hover.hoverInfo uri (uToLspPos pos)
  case mayHoverInfo of
    Nothing -> crash "Expected hover info, got nothing"
    Just actual -> do
      -- We wrap all responses in markdown code blocks
      -- so the client syntax highlights them
      let wrappedExpected = "``` unison\n" <> expected <> "\n```\n"
      expectEqual wrappedExpected actual

localBindingHoverTest :: Test ()
localBindingHoverTest =
  scope "local binding hover types" . tests . fmap makeHoverInfoTest $
    [ ( "Simple local binding",
        "two : ##Nat",
        [here|term =
                one = 1
                two = 2
                one + tw^o
        |]
      ),
      ( "Simple local bindings within a do block",
        "two : ##Nat",
        [here|term = do
                one = 1
                two = 2
                one + tw^o
        |]
      ),
      ( "Self recursive function",
        "recurse : a -> a",
        [here|
term =
  recurse a = if true then a else recu^rse a
  recurse 5
        |]
      ),
      ( "Hover at binding site",
        "recurse : a -> a",
        [here|
term =
  recu^rse a = if true then a else recurse a
  recurse 5
        |]
      ),
      ( "Correctly handles type parameters at binding site",
        "myid : a -> a",
        [here|
term =
  my^id a = a
  myid 1
        |]
      ),
      ( "Correctly handles type parameters at usage site",
        "myid : a -> a",
        [here|
term =
  myid a = a
  my^id 1
        |]
      )
    ]

-- Don't yet support top-level bindings
-- ( "Hover at top-level binding site",
--   "recurse : a -> a",
--   [here|
-- recu^rse a = if true then a else recurse a
--   |]
-- )

typeMismatchLocations :: Test ()
typeMismatchLocations =
  scope "type mismatch locations" . tests . fmap makeTypecheckerDiagnosticRangeTest $
    [ ( "Should highlight the actual incorrect terminal expression in a let block",
        [here|
type Foo = Foo
term : Foo
term =
  _blah = true
  _foo = true
  _baz = true
  «"incorrect"»
        |]
      ),
      ( "Should highlight the actual incorrect terminal expression in an if-block",
        [here|
type Foo = Foo
term : Foo
term = if true
  then «"wrong"»
  else "also wrong"
|]
      ),
      ( "Should highlight the handler of handle expressions",
        [here|
type Foo = Foo
term : Foo
term =
  const a b = a
  handle "" with const «"wrong"»
|]
      )
    ]

------------------------------------------------------------------------------
-- Chunk F1: LSP hover and goto-def on synthesized implicit arguments.
--
-- Strategy: D3's 'Unison.Typechecker.GivenApply.applyGivenDecisions'
-- emits 'Context.ImplicitArgRef' info notes (anchored at the call
-- site of each implicit-arrow function) which 'FileAnalysis' indexes
-- into 'implicitArgInfo'. The hover and goto-def handlers consult
-- this map. Because the typechecker integration for sourced @given@s
-- isn't fully wired (let-given doesn't currently extend the
-- lexical-given environment, and the file-level ambient pool is
-- empty), we drive the LSP layer by constructing a 'FileAnalysis'
-- directly with a populated 'implicitArgInfo' and installing it in
-- the checked-files cache. This is the same shape the LSP would see
-- if D3 had emitted notes against a real file.

-- | Build a synthetic 'FileAnalysis' carrying the supplied
-- 'implicitArgInfo' map, with empty defaults for everything else.
mkSyntheticFileAnalysis ::
  LSP.Uri ->
  IM.IntervalMap LSP.Position [Reference.Reference] ->
  Maybe (UF.TypecheckedUnisonFile Symbol Ann) ->
  Maybe (UF.UnisonFile Symbol Ann) ->
  Maybe FileSummary ->
  FileAnalysis
mkSyntheticFileAnalysis uri implicitInfo mtf mpf mfs =
  FileAnalysis
    { fileUri = uri,
      fileVersion = 0,
      lexedSource = ("", []),
      tokenMap = mempty,
      parsedFile = mpf,
      typecheckedFile = mtf,
      notes = mempty,
      diagnostics = mempty,
      codeActions = mempty,
      localBindingInfo = mempty,
      implicitArgInfo = implicitInfo,
      typeSignatureHints = mempty,
      fileSummary = mfs,
      documentSymbols = mempty
    }

-- | Build an empty 'FileSummary' with only 'termsByReference'
-- populated. Used by the goto-def positive path test.
mkSyntheticFileSummary ::
  Map (Maybe Reference.Id) (Map Symbol (Ann, Term.Term Symbol Ann, Maybe (Type.Type Symbol Ann))) ->
  FileSummary
mkSyntheticFileSummary trmsByRef =
  FileSummary
    { dataDeclsBySymbol = mempty,
      dataDeclsByReference = mempty,
      effectDeclsBySymbol = mempty,
      effectDeclsByReference = mempty,
      termsBySymbol = mempty,
      termsByReference = trmsByRef,
      testWatchSummary = mempty,
      exprWatchSummary = mempty,
      fileNames = mempty
    }

-- | Hover should mention "Implicit argument; resolved from given …"
-- when the cursor lands on a position recorded in 'implicitArgInfo'.
implicitArgHoverTest :: Test ()
implicitArgHoverTest = scope "implicit-arg hover" $ do
  -- Anchor the implicit-arg note at line 1, columns 8-9 (LSP 0-based:
  -- line 0, char 7-8). The chosen given is a builtin reference for
  -- which the empty PPED falls back to printing the hash, giving us
  -- a stable expected text.
  let anchorInterval =
        IM.ClosedInterval
          (LSP.Position 0 7)
          (LSP.Position 0 9)
      givenRef = Reference.Builtin "Show.Nat"
      implicitInfo = IM.singleton anchorInterval [givenRef]
      uri = LSP.Uri "test-implicit"
  hoverTxt <- runTestLsp $ do
    let fa = mkSyntheticFileAnalysis uri implicitInfo Nothing Nothing Nothing
    filesVar <- asks ULSP.checkedFilesVar
    liftIO $ UnliftIO.atomically do
      mvar <- UnliftIO.newTMVar fa
      UnliftIO.modifyTVar' filesVar (Map.insert uri mvar)
    -- Hover at line 0, char 8 (inside the recorded interval).
    runMaybeT (Hover.hoverInfo uri (LSP.Position 0 8))
  case hoverTxt of
    Nothing -> crash "expected implicit-arg hover, got Nothing"
    Just t -> do
      let txt = Text.unpack t
      let needle = "Implicit argument; resolved from given"
      if Text.isInfixOf (Text.pack needle) t
        then ok
        else crash ("hover text missing implicit-arg phrase: " <> txt)

-- | Goto-def should jump to the resolved given when the cursor lands
-- on a position recorded in 'implicitArgInfo' and the given resolves
-- to an in-file 'DerivedId'.
implicitArgGotoDefTest :: Test ()
implicitArgGotoDefTest = scope "implicit-arg goto-def" $ do
  -- Negative case: builtin refs have no in-file location, so goto-def
  -- should fail (return Nothing). This confirms that the implicit-arg
  -- branch is consulted but produces no result — the localBinding
  -- fallback also has nothing to return.
  let anchorInterval =
        IM.ClosedInterval
          (LSP.Position 0 7)
          (LSP.Position 0 9)
      builtinRef = Reference.Builtin "Show.Nat"
      builtinImplicitInfo = IM.singleton anchorInterval [builtinRef]
      builtinUri = LSP.Uri "test-implicit-goto-builtin"
  scope "builtin given returns Nothing" $ do
    builtinResult <- runTestLsp $ do
      let fa = mkSyntheticFileAnalysis builtinUri builtinImplicitInfo Nothing Nothing Nothing
      filesVar <- asks ULSP.checkedFilesVar
      liftIO $ UnliftIO.atomically do
        mvar <- UnliftIO.newTMVar fa
        UnliftIO.modifyTVar' filesVar (Map.insert builtinUri mvar)
      runMaybeT (GoToDefinition.locationInfo builtinUri (LSP.Position 0 8))
    case builtinResult of
      Nothing -> ok -- expected: builtins have no source location
      Just r -> crash ("expected no goto-def for builtin given, got: " <> show r)

  -- Positive case: a 'DerivedId' given that points at a term defined
  -- in the file resolves to the term's annotation range.
  scope "in-file derived given returns its range" $ do
    -- The resolved given lives at line 2, columns 3-7 in the file
    -- (Unison file positions are 1-based). After uToLspPos this maps
    -- to LSP Range (Position 1 2) (Position 1 6).
    let givenAnn = Ann.Ann (Lexer.Pos 2 3) (Lexer.Pos 2 7)
        expectedRange = LSP.Range (LSP.Position 1 2) (LSP.Position 1 6)
    -- Build a 'Reference.Id' by parsing a derived reference text and
    -- extracting its 'Id'. This avoids manually constructing a Hash.
    derivedRefId <- case Reference.unsafeFromText "#abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcd" of
      Reference.DerivedId rid -> pure rid
      _ -> crash "expected DerivedId from unsafeFromText"
    let givenSym :: Symbol
        givenSym = Var.named "myGiven"
        givenTerm = Term.boolean givenAnn True -- placeholder, never inspected
        termsByRef =
          Map.singleton
            (Just derivedRefId)
            (Map.singleton givenSym (givenAnn, givenTerm, Nothing))
        fileSummary' = mkSyntheticFileSummary termsByRef
        givenRef = Reference.DerivedId derivedRefId
        implicitInfo' = IM.singleton anchorInterval [givenRef]
        uri = LSP.Uri "test-implicit-goto-derived"
    derivedResult <- runTestLsp $ do
      let fa = mkSyntheticFileAnalysis uri implicitInfo' Nothing Nothing (Just fileSummary')
      filesVar <- asks ULSP.checkedFilesVar
      liftIO $ UnliftIO.atomically do
        mvar <- UnliftIO.newTMVar fa
        UnliftIO.modifyTVar' filesVar (Map.insert uri mvar)
      runMaybeT (GoToDefinition.locationInfo uri (LSP.Position 0 8))
    expectEqual (Just expectedRange) derivedResult

------------------------------------------------------------------------------
-- Chunk F2: LSP diagnostics + code actions for implicit-resolution
-- failures.
--
-- We exercise 'analyseNotes' directly with a synthetic
-- 'Result.UnresolvedImplicit' note for each 'GR.ResolveError'
-- variant, and verify the resulting diagnostic carries the expected
-- string code and that the right code actions are produced.

-- | Build a sample goal type. The renderer ('PrintError') falls back
-- to a hash for an unknown 'Reference.Builtin' under an empty PPE,
-- which is fine — F2's tests only check the diagnostic 'code' field
-- and code-action shape, not the prose.
fakeGoalType :: Type.Type Symbol Ann
fakeGoalType = Type.builtin Ann.External "F2.TestGoal"

-- | Construct an 'UnresolvedImplicit' note carrying the supplied
-- 'ResolveError'. The apply-site location is fixed at line 1, col 1
-- (one-based) for predictable diagnostic ranges.
mkImplicitNote :: GR.ResolveError Symbol Ann -> Result.Note Symbol Ann
mkImplicitNote err =
  let pos = Lexer.Pos 1 1
      pos' = Lexer.Pos 1 2
      ann = Ann pos pos'
   in Result.UnresolvedImplicit ann fakeGoalType err

-- | Run 'FileAnalysis.analyseNotes' on a single 'UnresolvedImplicit'
-- note and return the (diagnostics, code-actions) pair.
runAnalyseImplicit ::
  GR.ResolveError Symbol Ann ->
  Test ([LSP.Diagnostic], [ULSP.RangedCodeAction])
runAnalyseImplicit err = io do
  let codebase = error "F2 test: codebase unused"
  let ppe = PPE.empty
  FileAnalysis.analyseNotes
    codebase
    (LSP.Uri "f2-test")
    ppe
    "f2-test"
    [mkImplicitNote err]

-- | Each 'ResolveError' variant should produce exactly one
-- diagnostic, tagged with the expected string code, at Error
-- severity, with source "unison".
implicitResolveDiagnosticCodes :: Test ()
implicitResolveDiagnosticCodes = scope "diagnostic codes per category" $ do
  let cases :: [(String, GR.ResolveError Symbol Ann, Text)]
      cases =
        [ ( "NoGiven",
            GR.NoGiven fakeGoalType [],
            "implicit-no-given"
          ),
          ( "Ambiguous",
            GR.Ambiguous fakeGoalType [],
            "implicit-ambiguous"
          ),
          ( "DepthExceeded",
            GR.DepthExceeded [],
            "implicit-depth-exceeded"
          ),
          ( "Cycle",
            GR.Cycle [],
            "implicit-cycle"
          ),
          ( "UnresolvedMetavarInGoal",
            GR.UnresolvedMetavarInGoal fakeGoalType,
            "implicit-unresolved-metavar"
          )
        ]
  for_ cases $ \(name, err, expectedCode) -> scope name $ do
    (diags, _actions) <- runAnalyseImplicit err
    case diags of
      [d] -> do
        let actualCode = d ^. LSP.code
        let expected = Just (LSP.InR expectedCode)
        if actualCode == expected
          then pure ()
          else
            crash $
              "expected diagnostic code "
                <> show expected
                <> " but got "
                <> show actualCode
        let sev = d ^. LSP.severity
        when (sev /= Just LSP.DiagnosticSeverity_Error) $
          crash $
            "expected Error severity, got " <> show sev
        ok
      ds -> crash ("expected exactly one diagnostic, got " <> show (Prelude.length ds))

-- | The 'NoGiven' code action must include a workspace edit that
-- inserts a 'given _ : <T> = todo' skeleton at the top of the file
-- (line 0, column 0).
implicitResolveNoGivenCodeAction :: Test ()
implicitResolveNoGivenCodeAction = scope "NoGiven code action inserts skeleton at file top" $ do
  (_diags, actions) <- runAnalyseImplicit (GR.NoGiven fakeGoalType [])
  case actions of
    [rca] -> do
      let ca = rca ^. LSP.codeAction
      let title = ca ^. LSP.title
      when (not (Text.isInfixOf "Define given" title)) $
        crash ("expected title to mention 'Define given', got: " <> Text.unpack title)
      case ca ^. LSP.edit of
        Nothing -> crash "expected workspace edit on the NoGiven code action"
        Just we -> case we ^. LSP.changes of
          Nothing -> crash "expected workspace edit changes map"
          Just changes -> case Map.lookup (LSP.Uri "f2-test") changes of
            Nothing -> crash "expected an edit targeting the test URI"
            Just edits -> case edits of
              [e] -> do
                let editRange = e ^. LSP.range
                let expectedRange =
                      LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)
                when (editRange /= expectedRange) $
                  crash $
                    "expected edit at file top, got " <> show editRange
                let txt = e ^. LSP.newText
                when (not (Text.isInfixOf "given" txt)) $
                  crash ("expected 'given' in edit text, got: " <> Text.unpack txt)
                ok
              es -> crash ("expected one TextEdit, got " <> show (Prelude.length es))
    _ -> crash ("expected exactly one code action, got " <> show (Prelude.length actions))
