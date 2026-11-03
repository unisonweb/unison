module Unison.Codebase.Editor.HandleInput.FindAndReplace
  ( handleStructuredFindReplaceI,
    handleStructuredFindI,
    handleTextFindI,
  )
where

import Control.Lens hiding (at)
import Control.Monad.Reader (ask)
import Control.Monad.State
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import System.Directory qualified as Directory
import System.FilePath (normalise, takeBaseName, takeDirectory, takeFileName, (</>))
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as P
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.EditNamespace (getNamesForEdit)
import Unison.Codebase.Editor.HandleInput.Load (parseAndTypecheckUnisonFile)
import Unison.Codebase.Editor.HandleInput.ShowDefinition (renderToFile)
import Unison.Codebase.Editor.Input (RelativeToFold (..), RewriteTarget (..), RewritesSpec (..))
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Parsers qualified as Parsers
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo, empty)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText, toVar)
import Unison.Syntax.Name qualified as Name (toVar, unsafeParseText, unsafeParseVar)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UFN
import Unison.Util.Alphabetical qualified as Alphabetical
import Unison.Util.Pretty qualified as P
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.Var qualified as Var

handleStructuredFindReplaceI :: RewritesSpec -> Maybe RewriteTarget -> Cli ()
handleStructuredFindReplaceI spec target = do
  currentBranch <- Cli.getCurrentBranch0
  let branchNames = Branch.toNames currentBranch
  (ruleTerms, ruleNames) <- lookupRuleTerms spec
  case target of
    Just (RewriteFile path) -> do
      (text, uf0) <- loadAndParse path
      -- The file is rewritten in place, so that its name stays the same through a refactoring, and the version
      -- before is saved beside it.
      backup <- newRewritesPath (rewritesDirectoryFor path) (Just (takeBaseName path))
      applyRewrites False ruleTerms ruleNames branchNames (InPlace path backup text) uf0
    Just (RewriteNamespace path0) -> rewriteDefinitionsIn (Just path0) currentBranch ruleTerms ruleNames
    Nothing -> rewriteDefinitionsIn Nothing currentBranch ruleTerms ruleNames
  where
    -- Rewrite the definitions in the current branch (or in one namespace of it) that the rules match.
    rewriteDefinitionsIn mpath currentBranch ruleTerms ruleNames = do
      Cli.Env {codebase} <- ask
      let path = maybe mempty Path.fromPath' mpath
          scope = (if path == mempty then Branch.withoutLib else id) (Branch.getAt0 path currentBranch)
          scopeNames =
            case Path.toName path of
              Nothing -> Branch.toNames scope
              Just prefix -> Names.prefix0 prefix (Branch.toNames scope)
          ppe = PPED.makePPED (PPE.hqNamer 10 scopeNames) (PPE.suffixifyByHashName scopeNames)
      rules <- extractRules InvalidStructuredFindReplace (\_ tm -> tm) ruleTerms
      matching <-
        Set.fromList . catMaybes <$> for (Set.toList (Relation.ran scopeNames.terms)) \case
          Referent.Ref ref@(Reference.DerivedId r) -> do
            oe <- Cli.runTransaction (Codebase.getTerm codebase r)
            pure (ref <$ guard (maybe False (\e -> any (($ e) . snd) rules) oe))
          _ -> pure Nothing
      let matchingNames =
            Names
              { terms = Relation.filterRan (\case Referent.Ref ref -> Set.member ref matching; _ -> False) scopeNames.terms,
                types = mempty
              }
      (types, terms) <- Cli.runTransaction (getNamesForEdit codebase ppe matchingNames)
      rendered <- liftIO (newIORef Nothing)
      _ <-
        renderToFile codebase (const True) (\_ txt _ -> writeIORef rendered (Just txt)) Nothing "rewrites" AboveFold ppe terms types
      branchNames <- Branch.toNames <$> Cli.getCurrentBranch0
      pp <- Cli.getCurrentProjectPath
      liftIO (readIORef rendered) >>= \case
        Nothing -> Cli.respond (OutputRewrittenFile "" Nothing [])
        Just text -> do
          parsingEnv <- Cli.makeParsingEnv pp branchNames
          uf0 <-
            Cli.runTransaction (Parsers.parseFile "rewrites" (Text.unpack text) parsingEnv)
              & onLeftM \err -> Cli.returnEarly (ParseErrors text [err])
          dest <- newRewritesPath "rewrites" Nothing
          applyRewrites True ruleTerms ruleNames branchNames (NewFile dest) uf0

-- | Where a rewrite's result goes: a new file, or the file that was rewritten, with the text it had saved elsewhere.
data RewriteDestination
  = NewFile FilePath
  | InPlace FilePath FilePath Text

-- | Apply the rewrites to the definitions of a parsed file, and write the result, if anything changed.
applyRewrites ::
  -- | Whether to leave the definitions that didn't change out of the new file
  Bool ->
  [(HQ.HashQualified Name, Term Symbol Ann)] ->
  -- | The names the rules' file defines, if they came from a file
  Names ->
  -- | The names of the branch
  Names ->
  RewriteDestination ->
  UF.UnisonFile Symbol Ann ->
  Cli ()
applyRewrites onlyChanged ruleTerms0 ruleNames branchNames destination uf0 = do
  env <- ask
  hqLength <- Cli.runTransaction Codebase.hashLength
  fileNames <- Cli.getNamesFromLatestFile
  let (prepare, uf, finish) = UF.prepareRewrite uf0
      -- The rules may name definitions that the target file defines too, as when the rules are in the target file itself,
      -- with the new version of a definition they rewrite uses of. Typechecking the rules' file gave those definitions
      -- hashes of their own, but the rewritten code has to refer to the target's: so a rule's reference to such a term
      -- becomes the term's name, which the file binds, and a reference to such a type or constructor becomes the target's.
      targetDeclNames = UFN.toNames uf0
      targetTermNames = Set.map Name.unsafeParseVar (UFN.toTermAndWatchNames uf0)
      termVars =
        Map.fromList
          [(ref, Name.toVar name) | (name, Referent.Ref ref) <- Relation.toList ruleNames.terms, Set.member name targetTermNames]
      conUpdates =
        Map.fromList
          [ (con, con')
          | (name, con@Referent.Con {}) <- Relation.toList ruleNames.terms,
            con' <- Set.toList (Relation.lookupDom name targetDeclNames.terms),
            con /= con'
          ]
      typeUpdates =
        Map.fromList
          [ (ref, ref')
          | (name, ref) <- Relation.toList ruleNames.types,
            ref' <- Set.toList (Relation.lookupDom name targetDeclNames.types),
            ref /= ref'
          ]
      retarget =
        Term.updateDependencies conUpdates typeUpdates . ABT.rebuildUp' \case
          tm@(Term.Ref' ref) | Just v <- Map.lookup ref termVars -> Term.var (ABT.annotation tm) v
          tm -> tm
      ruleTerms = second retarget <$> ruleTerms0
  rules <- extractRules InvalidStructuredFindReplace prepare ruleTerms
  let -- For a name the target file defines, its definition is the one to show, not the rules' file's or the latest file's.
      allNames = targetDeclNames `Names.shadowing` (fileNames <> ruleNames <> branchNames)
      ppe = PPED.makePPED (PPE.hqNamer hqLength allNames) (PPE.suffixifyByHash allNames)
      go n tm [] = if n == (0 :: Int) then Nothing else Just tm
      go n tm ((r, _) : rules) = case r tm of
        Nothing -> go n tm rules
        Just tm -> go (n + 1) tm rules
      leaveAlone = Set.fromList [HQ.toVar rule | (rule, _) <- ruleTerms]
      (vs, uf0') = UF.rewrite leaveAlone (\tm -> go 0 tm rules) uf
      finished = finish uf0'
      uf' = (vs, if onlyChanged then finished & #terms %~ (`Map.restrictKeys` Set.fromList vs) else finished)
      (dest, backup) = case destination of
        NewFile path -> (path, Nothing)
        InPlace path previous _ -> (path, Just previous)
      header =
        [ "Rewrote using: " <> Text.intercalate ", " (HQ.toText . fst <$> ruleTerms),
          "Review the definitions below, then typecheck this file (`load " <> Text.pack dest <> "`) and run `update` when you are done."
        ]
  #latestTypecheckedFile .= Nothing
  case vs of
    [] -> pure ()
    _ -> do
      #latestFile ?= (dest, True)
      -- A file rewritten in place gets no header: it is the user's own file, and a comment naming the rules would be
      -- one more thing to strip out (and a trap for a script that looks for the rules by name).
      let rendered = P.toPlain 80 case destination of
            NewFile _ -> renderRewrittenFile ppe header uf'
            InPlace {} -> P.prettyUnisonFile ppe (snd uf')
      liftIO do
        case destination of
          NewFile _ -> pure ()
          InPlace path previous text -> do
            env.writeSource (Text.pack previous) text False
            -- `writeSource` adds to a file that exists; the rewritten file replaces it.
            exists <- Directory.doesFileExist path
            when exists (Directory.removeFile path)
        env.writeSource (Text.pack dest) rendered False
  Cli.respond $ OutputRewrittenFile dest backup vs

-- | The directory that a file of rewrites goes in, for rewrites of the given file: a @rewrites@ directory beside it, or
-- the directory it is in if that is a @rewrites@ directory.
rewritesDirectoryFor :: FilePath -> FilePath
rewritesDirectoryFor path
  | takeFileName dir == "rewrites" = dir
  | otherwise = normalise (dir </> "rewrites")
  where
    dir = takeDirectory path

-- | @<dir>/<yyyy-mm-dd-hh-mm>-[<file>-]<short-guid>.u@, where @file@ is the file being rewritten, if any. A transcript run
-- (a test, or `ucm transcript`, but not the MCP server) gets a fixed name, so that its output doesn't change.
newRewritesPath :: FilePath -> Maybe String -> Cli FilePath
newRewritesPath dir mfile = do
  env <- ask
  (time, guid) <-
    if env.isTranscriptTest || (isNothing env.watchState && not env.unattended)
      then pure ("0000-00-00-00-00", "00000000")
      else liftIO do
        now <- getCurrentTime
        uuid <- UUID.nextRandom
        pure (formatTime defaultTimeLocale "%Y-%m-%d-%H-%M" now, take 8 (UUID.toString uuid))
  let path = dir </> time <> "-" <> maybe "" (<> "-") mfile <> guid <> ".u"
  -- An agent's own working directory is usually not the server's, so it gets the whole path.
  if env.unattended then liftIO (Directory.makeAbsolute path) else pure path

-- | Parse a file, without typechecking it: its text, and the parse.
loadAndParse :: FilePath -> Cli (Text, UF.UnisonFile Symbol Ann)
loadAndParse path = do
  env <- ask
  currentBranch <- Cli.getCurrentBranch0
  pp <- Cli.getCurrentProjectPath
  text <-
    liftIO (env.loadSource (Text.pack path)) >>= \case
      Cli.InvalidSourceNameError -> Cli.returnEarly $ InvalidSourceName path
      Cli.LoadError -> Cli.returnEarly $ SourceLoadFailed path
      Cli.LoadSuccess text -> pure text
  parsingEnv <- Cli.makeParsingEnv pp (Branch.toNames currentBranch)
  uf <-
    Cli.runTransaction (Parsers.parseFile path (Text.unpack text) parsingEnv)
      & onLeftM \err -> Cli.returnEarly (ParseErrors text [err])
  pure (text, uf)

-- | Parse and typecheck a file, leaving the latest file and the latest typechecked file as they were.
loadAndTypecheck :: FilePath -> Cli (UF.TypecheckedUnisonFile Symbol Ann)
loadAndTypecheck path = do
  env <- ask
  currentBranch <- Cli.getCurrentBranch0
  text <-
    liftIO (env.loadSource (Text.pack path)) >>= \case
      Cli.InvalidSourceNameError -> Cli.returnEarly $ InvalidSourceName path
      Cli.LoadError -> Cli.returnEarly $ SourceLoadFailed path
      Cli.LoadSuccess text -> pure text
  latestFile <- use #latestFile
  latestTypecheckedFile <- use #latestTypecheckedFile
  tf <- parseAndTypecheckUnisonFile (Branch.toNames currentBranch) (Text.pack path) text
  #latestFile .= latestFile
  #latestTypecheckedFile .= latestTypecheckedFile
  pure tf

handleStructuredFindI :: RewritesSpec -> Cli ()
handleStructuredFindI spec = do
  Cli.Env {codebase} <- ask
  (ruleTerms, _) <- lookupRuleTerms spec
  rules0 <- extractRules InvalidStructuredFind (\_ tm -> tm) ruleTerms
  let rules = snd <$> rules0
  currentBranch <- Cli.getCurrentBranch0
  hqLength <- Cli.runTransaction Codebase.hashLength
  fileNames <- Cli.getNamesFromLatestFile
  let names = fileNames <> Branch.toNames currentBranch
  let ppe = PPED.makePPED (PPE.hqNamer hqLength names) (PPE.suffixifyByHash names)
  let fqppe = PPED.unsuffixifiedPPE ppe
  results :: [(HQ.HashQualified Name, Referent)] <- pure $ do
    r <- Set.toList (Relation.ran $ Names.terms names)
    Just hq <- [PPE.terms fqppe r]
    fullName <- [HQ'.toName hq]
    guard (not (Name.beginsWithSegment fullName NameSegment.libSegment))
    Referent.Ref _ <- pure r
    Just shortName <- [PPE.terms (PPED.suffixifiedPPE ppe) r]
    pure (HQ'.toHQ shortName, r)
  let ok (hq, Referent.Ref (Reference.DerivedId r)) = do
        oe <- Cli.runTransaction (Codebase.getTerm codebase r)
        pure $ (hq, maybe False (\e -> any ($ e) rules) oe)
      ok (hq, _) = pure (hq, False)
  results0 <- traverse ok results
  let results = Alphabetical.sortAlphabetically [hq | (hq, True) <- results0]
  Cli.setNumberedArgs $ map SA.HashQualified results
  Cli.respond (ListStructuredFind results)

handleTextFindI :: Bool -> [String] -> Cli ()
handleTextFindI allowLib tokens = do
  Cli.Env {codebase} <- ask
  currentBranch <- Cli.getCurrentBranch0
  hqLength <- Cli.runTransaction Codebase.hashLength
  let names = Branch.toNames currentBranch
  let ppe = PPED.makePPED (PPE.hqNamer hqLength names) (PPE.suffixifyByHash names)
  let fqppe = PPED.unsuffixifiedPPE ppe
  results :: [(HQ.HashQualified Name, Referent)] <- pure $ do
    r <- Set.toList (Relation.ran $ Names.terms names)
    Just hq <- [PPE.terms fqppe r]
    fullName <- [HQ'.toName hq]
    guard (allowLib || not (Name.beginsWithSegment fullName NameSegment.libSegment))
    Referent.Ref _ <- pure r
    Just shortName <- [PPE.terms (PPED.suffixifiedPPE ppe) r]
    pure (HQ'.toHQ shortName, r)
  let ok (hq, Referent.Ref (Reference.DerivedId r)) = do
        oe <- Cli.runTransaction (Codebase.getTerm codebase r)
        pure $ (hq, maybe False containsTokens oe)
      ok (hq, _) = pure (hq, False)
  results0 <- traverse ok results
  let results = Alphabetical.sortAlphabetically [hq | (hq, True) <- results0]
  Cli.setNumberedArgs $ map SA.HashQualified results
  Cli.respond (ListTextFind allowLib results)
  where
    tokensTxt = Text.pack <$> tokens
    containsTokens tm =
      hasAll . join $ ABT.find txts tm
      where
        hasAll txts = all (\tok -> any (\haystack -> Text.isInfixOf tok haystack) txts) tokensTxt
        txts (Term.Text' haystack) = ABT.Found [haystack]
        txts (Term.Nat' haystack) = ABT.Found [Text.pack (show haystack)]
        txts (Term.Int' haystack) = ABT.Found [Text.pack (show haystack)]
        txts (Term.Float' haystack) = ABT.Found [Text.pack (show haystack)]
        txts (Term.Char' haystack) = ABT.Found [Text.pack [haystack]]
        txts (Term.Match' _ cases) = ABT.Found r
          where
            r = join $ Pattern.foldMap' txtPattern . Term.matchPattern <$> cases
        txts _ = ABT.Continue
        txtPattern (Pattern.Text _ txt) = [txt]
        txtPattern (Pattern.Nat _ n) = [Text.pack (show n)]
        txtPattern (Pattern.Int _ n) = [Text.pack (show n)]
        txtPattern (Pattern.Float _ n) = [Text.pack (show n)]
        txtPattern (Pattern.Char _ c) = [Text.pack [c]]
        txtPattern _ = []

-- | The terms that the spec names, which should be @\@rewrite@ blocks or functions that return one, each with its name,
-- and the names that the spec's file defines (none if the spec has no file). A file is parsed and typechecked when the
-- command runs, without changing the latest file, so the names in the rules are resolved by type like anywhere else.
lookupRuleTerms :: RewritesSpec -> Cli ([(HQ.HashQualified Name, Term Symbol Ann)], Names)
lookupRuleTerms spec = case spec.file of
  Just path -> do
    tf <- loadAndTypecheck path
    let bindings =
          [ (v, tm)
          | (v, (_, _, watchKind, tm, _)) <- Map.toList (UF.hashTerms tf),
            isNothing watchKind
          ]
        fileNames = UFN.typecheckedToNames tf
    rules <- case spec.rewrites of
      [] -> case bindings of
        [(v, tm)] -> pure [(HQ.NameOnly (Name.unsafeParseText (Var.name v)), tm)]
        _ ->
          Cli.returnEarly . InvalidRewriteSpec $
            "The file "
              <> Text.pack path
              <> " has "
              <> Text.pack (show (length bindings))
              <> " definitions, so I need to know which of them are rewrites. Example syntax: "
              <> Text.pack path
              <> ":rule1,rule2"
      names -> for names \name -> case lookup (HQ.toVar name) bindings of
        Just tm -> pure (name, tm)
        Nothing ->
          Cli.returnEarly . InvalidRewriteSpec $
            "I couldn't find a definition named " <> HQ.toText name <> " in " <> Text.pack path <> "."
    pure (rules, fileNames)
  Nothing ->
    (,mempty) <$> for spec.rewrites \name -> do
      Cli.Env {codebase} <- ask
      currentBranch <- Cli.getCurrentBranch0
      hqLength <- Cli.runTransaction Codebase.hashLength
      fileNames <- Cli.getNamesFromLatestFile
      let currentNames = fileNames <> Branch.toNames currentBranch
      let ppe = PPED.makePPED (PPE.hqNamer hqLength currentNames) (PPE.suffixifyByHash currentNames)
      ot <- Cli.getTermFromLatestParsedFile name
      ot <- case ot of
        Just _ -> pure ot
        Nothing -> do
          case Names.lookupHQTerm Names.IncludeSuffixes name currentNames of
            s
              | Set.size s == 1,
                Referent.Ref (Reference.DerivedId r) <- Set.findMin s ->
                  Cli.runTransaction (Codebase.getTerm codebase r)
            s -> Cli.returnEarly (TermAmbiguous (PPE.suffixifiedPPE ppe) name s)
      tm <- maybe (Cli.returnEarly (TermAmbiguous (PPE.suffixifiedPPE ppe) name mempty)) pure ot
      pure (name, tm)

-- | Turn each rewrite term into a function that rewrites a term, and one that says whether a term has anything to
-- rewrite. @prepare@ is applied to the terms first (to make references to the definitions in a file stand for them).
extractRules ::
  (HQ.HashQualified Name -> Output) ->
  ([Symbol] -> Term Symbol Ann -> Term Symbol Ann) ->
  [(HQ.HashQualified Name, Term Symbol Ann)] ->
  Cli [(Term Symbol Ann -> Maybe (Term Symbol Ann), Term Symbol Ann -> Bool)]
extractRules onErr prepare ruleTerms =
  concat <$> for ruleTerms \(rule, tm) -> do
    let extract vs tm = case tm of
          Term.Ann' tm _typ -> extract vs tm
          (DD.RewriteTerm' lhs rhs) ->
            pure
              ( ABT.rewriteExpression lhs rhs,
                ABT.containsExpression lhs
              )
          (DD.RewriteCase' lhs rhs) ->
            pure
              ( Term.rewriteCasesLHS lhs rhs,
                fromMaybe False . Term.containsCaseTerm lhs
              )
          (DD.RewriteSignature' _vs lhs rhs) ->
            pure (Term.rewriteSignatures lhs rhs, Term.containsSignature lhs)
          _ -> Cli.returnEarly (onErr rule)
        extractOuter vs0 tm = case tm of
          Term.Ann' tm _typ -> extractOuter vs0 tm
          Term.LamsNamed' vs tm -> extractOuter (vs0 ++ vs) tm
          tm -> case prepare vs0 tm of
            DD.Rewrites' rules -> traverse (extract vs0) rules
            _ -> Cli.returnEarly (onErr rule)
    extractOuter [] tm

renderRewrittenFile :: (Ord a, Var v) => PPED.PrettyPrintEnvDecl -> [Text] -> ([v], UF.UnisonFile v a) -> P.Pretty P.ColorText
renderRewrittenFile ppe headerLines (vs, uf) = do
  let prettyVar = P.text . Var.name
      modifiedDefs = P.sep " " (P.blue . prettyVar <$> vs)
      header = P.lines (("-- " <>) . P.text <$> headerLines) <> "\n" <> "-- | Modified definition(s): " <> modifiedDefs
   in (header <> "\n\n" <> P.prettyUnisonFile ppe uf)
