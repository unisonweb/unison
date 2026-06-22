{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Elaborating the 'Surface' IR to the content-addressed AST (the read\/parse direction).
--
-- This is the syntax-neutral half of parsing: it turns an 'SFile' (which any dialect's parser produces) into a
-- @UnisonFile Symbol Ann@, resolving names with 'Term.bindNames' exactly as the file parser does. Every dialect's
-- parser reuses this, so a new dialect only needs a @text -> SFile@ front end.
--
-- v1 scope mirrors 'Unison.Syntax.Dialect.SExpr.Parser': top-level bindings + signatures and the full term\/type\/
-- pattern grammar; declaring new types, watches, and docs are deferred (an 'SFile' using them yields an error).
module Unison.Syntax.Surface.Elaborate
  ( elaborateFile,
    elaborateType,
    parseDocs,
    resolveDeclGuids,
  )
where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Char (isSpace)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as CP
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as BuiltinDecls
import Unison.ConstructorReference (ConstructorReference)
import Unison.DataDeclaration (DataDeclaration (..), EffectDeclaration)
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Records (generateRecordAccessors)
import Unison.HashQualified qualified as HQ
import Unison.Lexer.Pos qualified as Pos
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Pattern (Pattern)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.ConstructorType qualified as CT
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parsers qualified as Parsers
import Unison.PrettyPrintEnv.Names qualified as PPEN
import Unison.ShortHash qualified as ShortHash
import Unison.Symbol (Symbol)
import Unison.Syntax.Lexer.Unison (Err, Token, typeOrTerm)
import Unison.Syntax.Name qualified as Name (toVar, unsafeParseVar)
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.Parser.Doc qualified as Doc
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Syntax.Var qualified as Var (namespaced)
import Unison.Syntax.Surface
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Util.Pretty qualified as PP
import Unison.Typechecker.Components qualified as Components
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.UnisonFile (UnisonFile (..))
import Unison.UnisonFile.Env qualified as UFE
import Unison.UnisonFile.Names qualified as UFN
import Unison.Var (Var)
import Unison.Var qualified as Var

type E v = Either (Parser.Err v)

errFail :: String -> Parser.Err v
errFail msg = P.FancyError 0 (Set.singleton (P.ErrorFail msg))

-- | Turn a surface name into a variable. (Name resolution to references happens later, in 'Term.bindNames'.)
sNameVar :: (Var v) => SName -> v
sNameVar = \case
  HQ.NameOnly n -> Name.toVar n
  HQ.HashQualified n _ -> Name.toVar n
  HQ.HashOnly sh -> Var.named (ShortHash.toText sh)

-- | Elaborate a whole surface file into a 'UnisonFile'. @docMap@ maps each doc literal's @{{ … }}@ source text to its
-- already-parsed term (produced monadically by 'parseDocs'), keeping this function pure.
elaborateFile :: forall v. (Var v) => Names -> Map Text (Term v Ann) -> SFile -> E v (UnisonFile v Ann)
elaborateFile names docMap sfile = do
  -- Build data/ability declarations and resolve them into an environment (the same machinery the file parser uses).
  let (dataMap, effectMap) = splitDecls (map elabDecl (fDecls sfile))
  env <- case UFN.environmentFor names dataMap effectMap of
    Left rfs -> Left (mkResolutionErr rfs)
    Right (Left declErrs) -> Left (P.FancyError 0 (Set.singleton (P.ErrorCustom (Parser.TypeDeclarationErrors declErrs))))
    Right (Right env) -> Right env
  let baseNames = Names.shadowing (UFE.names env) names
  -- Regenerate record field accessors (get/set/modify) for any record declaration, exactly as the file parser does, so
  -- that records round-trip with their accessors. The generated terms are already fully resolved (they reference the
  -- constructor directly), so they need no name resolution; including their names below lets ordinary terms refer to
  -- them by name.
  let recordAccessors :: [(v, Ann, Term v Ann)]
      recordAccessors =
        flip foldMap (fDecls sfile) \sd -> case dFields sd of
          Just fieldNames
            | Just (ref, _) <- Map.lookup (Name.toVar (dName sd)) (UFE.datas env) ->
                generateRecordAccessors
                  Var.namespaced
                  Ann.GeneratedFrom
                  [(Name.toVar f, dAnn sd) | f <- fieldNames]
                  (Name.toVar (dName sd))
                  ref
          _ -> []
  userDefs <-
    for (fBindings sfile) \b -> do
      t <- elaborateTerm baseNames docMap (bValue b)
      let term = maybe t (\ty -> Term.ann (bAnn b) t (elaborateType ty)) (bType b)
      pure (Name.toVar (bName b), bAnn b, term)
  let rawDefs = userDefs <> recordAccessors
      fqLocalTerms = [v | (v, _, _) <- rawDefs]
      bind =
        Term.bindNames
          Name.unsafeParseVar
          Name.toVar
          (Set.fromList fqLocalTerms)
          (Names.shadowTerms (map Name.unsafeParseVar fqLocalTerms) baseNames)
  boundDefs <-
    for rawDefs \(v, a, term) -> case bind term of
      Left errs -> Left (mkResolutionErr errs)
      Right term' -> Right (v, a, term')
  -- Watch expressions (`> expr` / `test> expr`): each gets a generated variable; the body is name-resolved like terms.
  watchEntries <-
    for (zip [0 :: Int ..] (fWatches sfile)) \(i, w) -> do
      body0 <- elaborateTerm baseNames docMap (wBody w)
      body <- case bind body0 of
        Left errs -> Left (mkResolutionErr errs)
        Right b -> Right b
      pure (wKind w, (Var.unnamedTest (tShow i), wAnn w, body))
  let watches = Map.fromListWith (flip (<>)) [(k, [e]) | (k, e) <- watchEntries]
  Right
    UnisonFileId
      { fileNamespace = Nothing,
        dataDeclarationsId = UFE.datasId env,
        effectDeclarationsId = UFE.effectsId env,
        terms = Map.fromList [(v, (a, term)) | (v, a, term) <- boundDefs],
        watches = watches
      }
  where
    mkResolutionErr errs = P.FancyError 0 (Set.singleton (P.ErrorCustom (Parser.ResolutionFailures (toList errs))))

-- | Fill in the GUIDs of @unique@ declarations parsed without one (the @SUnique ""@ sentinel that dialects produce,
-- since they don't print GUIDs): reuse the codebase's existing GUID for that type name via the parsing env, or generate
-- a fresh one from its position. Mirrors the default parser's @resolveUniqueTypeGuid@, so unique types still round-trip.
-- Call this in @parseFile@ (which is monadic) before 'elaborateFile' (which is pure).
resolveDeclGuids :: (Monad m) => Parser.ParsingEnv m -> SFile -> m SFile
resolveDeclGuids env sfile = do
  decls' <- traverse resolveDecl (fDecls sfile)
  pure sfile {fDecls = decls'}
  where
    Parser.UniqueName mkName = Parser.uniqueNames env
    resolveDecl d = case dModifier d of
      SUnique "" -> do
        existing <- Parser.uniqueTypeGuid env (dName d)
        let pos = case dAnn d of Ann.Ann s _ -> s; _ -> Pos.Pos (-1) (-1)
            guid = fromMaybe (fromMaybe (tShow pos) (mkName pos 32)) existing
        pure d {dModifier = SUnique guid}
      _ -> pure d

-- | Build a (type-name, declaration) pair from a surface declaration. Constructor types are taken verbatim from the
-- IR (the parser is responsible for reconstructing each constructor's full @args -> Self tyvars@ type).
elabDecl :: (Var v) => SDecl -> (v, Either (EffectDeclaration v Ann) (DataDeclaration v Ann))
elabDecl sd =
  let tyvars = map Name.toVar (dTypeParams sd)
      ctors = [(cAnn c, Name.toVar (cName c), elaborateType (cType c)) | c <- dConstructors sd]
      modifier = case dModifier sd of SStructural -> DD.Structural; SUnique t -> DD.Unique t
      dd = DataDeclaration modifier (dAnn sd) tyvars ctors
   in ( Name.toVar (dName sd),
        if dIsAbility sd then Left (DD.mkEffectDecl' modifier (dAnn sd) tyvars ctors) else Right dd
      )

splitDecls ::
  (Ord v) =>
  [(v, Either (EffectDeclaration v Ann) (DataDeclaration v Ann))] ->
  (Map v (DataDeclaration v Ann), Map v (EffectDeclaration v Ann))
splitDecls ds =
  ( Map.fromList [(v, dd) | (v, Right dd) <- ds],
    Map.fromList [(v, ed) | (v, Left ed) <- ds]
  )

-- | Elaborate a surface term to the AST. Names are needed to resolve constructor names in patterns; @docMap@ supplies
-- the already-parsed term for each doc literal.
elaborateTerm :: forall v. (Var v) => Names -> Map Text (Term v Ann) -> STerm -> E v (Term v Ann)
elaborateTerm names docMap = go
  where
    go :: STerm -> E v (Term v Ann)
    go (STerm a f) = case f of
      SLit l -> pure (lit a l)
      SName n -> pure (Term.var a (sNameVar n))
      SApp h args -> Term.apps' <$> go h <*> traverse go args
      SBinOp n _ x y -> (\x' y' -> Term.apps' (Term.var a (sNameVar n)) [x', y']) <$> go x <*> go y
      SLam ps body -> Term.lam' a [(pAnn p, Name.toVar (pName p)) | p <- ps] <$> go body
      SLet bs body -> elabBlock a bs body
      SLetRec bs body -> elabBlock a bs body
      SIf c t e -> (\c' t' e' -> Term.iff a c' t' e') <$> go c <*> go t <*> go e
      SAnd x y -> Term.and a <$> go x <*> go y
      SOr x y -> Term.or a <$> go x <*> go y
      SMatch s cs -> Term.match a <$> go s <*> traverse elabCase cs
      SHandle h e -> Term.handle a <$> go h <*> go e
      SDelay e -> Term.delay a <$> go e
      SList xs -> Term.list a <$> traverse go xs
      STuple xs -> BuiltinDecls.tupleTerm <$> traverse go xs
      SAnn e t -> (\e' -> Term.ann a e' (elaborateType t)) <$> go e
      SHole -> pure (Term.var a (Var.named "_"))
      STermLink n -> pure (Term.var a (sNameVar n)) -- TODO: real termLink
      STypeLink n -> pure (Term.var a (sNameVar n)) -- TODO: real typeLink
      SDocLit txt -> maybe (Left (errFail "internal error: unparsed doc literal")) Right (Map.lookup txt docMap)

    -- Elaborate a block of bindings + result the same way Unison's own block parser does: build one nested letRec
    -- over all bindings, then run SCC minimization to split it into the canonical mix of non-recursive @let@s and
    -- recursive @let rec@ groups. This means a surface block need not declare whether it is recursive (Curlison and
    -- Pyson @let@ don't) — recursion is recovered structurally, matching the hash the default syntax would produce.
    elabBlock :: Ann -> [SBinding] -> STerm -> E v (Term v Ann)
    elabBlock a bs body = do
      bs' <- traverse (\b -> (\v' -> (bAnn b, Name.toVar (bName b), v')) <$> go (bValue b)) bs
      body' <- go body
      let tm = foldr (\(ba, v, t) acc -> Term.consLetRec False a (ba, v, t) acc) body' bs'
      case Components.minimize' tm of
        Left _ -> Left (errFail "duplicate names in let block")
        Right tm' -> Right tm'

    elabCase (SCase pat guard body) = do
      (spat, bvs) <- elaboratePattern names pat
      body' <- go body
      guard' <- traverse go guard
      pure (Term.MatchCase spat (ABT.absChain' bvs <$> guard') (ABT.absChain' bvs body'))

lit :: (Var v) => Ann -> SLit -> Term v Ann
lit a = \case
  SInt i -> Term.int a i
  SNat n -> Term.nat a n
  SFloat f -> Term.float a f
  SBool b -> Term.boolean a b
  SText t -> Term.text a t
  SChar c -> Term.char a c

litPat :: Ann -> SLit -> Pattern Ann
litPat a = \case
  SInt i -> Pattern.Int a i
  SNat n -> Pattern.Nat a n
  SFloat f -> Pattern.Float a f
  SBool b -> Pattern.Boolean a b
  SText t -> Pattern.Text a t
  SChar c -> Pattern.Char a c

elaboratePattern :: forall v. (Var v) => Names -> SPattern -> E v (Pattern Ann, [(Ann, v)])
elaboratePattern names = go
  where
    go :: SPattern -> E v (Pattern Ann, [(Ann, v)])
    go (SPattern a p) = case p of
      SPWild -> pure (Pattern.Unbound a, [])
      -- A bare name that resolves to a (nullary) constructor is a constructor pattern, not a variable binding —
      -- this is how brace/indent dialects spell @Red@ as a pattern. (Matches the default parser's behavior.)
      SPVar n -> case resolveCtorMaybe CT.Data (HQ.NameOnly n) of
        Just cref -> pure (Pattern.Constructor a cref [], [])
        Nothing -> pure (Pattern.Var a, [(a, Name.toVar n)])
      SPLit l -> pure (litPat a l, [])
      SPCtor sn subs -> do
        cref <- resolveCtor CT.Data sn
        (subs', bvs) <- goList subs
        pure (Pattern.Constructor a cref subs', bvs)
      SPAs n sub -> do
        (sub', bvs) <- go sub
        pure (Pattern.As a sub', (a, Name.toVar n) : bvs)
      SPList subs -> do
        (subs', bvs) <- goList subs
        pure (Pattern.SequenceLiteral a subs', bvs)
      SPSeqOp l op r -> do
        (l', lv) <- go l
        (r', rv) <- go r
        pure (Pattern.SequenceOp a l' (seqOp op) r', lv <> rv)
      SPEffectPure sub -> do
        (sub', bvs) <- go sub
        pure (Pattern.EffectPure a sub', bvs)
      SPEffect sn subs k -> do
        cref <- resolveCtor CT.Effect sn
        (subs', bvs) <- goList subs
        (k', kv) <- go k
        pure (Pattern.EffectBind a cref subs' k', bvs <> kv)

    goList :: [SPattern] -> E v ([Pattern Ann], [(Ann, v)])
    goList = \case
      [] -> pure ([], [])
      (x : xs) -> do
        (x', bv) <- go x
        (xs', bvs) <- goList xs
        pure (x' : xs', bv <> bvs)

    -- Resolve a pattern name to a constructor of the given type, suffix-aware (so @Circle@ matches @Shape.Circle@ in
    -- the codebase), matching the default parser's behavior. Returns 'Nothing' unless exactly one constructor matches.
    resolveCtorMaybe :: CT.ConstructorType -> SName -> Maybe ConstructorReference
    resolveCtorMaybe ct sn =
      case toList (Names.lookupHQPattern Names.IncludeSuffixes sn ct names) of
        [cref] -> Just cref
        _ -> Nothing

    resolveCtor :: CT.ConstructorType -> SName -> E v ConstructorReference
    resolveCtor ct sn = case resolveCtorMaybe ct sn of
      Just cref -> Right cref
      Nothing -> Left (errFail ("unknown constructor in pattern: " <> show sn))

    seqOp = \case SCons -> Pattern.Cons; SSnoc -> Pattern.Snoc; SConcat -> Pattern.Concat

-- | Elaborate a surface type to the AST. Type names become free variables (resolved later by 'Term.bindNames').
elaborateType :: (Var v) => SType -> Type v Ann
elaborateType = go
  where
    go (SType a t) = case t of
      STyVar n -> Type.var a (Name.toVar n)
      STyRef n -> Type.var a (sNameVar n)
      STyForall vs body -> Type.foralls a (map Name.toVar vs) (go body)
      STyArrow i meffs o -> case meffs of
        Nothing -> Type.arrow a (go i) (go o)
        Just es -> Type.arrow a (go i) (Type.effect a (map go es) (go o))
      STyApp f args -> Type.apps' (go f) (map go args)
      STyEffects es -> Type.effects a (map go es)

-- | Parse every doc literal in the file with the real Unison parser, mapping each doc's @{{ … }}@ source text to its
-- term. This is the only monadic step; it lets 'elaborateFile' remain pure.
--
-- A dialect renders the code embedded in docs (the @@eval@\/@@typecheck@\/example blocks) in its own surface syntax,
-- so before handing the doc text to the (Unison) doc parser we transcode those code spans back to Unison with
-- 'transcodeDocToUnison', driving the shared Doc grammar with @pTerm@ to locate them. The markup itself is
-- dialect-independent and passes through untouched.
parseDocs :: forall m v. (Monad m, Var v) => Parser.ParsingEnv m -> P.Parsec Void String STerm -> SFile -> m (Either (Parser.Err v) (Map Text (Term v Ann)))
parseDocs env pTerm sfile = do
  let texts = Set.toList (Set.fromList (collectDocTexts sfile))
  pairs <- for texts \t -> do
    r <- Parsers.parseTerm (Text.unpack (transcodeDocToUnison (Parser.names env) pTerm t)) env
    pure (((,) t) <$> r)
  pure (Map.fromList <$> sequence pairs)

-- | Rewrite a dialect doc literal's source into equivalent Unison-syntax doc source by re-rendering each embedded code
-- span (parsed with the dialect's @pTerm@, elaborated, then printed by the default Unison printer). Only the code spans
-- change; the surrounding markup is left byte-for-byte. Falls back to the original text if the doc doesn't parse (the
-- subsequent 'Parsers.parseTerm' then surfaces a normal error).
transcodeDocToUnison :: Names -> P.Parsec Void String STerm -> Text -> Text
transcodeDocToUnison names pTerm full =
  case Text.stripPrefix "{{" full of
    Nothing -> full
    Just afterOpen ->
      let s0 = Text.unpack afterOpen
          (res, spans) = runWriter (P.runParserT docP "" s0)
       in case res of
            Left _ -> full
            Right _ -> Text.pack ("{{" <> spliceAll s0 spans)
  where
    docP :: P.ParsecT (Token Err) String (Writer [(Int, Int, Text)]) ()
    -- Skip the whitespace after `{{` before the Doc grammar starts (the Unison lexer does this too); otherwise the
    -- grammar sees leading whitespace and parses an empty doc.
    docP = void (CP.space *> Doc.doc typeOrTerm codeP (P.lookAhead (void (P.chunk "}}"))))
    -- Locate one embedded code span: parse it with the dialect's term parser, record its (offset range, Unison text),
    -- then consume the delimiter the Doc grammar handed us. The Doc grammar invokes this only at code positions.
    codeP ::
      P.ParsecT (Token Err) String (Writer [(Int, Int, Text)]) () ->
      P.ParsecT (Token Err) String (Writer [(Int, Int, Text)]) ()
    codeP close = do
      -- @typecheck fenced blocks (unlike @eval) don't consume the newline after the fence before invoking us, and the
      -- dialect term parsers don't skip leading whitespace, so do it here. The recorded span starts after it.
      _ <- P.takeWhileP Nothing isSpace
      s <- P.getOffset
      rest <- P.getInput
      case P.parse (P.match pTerm) "" rest of
        Left _ -> fail "embedded dialect code did not parse"
        Right (consumed, sterm) -> do
          _ <- P.takeP Nothing (length consumed)
          e <- P.getOffset
          let uni = fromMaybe (Text.pack consumed) (renderTermUnison sterm)
          lift (tell [(s, e, uni)])
          _ <- P.takeWhileP Nothing isSpace
          close
    renderTermUnison :: STerm -> Maybe Text
    renderTermUnison sterm = case elaborateTerm names Map.empty sterm :: E Symbol (Term Symbol Ann) of
      Left _ -> Nothing
      Right tm -> Just (PP.toPlain 80 (TermPrinter.pretty ppe tm))
    ppe = PPEN.makePPE (PPEN.namer names) PPEN.dontSuffixify
    spliceAll s spans =
      foldl' (\acc (st, en, txt) -> take st acc <> Text.unpack txt <> drop en acc) s (sortOn (\(st, _, _) -> negate st) spans)

collectDocTexts :: SFile -> [Text]
collectDocTexts sfile =
  foldMap (docTextsT . bValue) (fBindings sfile) <> foldMap (docTextsT . wBody) (fWatches sfile)

docTextsT :: STerm -> [Text]
docTextsT (STerm _ f) = case f of
  SDocLit t -> [t]
  SLit _ -> []
  SName _ -> []
  SHole -> []
  STermLink _ -> []
  STypeLink _ -> []
  SApp h as -> docTextsT h <> foldMap docTextsT as
  SBinOp _ _ a b -> docTextsT a <> docTextsT b
  SLam _ b -> docTextsT b
  SLet bs b -> foldMap (docTextsT . bValue) bs <> docTextsT b
  SLetRec bs b -> foldMap (docTextsT . bValue) bs <> docTextsT b
  SIf a b c -> docTextsT a <> docTextsT b <> docTextsT c
  SAnd a b -> docTextsT a <> docTextsT b
  SOr a b -> docTextsT a <> docTextsT b
  SMatch s cs -> docTextsT s <> foldMap caseDocs cs
  SHandle a b -> docTextsT a <> docTextsT b
  SDelay a -> docTextsT a
  SList xs -> foldMap docTextsT xs
  STuple xs -> foldMap docTextsT xs
  SAnn a _ -> docTextsT a
  where
    caseDocs (SCase _ g b) = foldMap docTextsT g <> docTextsT b
