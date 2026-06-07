module Unison.Syntax.FileParser
  ( file,
  )
where

import Control.Lens
import Control.Monad.Reader (asks, local)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration (..), EffectDeclaration)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DataDeclaration.Records (generateRecordAccessors)
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.Reference (TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Syntax.DeclParser (SynDataDecl (..), SynDecl (..), SynEffectDecl (..), SynTypeAliasDecl (..), synDeclConstructors, synDeclName, synDeclsP)
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Name qualified as Name (toText, toVar, unsafeParseVar)
import Unison.Syntax.Parser
import Unison.Syntax.TermParser qualified as TermParser
import Unison.Syntax.Var qualified as Var (namespaced, namespaced2)
import Unison.Term (Term, Term2)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Type.Names qualified as Type.Names
import Unison.TypeAlias qualified
import Unison.TypeAlias.Expand qualified as TypeAlias.Expand
import Unison.UnisonFile (UnisonFile (..))
import Unison.UnisonFile.Env qualified as UF
import Unison.UnisonFile.Names qualified as UFN
import Unison.Util.List qualified as List
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind (WatchKind)
import Unison.WatchKind qualified as UF
import Prelude hiding (readFile)

resolutionFailures :: (Ord v) => [Names.ResolutionFailure Ann] -> P v m x
resolutionFailures es = P.customFailure (ResolutionFailures es)

file :: forall m v. (Monad m, Var v) => P v m (UnisonFile v Ann)
file = do
  _ <- openBlock

  -- Parse an optional directive like "namespace foo.bar"
  maybeAnnotatedNamespace :: Maybe (Ann, Name.Name) <-
    optional (reserved "namespace") >>= \case
      Nothing -> pure Nothing
      Just _ -> do
        namespace <- importRelativeWordyId <|> importRelativeSymbolyId
        void (optional semi)
        pure (Just (ann namespace, namespace.payload))
  let maybeNamespace = snd <$> maybeAnnotatedNamespace
  let maybeNamespaceVar = Name.toVar <$> maybeNamespace

  -- The file may optionally contain top-level imports,
  -- which are parsed and applied to the type decls and term stanzas
  (namesStart, imports) <- TermParser.imports <* optional semi

  -- Parse all syn decls. The namespace in the parsing environment is required here in order to avoid unique type churn.
  unNamespacedSynDecls <- local (\e -> e {maybeNamespace}) synDeclsP

  -- Sanity check: bail if there's a duplicate name among them
  unNamespacedSynDecls
    & List.map (\decl -> (L.payload (synDeclName decl), decl))
    & List.multimap
    & Map.toList
    & mapMaybe \case
      (name, decls@(_ : _ : _)) -> Just (name, map ann decls)
      _ -> Nothing
    & \case
      [] -> pure ()
      dupes -> P.customFailure (DuplicateTypeNames dupes)

  -- Apply the namespace directive (if there is one) to the decls
  let synDecls = maybe id applyNamespaceToSynDecls maybeNamespaceVar unNamespacedSynDecls

  -- Make real data/effect decls from the "syntactic" ones, and capture the
  -- file's type aliases (already cycle-checked, in dependency order).
  (dataDecls, effectDecls, fileAliases) <- synDeclsToDecls synDecls

  -- Decls and aliases can reference each other but not cyclically. We
  -- hash in two passes around 'environmentFor':
  --
  -- 1. Aliases whose bodies only reference codebase types or other
  --    aliases earlier in the list. Their refs feed into the decl env.
  -- 2. 'environmentFor' produces decl refs.
  -- 3. Remaining aliases — those whose bodies reference file decls —
  --    are hashed against the now-populated decl env.
  --
  -- A body that references something not yet available falls into phase
  -- 3 automatically; if phase 3 still can't resolve it, that surfaces
  -- the real error.
  let fileDeclNames :: Set Name.Name
      fileDeclNames =
        Set.fromList
          [ Name.unsafeParseVar v
          | v <- Map.keys dataDecls ++ Map.keys effectDecls
          ]
  let mentionsFileDecl :: Unison.TypeAlias.TypeAlias v Ann -> Bool
      mentionsFileDecl alias =
        any
          (\fv -> Set.member (Name.unsafeParseVar fv) fileDeclNames)
          (ABT.freeVars alias.body)
  let (aliasesBeforeDecls, aliasesAfterDecls) =
        List.partition (\(_v, ta) -> not (mentionsFileDecl ta)) fileAliases

  let resolveAlias accNames alias = do
        resolvedBody <-
          Type.Names.bindNames
            Name.unsafeParseVar
            Name.toVar
            (Set.fromList alias.paramNames)
            accNames
            alias.body
            & onLeft \errs -> resolutionFailures (toList errs)
        let resolvedAlias = alias {Unison.TypeAlias.body = resolvedBody}
        pure (Hashing.hashTypeAlias resolvedAlias, resolvedAlias)
  let aliasStep (accNames, acc) (v, alias) = do
        (refId, resolved) <- resolveAlias accNames alias
        let accNames' =
              Names.fromTermsAndTypes
                []
                [(Name.unsafeParseVar v, Reference.DerivedId refId)]
                <> accNames
        pure (accNames', Map.insert v (refId, resolved) acc)

  -- Phase 1: aliases that don't reference file decls.
  (envNamesAfterPhase1, aliasesPhase1) <-
    foldM aliasStep (namesStart, Map.empty) aliasesBeforeDecls

  -- Phase 2: hash decls. Their constructor types can resolve any
  -- phase-1 alias name to its ref.
  env <- do
    result <- UFN.environmentFor envNamesAfterPhase1 dataDecls effectDecls & onLeft \errs -> resolutionFailures (toList errs)
    result & onLeft \errs -> P.customFailure (TypeDeclarationErrors errs)

  -- Phase 3: aliases that reference file decls.
  let envNamesAfterPhase2 = Names.shadowing (UF.names env) envNamesAfterPhase1
  (_, aliasesPhase3) <-
    foldM aliasStep (envNamesAfterPhase2, Map.empty) aliasesAfterDecls

  let fileAliasesWithHashes :: Map v (TypeReferenceId, Unison.TypeAlias.TypeAlias v Ann)
      fileAliasesWithHashes = aliasesPhase1 <> aliasesPhase3

  let aliasNamesForDecls :: Names
      aliasNamesForDecls =
        Names.fromTermsAndTypes
          []
          [ (Name.unsafeParseVar v, Reference.DerivedId rid)
          | (v, (rid, _)) <- Map.toList fileAliasesWithHashes
          ]

  -- Generate the record accessors with *un-namespaced* names below, because we need to know these names in order to
  -- perform rewriting. As an example,
  --
  --   namespace foo
  --   type Bar = { baz : Nat }
  --   term = ... Bar.baz ...
  --
  -- we want to rename `Bar.baz` to `foo.Bar.baz`, and it seems easier to first generate un-namespaced accessors like
  -- `Bar.baz`, rather than rip off the namespace from accessors like `foo.Bar.baz` (though not by much).
  let unNamespacedAccessors :: [(v, Ann, Term v Ann)]
      unNamespacedAccessors =
        foldMap
          ( \case
              SynDecl'Data decl
                | Just fields <- decl.fields,
                  Just (ref, _) <-
                    Map.lookup (maybe id Var.namespaced2 maybeNamespaceVar decl.name.payload) (UF.datas env) ->
                    generateRecordAccessors
                      Var.namespaced
                      Ann.GeneratedFrom
                      (toPair <$> fields)
                      decl.name.payload
                      ref
              _ -> []
          )
          unNamespacedSynDecls
        where
          toPair (tok, typ) = (tok.payload, ann tok <> ann typ)

  let accessors :: [(v, Ann, Term v Ann)]
      accessors =
        unNamespacedAccessors
          & case maybeNamespaceVar of
            Nothing -> id
            Just namespace -> over (mapped . _1) (Var.namespaced2 namespace)

  -- At this stage of the file parser, we've parsed all the type and ability
  -- declarations. File-local alias names are visible during term parsing
  -- so type-position references to them resolve to alias refs.
  let envNamesWithAliases = aliasNamesForDecls <> UF.names env
  let updateEnvForTermParsing e =
        e
          { names = Names.shadowing envNamesWithAliases namesStart,
            maybeNamespace,
            localNamespacePrefixedTypesAndConstructors = envNamesWithAliases
          }
  local updateEnvForTermParsing do
    names <- asks names
    stanzas <- do
      unNamespacedStanzas0 <- sepBy semi stanza
      let unNamespacedStanzas = fmap (TermParser.substImports names imports) <$> unNamespacedStanzas0
      pure $
        unNamespacedStanzas
          & case maybeNamespaceVar of
            Nothing -> id
            Just namespace ->
              let unNamespacedTermNamespaceNames :: Set v
                  unNamespacedTermNamespaceNames =
                    Set.unions
                      [ -- The vars parsed from the stanzas themselves (before applying namespace directive)
                        Set.fromList (unNamespacedStanzas >>= getVars),
                        -- The un-namespaced constructor names (from the *originally-parsed* data and effect decls)
                        foldMap (Set.fromList . map (view _2) . synDeclConstructors) unNamespacedSynDecls,
                        -- The un-namespaced accessors
                        Set.fromList (map (view _1) unNamespacedAccessors)
                      ]
               in map (applyNamespaceToStanza namespace unNamespacedTermNamespaceNames)
    _ <- closeBlock
    let (termsr, watchesr) = foldl' go ([], []) stanzas
        go (terms, watches) s = case s of
          WatchBinding kind spanningAnn ((_, v), at) ->
            (terms, (kind, (v, spanningAnn, Term.generalizeTypeSignatures at)) : watches)
          WatchExpression kind guid spanningAnn at ->
            (terms, (kind, (Var.unnamedTest guid, spanningAnn, Term.generalizeTypeSignatures at)) : watches)
          Binding ((spanningAnn, v), at) -> ((v, spanningAnn, Term.generalizeTypeSignatures at) : terms, watches)
          Bindings bs -> ([(v, spanningAnn, Term.generalizeTypeSignatures at) | ((spanningAnn, v), at) <- bs] ++ terms, watches)
    let (terms, watches) = (reverse termsr, reverse watchesr)
        -- All locally declared term variables, running example:
        --   [foo.alice, bar.alice, zonk.bob]
        fqLocalTerms :: [v]
        fqLocalTerms = (stanzas >>= getVars) <> (view _1 <$> accessors)
    let bindNames =
          Term.bindNames
            Name.unsafeParseVar
            Name.toVar
            (Set.fromList fqLocalTerms)
            (Names.shadowTerms (map Name.unsafeParseVar fqLocalTerms) names)
    terms <- case List.validate (traverseOf _3 bindNames) terms of
      Left es -> resolutionFailures (toList es)
      Right terms -> pure terms
    watches <- case List.validate (traverseOf (traversed . _3) bindNames) watches of
      Left es -> resolutionFailures (toList es)
      Right ws -> pure ws

    validateUnisonFile
      maybeAnnotatedNamespace
      (UF.datasId env)
      (UF.effectsId env)
      fileAliasesWithHashes
      (terms <> accessors)
      (List.multimap watches)

-- | Suppose a data declaration `Foo` has a constructor `A` with fields `B` and `C`, where `B` is locally-bound and `C`
-- is not:
--
-- @
-- type B
--
-- type Foo
-- constructor Foo.A : B -> C -> Foo
-- @
--
-- Then, this function applies a namespace "namespace" to the data declaration `Foo` by prefixing each of its
-- constructors and references to locally-bound types with "namespace":
--
-- @
-- type Foo
-- constructor namespace.Foo.A : namespace.B -> C -> foo.Foo
--             ^^^^^^^^^^        ^^^^^^^^^^          ^^^^
-- @
--
-- (note that the name for the data declaration itself is not prefixed within this function, because a data declaration
-- does not contain its own name).
applyNamespaceToSynDecls :: forall v. (Var v) => v -> [SynDecl v] -> [SynDecl v]
applyNamespaceToSynDecls namespace decls =
  map
    ( \case
        SynDecl'Data decl ->
          SynDecl'Data
            ( decl
                & over (#constructors . mapped) applyToConstructor
                & over (#name . mapped) (Var.namespaced2 namespace)
            )
        SynDecl'Effect decl ->
          SynDecl'Effect
            ( decl
                & over (#constructors . mapped) applyToConstructor
                & over (#name . mapped) (Var.namespaced2 namespace)
            )
        SynDecl'TypeAlias decl ->
          SynDecl'TypeAlias
            ( decl
                & over #body (ABT.substsInheritAnnotation typeReplacements)
                & over (#name . mapped) (Var.namespaced2 namespace)
            )
        SynDecl'Opaque decl ->
          -- TODO(opaque): also substitute through body item terms; for now we
          -- only rewrite the RHS and decl name, which is enough for top-level
          -- parsing not to drop the decl. Body-item namespacing lands with
          -- the typecheck integration.
          SynDecl'Opaque
            ( decl
                & over #rhs (ABT.substsInheritAnnotation typeReplacements)
                & over (#name . mapped) (Var.namespaced2 namespace)
            )
    )
    decls
  where
    applyToConstructor :: (Ann, v, Type v Ann) -> (Ann, v, Type v Ann)
    applyToConstructor (ann, name, typ) =
      ( ann,
        Var.namespaced2 namespace name,
        ABT.substsInheritAnnotation typeReplacements typ
      )

    -- Replace var "Foo" with var "namespace.Foo"
    typeReplacements :: [(v, Type v ())]
    typeReplacements =
      decls
        & List.foldl' (\acc decl -> Set.insert (L.payload (synDeclName decl)) acc) Set.empty
        & Set.toList
        & map (\v -> (v, Type.var () (Var.namespaced2 namespace v)))

synDeclsToDecls ::
  forall m v.
  (Monad m, Var v) =>
  [SynDecl v] ->
  P
    v
    m
    ( Map v (DataDeclaration v Ann),
      Map v (EffectDeclaration v Ann),
      [(v, Unison.TypeAlias.TypeAlias v Ann)]
    )
synDeclsToDecls decls = do
  let (datasRaw, effectsRaw, aliasesRaw) = partitionDecls decls

  -- Cycle-check aliases and order them so each appears after the aliases
  -- it references — callers downstream resolve and hash in this order.
  aliases <-
    case TypeAlias.Expand.inDependencyOrder aliasesRaw of
      Right ordered -> pure ordered
      Left (TypeAlias.Expand.AliasCycle names) ->
        let anns =
              names
                & Set.toList
                & mapMaybe (\v -> (\a -> (v, a)) . ABT.annotation . Unison.TypeAlias.body <$> Map.lookup v aliasesRaw)
            firstAnn = case anns of
              ((_, a) : _) -> a
              _ -> Ann.External
         in P.customFailure (TypeAliasCycle firstAnn (map fst anns))

  let datas =
        Map.fromList
          [ (decl.name.payload, DataDeclaration decl.modifier decl.annotation decl.tyvars decl.constructors)
          | decl <- datasRaw
          ]

  let effects =
        Map.fromList
          [ (decl.name.payload, DataDeclaration.mkEffectDecl' decl.modifier decl.annotation decl.tyvars decl.constructors)
          | decl <- effectsRaw
          ]

  pure (datas, effects, aliases)

-- | Split a parsed decl list into data, effect, and alias maps.
partitionDecls ::
  (Ord v) =>
  [SynDecl v] ->
  ([SynDataDecl v], [SynEffectDecl v], Map v (Unison.TypeAlias.TypeAlias v Ann))
partitionDecls = foldr step ([], [], Map.empty)
  where
    step (SynDecl'Data d) (ds, es, as) = (d : ds, es, as)
    step (SynDecl'Effect d) (ds, es, as) = (ds, d : es, as)
    step (SynDecl'TypeAlias d) (ds, es, as) =
      ( ds,
        es,
        Map.insert
          d.name.payload
          ( Unison.TypeAlias.TypeAlias
              { Unison.TypeAlias.paramNames = d.tyvars,
                Unison.TypeAlias.body = d.body
              }
          )
          as
      )
    -- TODO(opaque): collect opaque decls and plumb them into UnisonFile.
    -- For now we silently drop them; the parser accepts the syntax but the
    -- decls are not yet visible to elaboration/codegen. Subsequent phases
    -- will collect, hash, and integrate them.
    step (SynDecl'Opaque _) (ds, es, as) = (ds, es, as)

applyNamespaceToStanza ::
  forall a v.
  (Var v) =>
  v ->
  Set v ->
  Stanza v (Term v a) ->
  Stanza v (Term v a)
applyNamespaceToStanza namespace locallyBoundTerms = \case
  Binding x -> Binding (goBinding x)
  Bindings xs -> Bindings (map goBinding xs)
  WatchBinding wk ann x -> WatchBinding wk ann (goBinding x)
  WatchExpression wk guid ann term -> WatchExpression wk guid ann (goTerm term)
  where
    goBinding :: ((Ann, v), Term v a) -> ((Ann, v), Term v a)
    goBinding ((ann, name), term) =
      ((ann, Var.namespaced2 namespace name), goTerm term)

    goTerm :: Term v a -> Term v a
    goTerm =
      ABT.substsInheritAnnotation replacements

    replacements :: [(v, Term2 v a a v ())]
    replacements =
      locallyBoundTerms
        & Set.toList
        & map (\v -> (v, Term.var () (Var.namespaced2 namespace v)))

-- | Final validations and sanity checks to perform before finishing parsing.
validateUnisonFile ::
  (Ord v) =>
  Maybe (Ann, Name.Name) ->
  Map v (TypeReferenceId, DataDeclaration v Ann) ->
  Map v (TypeReferenceId, EffectDeclaration v Ann) ->
  Map v (TypeReferenceId, Unison.TypeAlias.TypeAlias v Ann) ->
  [(v, Ann, Term v Ann)] ->
  Map WatchKind [(v, Ann, Term v Ann)] ->
  P v m (UnisonFile v Ann)
validateUnisonFile fn datas effects aliases terms watches =
  checkForDuplicateTermsAndConstructors fn datas effects aliases terms watches

-- | Because types and abilities can introduce their own constructors and fields it's difficult
-- to detect all duplicate terms during parsing itself. Here we collect all terms and
-- constructors and verify that no duplicates exist in the file, triggering an error if needed.
checkForDuplicateTermsAndConstructors ::
  forall m v.
  (Ord v) =>
  Maybe (Ann, Name.Name) ->
  Map v (TypeReferenceId, DataDeclaration v Ann) ->
  Map v (TypeReferenceId, EffectDeclaration v Ann) ->
  Map v (TypeReferenceId, Unison.TypeAlias.TypeAlias v Ann) ->
  [(v, Ann, Term v Ann)] ->
  Map WatchKind [(v, Ann, Term v Ann)] ->
  P v m (UnisonFile v Ann)
checkForDuplicateTermsAndConstructors fn datas effects aliases terms watches = do
  when (not . null $ duplicates) $ do
    let dupeList :: [(v, [Ann])]
        dupeList =
          duplicates
            & fmap Set.toList
            & Map.toList
    P.customFailure (DuplicateTermNames dupeList)
  pure
    UnisonFileId
      { fileNamespace = fn,
        dataDeclarationsId = datas,
        effectDeclarationsId = effects,
        typeAliasesId = aliases,
        -- TODO(opaque): plumbed through 'validateUnisonFile' once the parser collects opaques.
        opaqueDeclarationsId = Map.empty,
        terms = List.foldl (\acc (v, ann, term) -> Map.insert v (ann, term) acc) Map.empty terms,
        watches
      }
  where
    effectDecls :: [DataDeclaration v Ann]
    effectDecls = Map.elems . fmap (DataDeclaration.toDataDecl . snd) $ effects
    dataDecls :: [DataDeclaration v Ann]
    dataDecls = fmap snd $ Map.elems datas
    allConstructors :: [(v, Ann)]
    allConstructors =
      (dataDecls <> effectDecls)
        & foldMap DataDeclaration.constructors'
        & fmap (\(ann, v, _typ) -> (v, ann))
    allTerms :: [(v, Ann)]
    allTerms =
      map (\(v, ann, _term) -> (v, ann)) terms

    mergedTerms :: Map v (Set Ann)
    mergedTerms =
      (allConstructors <> allTerms)
        & (fmap . fmap) Set.singleton
        & Map.fromListWith Set.union
    duplicates :: Map v (Set Ann)
    duplicates =
      -- Any vars with multiple annotations are duplicates.
      Map.filter ((> 1) . Set.size) mergedTerms

-- A stanza is either a watch expression like:
--   > 1 + x
--   > z = x + 1
-- Or it is a binding like:
--   foo : Nat -> Nat
--   foo x = x + 42

data Stanza v term
  = WatchBinding UF.WatchKind Ann ((Ann, v), term)
  | WatchExpression UF.WatchKind Text Ann term
  | Binding ((Ann, v), term)
  | Bindings [((Ann, v), term)]
  deriving (Foldable, Traversable, Functor)

getVars :: (Var v) => Stanza v term -> [v]
getVars = \case
  WatchBinding _ _ ((_, v), _) -> [v]
  WatchExpression _ guid _ _ -> [Var.unnamedTest guid]
  Binding ((_, v), _) -> [v]
  Bindings bs -> [v | ((_, v), _) <- bs]

stanza :: (Monad m, Var v) => P v m (Stanza v (Term v Ann))
stanza = watchExpression <|> unexpectedAction <|> binding
  where
    unexpectedAction = failureIf (TermParser.blockTerm $> getErr) binding
    getErr = do
      t <- anyToken
      t2 <- optional anyToken
      P.customFailure $ DidntExpectExpression t t2
    watchExpression = do
      (kind, guid, ann) <- watched
      _ <- guardEmptyWatch ann
      msum
        [ TermParser.binding <&> (\trm@(((trmSpanAnn, _), _)) -> WatchBinding kind (ann <> trmSpanAnn) trm),
          TermParser.blockTerm <&> (\trm -> WatchExpression kind guid (ann <> ABT.annotation trm) trm)
        ]

    guardEmptyWatch ann =
      P.try $ do
        op <- optional (L.payload <$> P.lookAhead closeBlock)
        case op of
          Just () -> P.customFailure (EmptyWatch ann)
          _ -> pure ()

    -- binding :: forall v. Var v => P v ((Ann, v), Term v Ann)
    binding = do
      -- this logic converts
      --   {{ A doc }}  to   foo.doc = {{ A doc }}
      --   foo = 42          foo = 42
      doc <- P.optional (TermParser.doc2Block <* semi)
      binding@((_, v), _) <- TermParser.binding
      pure $ case doc of
        Nothing -> Binding binding
        Just (spanAnn, doc) -> Bindings [((spanAnn, Var.namespaced2 v (Var.named "doc")), doc), binding]

watched :: (Monad m, Var v) => P v m (UF.WatchKind, Text, Ann)
watched = P.try do
  kind <- (fmap . fmap . fmap) (Text.unpack . Name.toText) (optional importRelativeWordyId)
  guid <- uniqueName 10
  op <- optional (L.payload <$> P.lookAhead importRelativeSymbolyId)
  guard (op == Just (Name.fromSegment NameSegment.watchSegment))
  tok <- anyToken
  guard $ maybe True (`L.touches` tok) kind
  pure (maybe UF.RegularWatch L.payload kind, guid, maybe mempty ann kind <> ann tok)
