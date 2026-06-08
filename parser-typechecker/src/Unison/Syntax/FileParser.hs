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
import Unison.OpaqueDeclaration (OpaqueBody (..), OpaqueDeclaration (..))
import Unison.OpaqueDeclaration qualified as OpaqueDeclaration
import Unison.OpaqueDeclaration.Expand qualified as OpaqueDeclaration.Expand
import Unison.Syntax.DeclParser (SynDataDecl (..), SynDecl (..), SynEffectDecl (..), SynOpaqueBody (..), SynOpaqueDecl (..), SynTypeAliasDecl (..), synDeclConstructors, synDeclName, synDeclsP)
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
  -- file's type aliases (already cycle-checked, in dependency order) plus
  -- the file's opaque-type decls (un-hashed; resolved + hashed below once we
  -- have the alias env).
  (dataDecls, effectDecls, fileAliases, fileOpaques) <- synDeclsToDecls synDecls

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

  -- Convert each parsed @SynOpaqueDecl@ to a core 'OpaqueDeclaration' and
  -- compute its hash. Opaques may reference file decls, aliases, and other
  -- opaques in their RHS, so we (a) order them by dependency, (b) accumulate
  -- each opaque's name into the resolution env as we go, and (c) raise
  -- 'OpaqueDeclCycle' on self-reference or cross-opaque cycles.
  let opaquesAsMap :: Map v (SynOpaqueDecl v)
      opaquesAsMap = Map.fromList fileOpaques
      -- A stripped-down OpaqueDeclaration carrying just the fields needed
      -- for dependency ordering. The body is irrelevant — only the RHS
      -- contributes to type-level deps — and the modifier doesn't matter
      -- here either.
      opaquesForOrdering :: Map v (OpaqueDeclaration v Ann)
      opaquesForOrdering =
        opaquesAsMap
          & Map.map \sd ->
            OpaqueDeclaration
              { OpaqueDeclaration.modifier = sd.modifier,
                OpaqueDeclaration.annotation = sd.annotation,
                OpaqueDeclaration.paramNames = sd.tyvars,
                OpaqueDeclaration.rhs = sd.rhs,
                OpaqueDeclaration.body = []
              }
  orderedOpaques :: [(v, SynOpaqueDecl v)] <-
    case OpaqueDeclaration.Expand.inDependencyOrder opaquesForOrdering of
      Right ordered -> pure [(v, opaquesAsMap Map.! v) | (v, _) <- ordered]
      Left (OpaqueDeclaration.Expand.OpaqueCycle names) ->
        let anns =
              names
                & Set.toList
                & mapMaybe (\v -> (\sd -> (v, sd.annotation)) <$> Map.lookup v opaquesAsMap)
            firstAnn = case anns of
              ((_, a) : _) -> a
              _ -> Ann.External
         in P.customFailure (OpaqueDeclCycle firstAnn (map fst anns))

  let envNamesForOpaquesStart :: Names
      envNamesForOpaquesStart =
        Names.shadowing (aliasNamesForDecls <> UF.names env) envNamesAfterPhase1
  let resolveOpaque ::
        Names ->
        SynOpaqueDecl v ->
        P v m (TypeReferenceId, OpaqueDeclaration v Ann)
      resolveOpaque accNames sd = do
        resolvedRhs <-
          Type.Names.bindNames
            Name.unsafeParseVar
            Name.toVar
            (Set.fromList sd.tyvars)
            accNames
            sd.rhs
            & onLeft \errs -> resolutionFailures (toList errs)
        -- Prefix each body item's name with the opaque type's (now fully-
        -- qualified) name so that body fns surface as ordinary top-level
        -- terms named e.g. @Logarithm.fromFloat@. Substitute through each
        -- body term so intra-body references (one body fn calling another
        -- by its short name) target the qualified sibling. This mirrors
        -- how 'applyNamespaceToStanza' rewrites sibling references in
        -- ordinary term stanzas.
        let opaqueName :: v
            opaqueName = sd.name.payload
            bodyVarReplacements :: [(v, Term2 v Ann Ann v ())]
            bodyVarReplacements =
              [ (b.name, Term.var () (Var.namespaced2 opaqueName b.name))
              | b <- sd.body
              ]
            bodyItems =
              [ OpaqueBody
                  { OpaqueDeclaration.name = Var.namespaced2 opaqueName b.name,
                    OpaqueDeclaration.nameAnn = b.nameAnn,
                    OpaqueDeclaration.term =
                      ABT.substsInheritAnnotation bodyVarReplacements b.term
                  }
              | b <- sd.body
              ]
        let opaque =
              OpaqueDeclaration
                { OpaqueDeclaration.modifier = sd.modifier,
                  OpaqueDeclaration.annotation = sd.annotation,
                  OpaqueDeclaration.paramNames = sd.tyvars,
                  OpaqueDeclaration.rhs = resolvedRhs,
                  OpaqueDeclaration.body = bodyItems
                }
        pure (Hashing.hashOpaqueDeclaration opaque, opaque)
      opaqueStep ::
        (Names, Map v (TypeReferenceId, OpaqueDeclaration v Ann)) ->
        (v, SynOpaqueDecl v) ->
        P v m (Names, Map v (TypeReferenceId, OpaqueDeclaration v Ann))
      opaqueStep (accNames, acc) (v, sd) = do
        (refId, resolved) <- resolveOpaque accNames sd
        let accNames' =
              Names.fromTermsAndTypes
                []
                [(Name.unsafeParseVar v, Reference.DerivedId refId)]
                <> accNames
        pure (accNames', Map.insert v (refId, resolved) acc)
  (_, fileOpaquesWithHashes :: Map v (TypeReferenceId, OpaqueDeclaration v Ann)) <-
    foldM opaqueStep (envNamesForOpaquesStart, Map.empty) orderedOpaques

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

  let opaqueNamesForDecls :: Names
      opaqueNamesForDecls =
        Names.fromTermsAndTypes
          []
          [ (Name.unsafeParseVar v, Reference.DerivedId rid)
          | (v, (rid, _)) <- Map.toList fileOpaquesWithHashes
          ]

  -- At this stage of the file parser, we've parsed all the type and ability
  -- declarations. File-local alias and opaque names are visible during term
  -- parsing so type-position references to them resolve to alias / opaque
  -- refs.
  let envNamesWithAliases = aliasNamesForDecls <> opaqueNamesForDecls <> UF.names env
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
        -- Opaque-decl body items surface as ordinary top-level terms during
        -- typechecking. Their names are already fully qualified (set in
        -- 'resolveOpaque'), so they slot directly into 'fqLocalTerms' and
        -- get 'bindNames'-resolved alongside the file's other terms.
        opaqueBodyTermsByOpaque :: [(v, [(v, Ann, Term v Ann)])]
        opaqueBodyTermsByOpaque =
          [ ( opaqueVar,
              [ (b.name, b.nameAnn, Term.generalizeTypeSignatures b.term)
              | b <- od.body
              ]
            )
          | (opaqueVar, (_, od)) <- Map.toList fileOpaquesWithHashes
          ]
        opaqueBodyTerms :: [(v, Ann, Term v Ann)]
        opaqueBodyTerms = concatMap snd opaqueBodyTermsByOpaque
        -- All locally declared term variables, running example:
        --   [foo.alice, bar.alice, zonk.bob]
        fqLocalTerms :: [v]
        fqLocalTerms =
          (stanzas >>= getVars)
            <> (view _1 <$> accessors)
            <> (view _1 <$> opaqueBodyTerms)
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
    -- Resolve free vars in opaque body terms, then write the resolved terms
    -- back into the OpaqueDeclaration.body lists.
    boundOpaqueBodyTermsByOpaque ::
      [(v, [(v, Ann, Term v Ann)])] <-
      forM opaqueBodyTermsByOpaque \(opaqueVar, bs) -> do
        bs' <- case List.validate (traverseOf _3 bindNames) bs of
          Left es -> resolutionFailures (toList es)
          Right xs -> pure xs
        pure (opaqueVar, bs')
    let fileOpaquesWithBoundBodies ::
          Map v (TypeReferenceId, OpaqueDeclaration v Ann)
        fileOpaquesWithBoundBodies =
          List.foldl'
            ( \acc (opaqueVar, bs) ->
                Map.adjust
                  ( \(rid, od) ->
                      ( rid,
                        od
                          { OpaqueDeclaration.body =
                              [ OpaqueBody
                                  { OpaqueDeclaration.name = v',
                                    OpaqueDeclaration.nameAnn = a',
                                    OpaqueDeclaration.term = tm'
                                  }
                              | (v', a', tm') <- bs
                              ]
                          }
                      )
                  )
                  opaqueVar
                  acc
            )
            fileOpaquesWithHashes
            boundOpaqueBodyTermsByOpaque

    validateUnisonFile
      maybeAnnotatedNamespace
      (UF.datasId env)
      (UF.effectsId env)
      fileAliasesWithHashes
      fileOpaquesWithBoundBodies
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
          -- Rewrite the RHS, the decl name, and any type-position references
          -- inside body item terms. Body-item *name* qualification (prefixing
          -- each body name with the opaque type's name) and intra-body
          -- term-level substitution happen unconditionally in the
          -- 'SynOpaqueDecl' → 'OpaqueDeclaration' conversion below, so we
          -- skip them here to avoid double-prefixing.
          SynDecl'Opaque
            ( decl
                & over #rhs (ABT.substsInheritAnnotation typeReplacements)
                & over (#body . mapped . #term) (Term.typeMap (ABT.substsInheritAnnotation typeReplacements))
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
      [(v, Unison.TypeAlias.TypeAlias v Ann)],
      [(v, SynOpaqueDecl v)]
    )
synDeclsToDecls decls = do
  let (datasRaw, effectsRaw, aliasesRaw, opaquesRaw) = partitionDecls decls

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

  pure (datas, effects, aliases, opaquesRaw)

-- | Split a parsed decl list into data, effect, alias, and opaque parts.
partitionDecls ::
  (Ord v) =>
  [SynDecl v] ->
  ( [SynDataDecl v],
    [SynEffectDecl v],
    Map v (Unison.TypeAlias.TypeAlias v Ann),
    [(v, SynOpaqueDecl v)]
  )
partitionDecls = foldr step ([], [], Map.empty, [])
  where
    step (SynDecl'Data d) (ds, es, as, os) = (d : ds, es, as, os)
    step (SynDecl'Effect d) (ds, es, as, os) = (ds, d : es, as, os)
    step (SynDecl'TypeAlias d) (ds, es, as, os) =
      ( ds,
        es,
        Map.insert
          d.name.payload
          ( Unison.TypeAlias.TypeAlias
              { Unison.TypeAlias.paramNames = d.tyvars,
                Unison.TypeAlias.body = d.body
              }
          )
          as,
        os
      )
    step (SynDecl'Opaque d) (ds, es, as, os) =
      (ds, es, as, (d.name.payload, d) : os)

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
  Map v (TypeReferenceId, OpaqueDeclaration v Ann) ->
  [(v, Ann, Term v Ann)] ->
  Map WatchKind [(v, Ann, Term v Ann)] ->
  P v m (UnisonFile v Ann)
validateUnisonFile fn datas effects aliases opaques terms watches =
  checkForDuplicateTermsAndConstructors fn datas effects aliases opaques terms watches

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
  Map v (TypeReferenceId, OpaqueDeclaration v Ann) ->
  [(v, Ann, Term v Ann)] ->
  Map WatchKind [(v, Ann, Term v Ann)] ->
  P v m (UnisonFile v Ann)
checkForDuplicateTermsAndConstructors fn datas effects aliases opaques terms watches = do
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
        opaqueDeclarationsId = opaques,
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
