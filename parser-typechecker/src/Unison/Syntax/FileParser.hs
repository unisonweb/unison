module Unison.Syntax.FileParser
  ( file,
  )
where

import Control.Lens
import Control.Monad.Reader (asks, local)
import Data.Foldable (foldlM)
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration (..), EffectDeclaration)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DataDeclaration.Records (RecordKind (..), generateRecordAccessors)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.Reference (TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Syntax.DeclParser (SynDataDecl (..), SynDecl (..), SynEffectDecl (..), synDeclConstructors, synDeclName, synDeclsP)
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Name qualified as Name (toText, toVar, unsafeParseVar)
import Unison.Syntax.Parser
import Unison.Syntax.TermParser qualified as TermParser
import Unison.Syntax.Var qualified as Var (namespaced, namespaced2)
import Unison.Term (Term, Term2)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
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

  -- Compute an environment from the decls that we use to parse terms
  env <- do
    -- Make real data/effect decls from the "syntactic" ones
    (dataDecls, effectDecls) <- synDeclsToDecls synDecls
    result <- UFN.environmentFor namesStart dataDecls effectDecls & onLeft \errs -> resolutionFailures (toList errs)
    result & onLeft \errs -> P.customFailure (TypeDeclarationErrors errs)

  -- Generate the record accessors with *un-namespaced* names below, because we need to know these names in order to
  -- perform rewriting. As an example,
  --
  --   namespace foo
  --   type Bar = { baz : Nat }
  --   term = ... Bar.baz ...
  --
  -- we want to rename `Bar.baz` to `foo.Bar.baz`, and it seems easier to first generate un-namespaced accessors like
  -- `Bar.baz`, rather than rip off the namespace from accessors like `foo.Bar.baz` (though not by much).
  --
  -- For @class@ declarations we additionally wrap each accessor in a
  -- 'Term.Ann' that declares the dictionary parameter as implicit
  -- (an @=>@ arrow). The typechecker's @=>I@ rule then binds the
  -- getter's leading lambda variable as a /lexical given/ for the
  -- body, so a method call like @Monoid.op acc x@ resolves the
  -- dictionary against any enclosing @=>@-parameter or namespace
  -- given. See 'classAccessorType' below and the @=>I@ clause in
  -- @Unison.Typechecker.Context.checkWanted@.
  let unNamespacedAccessors :: [(v, Ann, Term v Ann)]
      unNamespacedAccessors =
        foldMap
          ( \case
              SynDecl'Data decl
                | Just fields <- decl.fields,
                  Just (ref, dataDecl) <-
                    Map.lookup (maybe id Var.namespaced2 maybeNamespaceVar decl.name.payload) (UF.datas env) ->
                    -- Pair each parsed field with its corresponding
                    -- type from the /resolved/ constructor — the
                    -- parsed type still has user-level type names
                    -- that haven't been bound to namespace references
                    -- yet, so checking the accessor against an
                    -- annotation built from parsed types fails for
                    -- any field referencing another type. Records
                    -- have exactly one constructor whose type is
                    -- @forall tyvars. T1 -> T2 -> … -> Tn -> Self@;
                    -- the inputs are the field types in declaration
                    -- order.
                    let kind = if decl.isClass then ClassRecord else TypeRecord
                        resolvedFieldTypes = resolveFieldTypes dataDecl fields
                        rawAccessors =
                          generateRecordAccessors
                            kind
                            Var.namespaced
                            Ann.GeneratedFrom
                            (toTriple <$> resolvedFieldTypes)
                            decl.tyvars
                            decl.name.payload
                            ref
                     in case kind of
                          TypeRecord -> rawAccessors
                          ClassRecord ->
                            -- Re-annotate the (getter-only) class
                            -- accessor with the class's @=>@-bearing
                            -- type so the dictionary is threaded
                            -- implicitly by the resolver.
                            map
                              ( annotateClassAccessor
                                  decl.name.payload
                                  decl.tyvars
                                  ref
                                  resolvedFieldTypes
                              )
                              rawAccessors
              _ -> []
          )
          unNamespacedSynDecls
        where
          toTriple (tok, typ) = (tok.payload, ann tok <> ann typ, typ)

  let accessors :: [(v, Ann, Term v Ann)]
      accessors =
        unNamespacedAccessors
          & case maybeNamespaceVar of
            Nothing -> id
            Just namespace -> over (mapped . _1) (Var.namespaced2 namespace)

  -- At this stage of the file parser, we've parsed all the type and ability
  -- declarations.
  let updateEnvForTermParsing e =
        e
          { names = Names.shadowing (UF.names env) namesStart,
            maybeNamespace,
            localNamespacePrefixedTypesAndConstructors = UF.names env
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

synDeclsToDecls :: (Monad m, Var v) => [SynDecl v] -> P v m (Map v (DataDeclaration v Ann), Map v (EffectDeclaration v Ann))
synDeclsToDecls = do
  foldlM
    ( \(datas, effects) -> \case
        SynDecl'Data decl -> do
          let decl1 = DataDeclaration decl.modifier decl.annotation decl.tyvars decl.constructors
          let !datas1 = Map.insert decl.name.payload decl1 datas
          pure (datas1, effects)
        SynDecl'Effect decl -> do
          let decl1 = DataDeclaration.mkEffectDecl' decl.modifier decl.annotation decl.tyvars decl.constructors
          let !effects1 = Map.insert decl.name.payload decl1 effects
          pure (datas, effects1)
    )
    (Map.empty, Map.empty)

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

-- | Pair each parsed field name with its corresponding type as it
-- appears in the /resolved/ data declaration's constructor — that
-- form has every type reference bound to the namespace (e.g.
-- @Text@ → its builtin reference), which is what the typechecker
-- needs when validating an accessor's body against the generated
-- type annotation. Records have exactly one constructor and its
-- declared type is @forall tyvars. T1 -> T2 -> … -> Tn -> Self@;
-- the inputs of that arrow chain are the field types in the order
-- the record syntax declared them.
resolveFieldTypes ::
  forall v.
  (Var v) =>
  DataDeclaration v Ann ->
  [(L.Token v, Type v Ann)] ->
  [(L.Token v, Type v Ann)]
resolveFieldTypes dataDecl fields =
  case DataDeclaration.constructors' dataDecl of
    [(_, _, ctorType)] ->
      let inputs = peelInputs (snd (Type.unforall' ctorType))
       in zipWith (\(tok, _) ty -> (tok, ty)) fields inputs
    _ -> fields
  where
    peelInputs ty = case ty of
      Type.Arrow' i o -> i : peelInputs o
      _ -> []

-- | Final validations and sanity checks to perform before finishing parsing.
validateUnisonFile ::
  (Monad m, Ord v) =>
  Maybe (Ann, Name.Name) ->
  Map v (TypeReferenceId, DataDeclaration v Ann) ->
  Map v (TypeReferenceId, EffectDeclaration v Ann) ->
  [(v, Ann, Term v Ann)] ->
  Map WatchKind [(v, Ann, Term v Ann)] ->
  P v m (UnisonFile v Ann)
validateUnisonFile fn datas effects terms watches =
  checkForDuplicateTermsAndConstructors fn datas effects terms watches

-- | Because types and abilities can introduce their own constructors and fields it's difficult
-- to detect all duplicate terms during parsing itself. Here we collect all terms and
-- constructors and verify that no duplicates exist in the file, triggering an error if needed.
checkForDuplicateTermsAndConstructors ::
  forall m v.
  (Monad m, Ord v) =>
  Maybe (Ann, Name.Name) ->
  Map v (TypeReferenceId, DataDeclaration v Ann) ->
  Map v (TypeReferenceId, EffectDeclaration v Ann) ->
  [(v, Ann, Term v Ann)] ->
  Map WatchKind [(v, Ann, Term v Ann)] ->
  P v m (UnisonFile v Ann)
checkForDuplicateTermsAndConstructors fn datas effects terms watches = do
  when (not . null $ duplicates) $ do
    let dupeList :: [(v, [Ann])]
        dupeList =
          duplicates
            & fmap Set.toList
            & Map.toList
    P.customFailure (DuplicateTermNames dupeList)
  -- Pull the parser-side @given@-keyword names out of the parser's
  -- state side channel and stamp them onto the 'UnisonFile' so the
  -- typechecker can recognise @given@ origins by name. Includes both
  -- file-level @given@ decls and @let given@ bindings nested inside
  -- term bodies (both invoke 'givenBindingBody' which calls
  -- 'recordGivenVar').
  gbs <- getGivenVars
  -- Also pull the @class@-keyword names so the @add@/@update@ command
  -- can mark them in namespace metadata via
  -- 'Unison.Codebase.Classes.markClassAt'. Without this the
  -- declaration round-trips through @view@ as a plain @type@.
  cbs <- getClassDeclVars
  pure
    UnisonFileId
      { fileNamespace = fn,
        dataDeclarationsId = datas,
        effectDeclarationsId = effects,
        terms = List.foldl (\acc (v, ann, term) -> Map.insert v (ann, term) acc) Map.empty terms,
        watches,
        givenBindings = gbs,
        classBindings = cbs
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

-- | Wrap a @class@ accessor term with a 'Term.Ann' that declares the
-- dictionary parameter as implicit. The accessor's getter body is
-- already a lambda taking the dictionary; this annotation pins the
-- leading parameter as an @=>@ arrow so the typechecker's @=>I@ rule
-- registers the binder as a lexical given for the body.
--
-- The resulting type is
-- @
--   forall tyvars... . T tyvars... => FieldType
-- @
-- where @T@ is the class type, @tyvars@ are its parameters, and
-- @FieldType@ is the field's declared type. The @FieldType@ is
-- copied verbatim from the field declaration, so existing type
-- variables continue to bind to the class's @forall@.
annotateClassAccessor ::
  forall v.
  (Var v) =>
  -- | Class type name (e.g. @Monoid@).
  v ->
  -- | Class type parameters (e.g. @[m]@).
  [v] ->
  -- | Reference of the class type, used to construct @T tyvars@.
  Reference.Reference ->
  -- | Class field declarations: @[(name, fieldType)]@.
  [(L.Token v, Type v Ann)] ->
  -- | The raw accessor produced by 'generateRecordAccessors'.
  (v, Ann, Term v Ann) ->
  (v, Ann, Term v Ann)
annotateClassAccessor className tyvars classRef classFields (vname, a, body) =
  case List.find (\(tok, _) -> matchesField (L.payload tok)) classFields of
    Nothing -> (vname, a, body)
    Just (_, fieldType) ->
      let classRefT = Type.ref a classRef
          classApp = foldl' (\acc tyv -> Type.app a acc (Type.var a tyv)) classRefT tyvars
          implicit = Type.implicitArrow a classApp fieldType
          quantified = Type.foralls a tyvars implicit
       in (vname, a, Term.ann a body quantified)
  where
    -- The accessor's variable name is @ClassName.fieldName@. Pull
    -- out the field name and match against the declaration's tokens.
    matchesField :: v -> Bool
    matchesField fieldName =
      let target = qualifyField className fieldName
       in vname == target

    qualifyField :: v -> v -> v
    qualifyField cn fn = Var.namespaced (cn :| [fn])

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
