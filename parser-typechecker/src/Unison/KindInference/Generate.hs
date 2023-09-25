module Unison.KindInference.Generate
  ( typeConstraints,
    termConstraints,
    declComponentConstraints,
    builtinConstraints,
  )
where

import Data.Foldable (foldlM)
import Data.Set qualified as Set
import U.Core.ABT qualified as ABT
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation (builtinAnnotation))
import Unison.DataDeclaration (Decl, asDataDecl)
import Unison.DataDeclaration qualified as DD
import Unison.KindInference.Constraint.Context (ConstraintContext (..))
import Unison.KindInference.Constraint.Provenance (Provenance (..))
import Unison.KindInference.Constraint.Unsolved (Constraint (..))
import Unison.KindInference.Generate.Monad (Gen, addConstraint, freshVar, insertType, lookupType, scopedType)
import Unison.KindInference.UVar (UVar)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Var

-- | Generate kind constraints arising from a given type. The given
-- @UVar@ is constrained to have the kind of the given type.
typeConstraints :: (Var v, Ord loc) => UVar v loc -> Type.Type v loc -> Gen v loc ()
typeConstraints resultVar term@ABT.Term {annotation, out} = do
  case out of
    ABT.Abs _ _ -> error "impossible? Abs" -- todo
    ABT.Var v ->
      lookupType (Type.var annotation v) >>= \case
        Nothing -> error "var nothing"
        Just x -> addConstraint (Unify (Provenance ContextLookup annotation) resultVar x)
    ABT.Cycle _ -> error "Cycle" -- todo
    ABT.Tm t0 -> case t0 of
      Type.Arrow dom cod -> do
        let ctx = AppArrow annotation dom cod
        addConstraint (IsStar resultVar (Provenance ctx annotation))
        k1 <- freshVar dom
        typeConstraints k1 dom
        addConstraint (IsStar k1 (Provenance ctx $ ABT.annotation dom))
        k2 <- freshVar cod
        typeConstraints k2 cod
        addConstraint (IsStar k2 (Provenance ctx $ ABT.annotation cod))
      Type.App abs arg -> do
        absVar <- freshVar abs
        absArgVar <- freshVar arg
        typeConstraints absVar abs
        argVar <- freshVar arg
        typeConstraints argVar arg
        addConstraint (IsArr absVar (Provenance (AppAbs absVar absArgVar) (ABT.annotation abs)) absArgVar resultVar)
        addConstraint (Unify (Provenance (AppArg absVar absArgVar argVar) (ABT.annotation arg)) absArgVar argVar)
      Type.Forall ABT.Term {annotation, out} -> case out of
        ABT.Abs v x ->
          scopedType (Type.var annotation v) \_ -> do
            typeConstraints resultVar x
        _ -> error "impossible? Forall wrapping a non-abs"
      Type.IntroOuter ABT.Term {annotation, out} -> case out of
        ABT.Abs v x -> handleIntroOuter v annotation (typeConstraints resultVar x)
        _ -> error "impossible? IntroOuter wrapping a non-abs"
      Type.Ann x _kind -> typeConstraints resultVar x -- todo
      Type.Ref r ->
        lookupType (Type.ref annotation r) >>= \case
          Nothing -> error ("[ref nothing] " <> show term)
          Just x -> addConstraint (Unify (Provenance ContextLookup annotation) resultVar x)
      Type.Effect effTyp b -> do
        effKind <- freshVar effTyp
        typeConstraints effKind effTyp
        typeConstraints resultVar b
      Type.Effects effs -> do
        for_ effs \eff -> do
          effKind <- freshVar eff
          typeConstraints effKind eff
          addConstraint (IsEffect effKind (Provenance EffectsList $ ABT.annotation eff))

handleIntroOuter :: Var v => v -> loc -> Gen v loc r -> Gen v loc r
handleIntroOuter v loc k = do
  let typ = Type.var loc v
  new <- freshVar typ
  orig <-
    lookupType typ >>= \case
      Nothing -> error "[malformed type] IntroOuter not in lexical scope of matching Forall"
      Just a -> pure a
  res <- k
  addConstraint (Unify (Provenance ScopeReference loc) new orig)
  pure res

-- | Helper for @termConstraints@ that instantiates the outermost
-- foralls and keeps the type in scope (in the type map) while
-- checking lexically nested type annotations.
instantiateType ::
  forall v loc r.
  (Var v, Ord loc) =>
  Type.Type v loc ->
  (Type.Type v loc -> Gen v loc r) ->
  Gen v loc r
instantiateType type0 k =
  let go = \case
        ABT.Tm' (Type.Forall ABT.Term {annotation, out = ABT.Abs x t}) -> do
          scopedType (Type.var annotation x) \_ -> do
            go t
        ABT.Tm' (Type.IntroOuter ABT.Term {annotation, out = ABT.Abs x t}) -> do
          handleIntroOuter x annotation (go t)
        t -> k t
   in go type0

-- | Check that all annotations in a term are well-kinded
termConstraints :: (Var v, Ord loc) => Term.Term v loc -> Gen v loc ()
termConstraints = dfAnns processAnn cons nil
  where
    processAnn ann typ rest = do
      instantiateType typ \typ -> do
        typKind <- freshVar typ
        typeConstraints typKind typ
        addConstraint (IsStar typKind (Provenance TypeAnnotation ann))
        rest
    cons = (>>)
    nil = pure ()

-- | Process type annotations depth-first. Allows processing
-- annotations with lexical scoping.
dfAnns :: (loc -> Type.Type v loc -> b -> b) -> (b -> b -> b) -> b -> Term.Term v loc -> b
dfAnns annAlg cons nil = ABT.cata \ann abt0 -> case abt0 of
  ABT.Var _ -> nil
  ABT.Cycle x -> x
  ABT.Abs _ x -> x
  ABT.Tm t -> case t of
    Term.Ann trm typ -> annAlg ann typ trm
    x -> foldr cons nil x

-- | Generate kind constraints for a mutally recursive component of
-- decls
declComponentConstraints ::
  forall v loc.
  (Var v, Ord loc) =>
  [(Reference, Decl v loc)] ->
  Gen v loc ()
declComponentConstraints decls = do
  decls <- for decls \(ref, decl) -> do
    -- Add a kind variable for every datatype
    declKind <- insertType (Type.ref (DD.annotation $ asDataDecl decl) ref)
    pure (ref, decl, declKind)
  for_ decls \(ref, decl, declKind) -> do
    let declAnn = DD.annotation $ asDataDecl decl
    -- todo: cleanup
    let declType = Type.ref declAnn ref
    -- Unify the datatype with @k_1 -> ... -> k_n -> *@ where @n@ is
    -- the number of type parameters
    let tyVars = map (\tyVar -> Type.var declAnn tyVar) (DD.bound $ asDataDecl decl)
    tyvarKinds <- for tyVars \tyVar -> do
      -- it would be nice to annotate these type vars with their
      -- precise location, but that information doesn't seem to be
      -- available via "DataDeclaration", so we currently settle for
      -- the whole decl annotation.
      k <- freshVar tyVar
      pure (k, tyVar)
    (fullyAppliedKind, _fullyAppliedType) <-
      let phi (dk, dt) (ak, at) = do
            -- introduce a kind uvar for each app node
            let t' = Type.app declAnn dt at
            v <- freshVar t'
            addConstraint (IsArr dk (Provenance DeclDefinition declAnn) ak v)
            pure (v, t')
       in foldlM phi (declKind, declType) tyvarKinds

    case decl of
      Left _effectDecl -> addConstraint (IsEffect fullyAppliedKind (Provenance DeclDefinition declAnn))
      Right _dataDecl -> addConstraint (IsStar fullyAppliedKind (Provenance DeclDefinition declAnn))

    for_ (DD.constructors' $ asDataDecl decl) \(constructorAnn, _, constructorType) -> do
      withInstantiatedConstructorType declType (map fst tyvarKinds) constructorType \constructorType -> do
        constructorKind <- freshVar constructorType
        typeConstraints constructorKind constructorType
        addConstraint (IsStar constructorKind (Provenance DeclDefinition constructorAnn))

-- | This is a helper to unify the kind constraints on type variables
-- across a decl's constructors.
--
-- With a decl like
--
-- @
-- unique type T a = C0 Nat a | C1 (a Nat)
-- @
--
-- @C0@ will have type @forall a. Nat -> a -> T a@ and @C1@ will have
-- type @forall a. (a Nat) -> T a@. We must unify the kind constraints
-- that the two constructors make on @a@ in order to determine the
-- kind of @a@ (or observe that there are contradictory
-- constraints). In this example @C0@ constrains @a@ to be of type *
-- because it is applied to the arrow type, whereas @C1@ constrains
-- @a@ to be of kind * -> * since it is applied to a Nat.
--
-- We unify these variables by instantiating the outermost foralls
-- with fresh kind variables, then follow any arrows to find the
-- result type which must have type @T b@ for some b, then unify @b@
-- with some kind variable representing the unification of @a@ for
-- each constructor.
withInstantiatedConstructorType ::
  forall v loc r.
  (Var v, Ord loc) =>
  Type.Type v loc ->
  [UVar v loc] ->
  Type.Type v loc ->
  (Type.Type v loc -> Gen v loc r) ->
  Gen v loc r
withInstantiatedConstructorType declType tyParams0 constructorType0 k =
  let goForall constructorType = case constructorType of
        ABT.Tm' (Type.Forall ABT.Term {annotation, out = ABT.Abs x t}) -> do
          scopedType (Type.var annotation x) \_ -> do
            goForall t
        _ -> do
          goArrow constructorType
          k constructorType

      goArrow :: Type.Type v loc -> Gen v loc ()
      goArrow = \case
        Type.Arrow' _ o -> goArrow o
        Type.Effect' es _ -> goEffs es
        resultTyp@(Type.Apps' f xs)
          | f == declType -> unifyVars resultTyp xs
        f | f == declType -> pure ()
        resultTyp -> error ("[goArrow] unexpected result type: " <> show resultTyp)

      goEffs = \case
        [] -> error ("[goEffs] couldn't find the expected ability: " <> show declType)
        e : es
          | e == declType -> pure ()
          | Type.Apps' f xs <- e,
            f == declType ->
              unifyVars e xs
          | otherwise -> goEffs es

      unifyVars :: Type.Type v loc -> [Type.Type v loc] -> Gen v loc ()
      unifyVars typ vs = for_ (zip vs tyParams0) \(v, tp) -> do
        lookupType v >>= \case
          Nothing -> error ("[unifyVars] unknown type in decl result: " <> show v)
          Just x -> do
            addConstraint (Unify (Provenance DeclDefinition (ABT.annotation typ)) x tp)
   in goForall constructorType0

-- | Kind constraints for builtin types
builtinConstraints :: forall v loc. (Ord loc, BuiltinAnnotation loc, Var v) => Gen v loc ()
builtinConstraints = do
  traverse_
    (constrain Star)
    [ Type.nat,
      Type.int,
      Type.float,
      Type.boolean,
      Type.text,
      Type.char,
      Type.bytes,
      Type.any,
      Type.termLink,
      Type.typeLink,
      Type.fileHandle,
      flip Type.ref Type.filePathRef,
      Type.threadId,
      Type.socket,
      Type.processHandle,
      Type.ibytearrayType,
      flip Type.ref Type.charClassRef,
      flip Type.ref Type.tlsRef,
      flip Type.ref Type.tlsClientConfigRef,
      flip Type.ref Type.tlsServerConfigRef,
      flip Type.ref Type.tlsSignedCertRef,
      flip Type.ref Type.tlsPrivateKeyRef,
      flip Type.ref Type.tlsCipherRef,
      flip Type.ref Type.tlsVersionRef,
      flip Type.ref Type.codeRef,
      flip Type.ref Type.valueRef,
      flip Type.ref Type.timeSpecRef,
      flip Type.ref Type.hashAlgorithmRef
    ]
  traverse_
    (constrain (Star :-> Star))
    [ Type.list,
      Type.iarrayType,
      flip Type.ref Type.mvarRef,
      flip Type.ref Type.tvarRef,
      flip Type.ref Type.ticketRef,
      flip Type.ref Type.promiseRef,
      flip Type.ref Type.patternRef
    ]
  traverse_
    (constrain Effect)
    [ Type.builtinIO,
      flip Type.ref Type.stmRef
    ]
  traverse_
    (constrain (Star :-> Effect))
    [flip Type.ref Type.scopeRef]
  traverse_
    (constrain (Effect :-> Star))
    [Type.mbytearrayType]
  traverse_
    (constrain (Effect :-> Star :-> Star))
    [Type.effectType, Type.marrayType, Type.refType]
  where
    constrain :: Kind -> (loc -> Type.Type v loc) -> Gen v loc ()
    constrain k t = do
      kindVar <- insertType (t builtinAnnotation)
      constrainToKind kindVar k

constrainToKind :: (BuiltinAnnotation loc, Var v) => UVar v loc -> Kind -> Gen v loc ()
constrainToKind resultVar = \case
  Star -> do
    addConstraint (IsStar resultVar (Provenance Builtin builtinAnnotation))
  Effect -> do
    addConstraint (IsEffect resultVar (Provenance Builtin builtinAnnotation))
  lhs :-> rhs -> do
    let inputTypeVar = Type.var builtinAnnotation (freshIn Set.empty (typed (User "a")))
    let outputTypeVar = Type.var builtinAnnotation (freshIn Set.empty (typed (User "a")))
    input <- freshVar inputTypeVar
    output <- freshVar outputTypeVar
    constrainToKind input lhs
    constrainToKind output rhs
    addConstraint (IsArr resultVar (Provenance Builtin builtinAnnotation) input output)

data Kind = Star | Effect | Kind :-> Kind

infixr 9 :->
