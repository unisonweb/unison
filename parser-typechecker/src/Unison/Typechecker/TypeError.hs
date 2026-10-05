{-# LANGUAGE BangPatterns #-}

module Unison.Typechecker.TypeError where

import Data.List.NonEmpty (NonEmpty)
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (ConstructorReference)
import Unison.KindInference (KindError)
import Unison.Pattern (Pattern)
import Unison.Prelude hiding (whenM)
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Context qualified as C
import Unison.Typechecker.Extractor qualified as Ex
import Unison.Typechecker.TypeVar (lowerType)
import Unison.Util.Monoid (whenM)
import Unison.Var (Var)
import Prelude hiding (all, and, or)

data BooleanMismatch = CondMismatch | AndMismatch | OrMismatch | GuardMismatch
  deriving (Show)

data ExistentialMismatch = IfBody | ListBody | CaseBody
  deriving (Show)

-- | Additional mismatch info, which can be useful for providing hints in error messages.
data MismatchInfo
  = MissingDelay
  | SuperfluousDelay
  deriving (Show, Eq, Ord)

data TypeError v loc
  = Mismatch
      { foundType :: C.Type v loc, -- overallType1
        expectedType :: C.Type v loc, -- overallType2
        foundLeaf :: C.Type v loc, -- leaf1
        expectedLeaf :: C.Type v loc, -- leaf2
        mismatchSite :: C.Term v loc,
        note :: C.ErrorNote v loc,
        additionalInfo :: Maybe MismatchInfo
      }
  | BooleanMismatch
      { getBooleanMismatch :: BooleanMismatch,
        mismatchSite :: C.Term v loc,
        foundType :: C.Type v loc,
        note :: C.ErrorNote v loc
      }
  | ExistentialMismatch
      { getExistentialMismatch :: ExistentialMismatch,
        expectedType :: C.Type v loc,
        expectedLoc :: loc,
        foundType :: C.Type v loc,
        mismatchSite :: C.Term v loc,
        note :: C.ErrorNote v loc
      }
  | ActionRestrictionFailure
      { foundType :: C.Type v loc,
        mismatchSite :: C.Term v loc,
        note :: C.ErrorNote v loc
      }
  | FunctionApplication
      { f :: C.Term v loc,
        ft :: C.Type v loc,
        arg :: C.Term v loc,
        argNum :: Int,
        foundType :: C.Type v loc,
        expectedType :: C.Type v loc,
        leafs :: Maybe (C.Type v loc, C.Type v loc), -- found, expected
        solvedVars :: [(v, C.Type v loc)],
        note :: C.ErrorNote v loc,
        patternCtor :: Maybe ConstructorReference
      }
  | NotFunctionApplication
      { f :: C.Term v loc,
        ft :: C.Type v loc,
        note :: C.ErrorNote v loc,
        args :: [C.Term v loc]
      }
  | FunctionUnderApplied
      { foundType :: C.Type v loc, -- overallType1
        expectedType :: C.Type v loc, -- overallType2
        foundLeaf :: C.Type v loc, -- leaf1
        expectedLeaf :: C.Type v loc, -- leaf2
        mismatchSite :: C.Term v loc,
        note :: C.ErrorNote v loc,
        needArgs :: [Type v loc]
      }
  | AbilityCheckFailure
      { ambient :: [C.Type v loc],
        requested :: [C.Type v loc],
        abilityCheckFailureSite :: loc,
        note :: C.ErrorNote v loc
      }
  | AbilitySubtypeFailure
      { sub :: [C.Type v loc],
        sup :: [C.Type v loc],
        tsub :: C.Type v loc,
        tsup :: C.Type v loc,
        abilityCheckFailureSite :: loc,
        note :: C.ErrorNote v loc
      }
  | AbilityEqFailure
      { lhs :: [C.Type v loc],
        rhs :: [C.Type v loc],
        tlhs :: C.Type v loc,
        trhs :: C.Type v loc,
        abilityCheckFailureSite :: loc,
        note :: C.ErrorNote v loc
      }
  | AbilityEqFailureFromAp
      { lhs :: [C.Type v loc],
        rhs :: [C.Type v loc],
        tlhs :: C.Type v loc,
        trhs :: C.Type v loc,
        expectedSite :: C.Term v loc,
        mismatchSite :: C.Term v loc,
        note :: C.ErrorNote v loc
      }
  | AbilityInstantiationFailure
      { var :: v,
        inst :: [C.Type v loc],
        instSite :: C.Term v loc,
        note :: C.ErrorNote v loc
      }
  | UnguardedLetRecCycle
      { cycle :: [v],
        cycleLocs :: [loc],
        note :: C.ErrorNote v loc
      }
  | UnknownType
      { unknownTypeV :: v,
        typeSite :: loc,
        note :: C.ErrorNote v loc
      }
  | UnknownTerm
      { unknownTermV :: v,
        termSite :: loc,
        suggestions :: [C.Suggestion v loc],
        expectedType :: C.Type v loc,
        note :: C.ErrorNote v loc
      }
  | DuplicateDefinitions
      { defns :: NonEmpty (v, [loc]),
        note :: C.ErrorNote v loc
      }
  | UncoveredPatterns loc (NonEmpty (Pattern ()))
  | RedundantPattern loc
  | KindInferenceFailure (KindError v loc)
  | RunWatchTypeMismatch (Type v loc) loc (C.ErrorNote v loc)
  | Other (C.ErrorNote v loc)
  deriving (Show)

type RedundantTypeAnnotation = Bool

data TypeInfo v loc = TopLevelComponent
  {definitions :: [(v, Type v loc, RedundantTypeAnnotation)]}
  deriving (Show)

type TypeNote v loc = Either (TypeError v loc) (TypeInfo v loc)

typeErrorFromNote ::
  (Ord loc, Show loc, Var v) => C.ErrorNote v loc -> TypeError v loc
typeErrorFromNote n = case Ex.extract allErrors n of
  Just msg -> msg
  Nothing -> Other n

typeInfoFromNote ::
  (Ord loc, Show loc, Var v) => C.InfoNote v loc -> Maybe (TypeInfo v loc)
typeInfoFromNote n = case n of
  C.TopLevelComponent defs -> Just $ TopLevelComponent defs
  _ -> Nothing

allErrors ::
  (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
allErrors =
  asum
    [ and,
      or,
      cond,
      actionRestriction,
      matchGuard,
      ifBody,
      listBody,
      matchBody,
      applyingPatternConstructor,
      applyingFunction,
      applyingNonFunction,
      generalMismatch,
      abilitySubFailure,
      abilityEqFailure,
      abilityCheckFailure,
      badEffectInstantiation,
      unguardedCycle,
      unknownType,
      unknownTerm,
      duplicateDefinitions,
      redundantPattern,
      uncoveredPatterns,
      kindInferenceFailure,
      runWatchTypeMismatch
    ]

topLevelComponent :: Ex.InfoExtractor v a (TypeInfo v a)
topLevelComponent = do
  defs <- Ex.topLevelComponent
  pure $ TopLevelComponent defs

redundantPattern :: Ex.ErrorExtractor v a (TypeError v a)
redundantPattern = do
  ploc <- Ex.redundantPattern
  pure (RedundantPattern ploc)

kindInferenceFailure :: Ex.ErrorExtractor v a (TypeError v a)
kindInferenceFailure = do
  ke <- Ex.kindInferenceFailure
  pure (KindInferenceFailure ke)

runWatchTypeMismatch :: Ex.ErrorExtractor v a (TypeError v a)
runWatchTypeMismatch = do
  (loc, typ) <- Ex.runWatchTypeMismatch
  n <- Ex.errorNote
  pure (RunWatchTypeMismatch typ loc n)

uncoveredPatterns :: Ex.ErrorExtractor v a (TypeError v a)
uncoveredPatterns = do
  (mloc, uncoveredCases) <- Ex.uncoveredPatterns
  pure (UncoveredPatterns mloc uncoveredCases)

abilityCheckFailure :: Ex.ErrorExtractor v a (TypeError v a)
abilityCheckFailure = do
  (ambient, requested, _ctx) <- Ex.abilityCheckFailure
  e <- Ex.innermostTerm
  n <- Ex.errorNote
  pure $ AbilityCheckFailure ambient requested (ABT.annotation e) n

abilitySubFailure :: Ex.ErrorExtractor v a (TypeError v a)
abilitySubFailure = do
  (sup, sub, _ctx) <- Ex.abilityCheckFailure
  failSite <- ABT.annotation <$> Ex.innermostTerm
  note <- Ex.errorNote
  path <- Ex.path
  (tsub, tsup) : _ <- pure . mapMaybe p $ reverse path
  pure $ AbilitySubtypeFailure sub sup tsub tsup failSite note
  where
    p (C.InSubtype t1 t2) = Just (t1, t2)
    p _ = Nothing

abilityEqFailure :: Ex.ErrorExtractor v a (TypeError v a)
abilityEqFailure = do
  (lhs, rhs, _ctx) <- Ex.abilityEqFailure
  e <- Ex.innermostTerm
  n <- Ex.errorNote
  path <- Ex.path
  (tlhs, trhs) : _ <- pure . mapMaybe p $ reverse path
  let app = do
        (_, f, _, _) <- Ex.unique Ex.inFunctionCall
        pure $ AbilityEqFailureFromAp lhs rhs tlhs trhs f e n
      plain = pure $ AbilityEqFailure lhs rhs tlhs trhs (ABT.annotation e) n
  app <|> plain
  where
    p (C.InSubtype t1 t2) = Just (t1, t2)
    p (C.InEquate t1 t2) = Just (t1, t2)
    p _ = Nothing

duplicateDefinitions :: Ex.ErrorExtractor v a (TypeError v a)
duplicateDefinitions = do
  vs <- Ex.duplicateDefinitions
  n <- Ex.errorNote
  pure $ DuplicateDefinitions (vs <&> second toList) n

unknownType :: Ex.ErrorExtractor v loc (TypeError v loc)
unknownType = do
  (loc, v) <- Ex.unknownSymbol
  n <- Ex.errorNote
  pure $ UnknownType v loc n

unknownTerm :: (Var v) => Ex.ErrorExtractor v loc (TypeError v loc)
unknownTerm = do
  (loc, v, suggs, typ) <- Ex.unknownTerm
  n <- Ex.errorNote
  pure $ UnknownTerm v loc suggs (Type.cleanup typ) n

data EffInst v loc
  = Eff [C.Type v loc] [C.Type v loc] -- want, have
  | Normal

-- checks if the top of the path was an instantiation, and indicates
-- whether it was from an ability variable.
instantiation ::
  (Var v, Ord loc) =>
  Ex.ErrorExtractor v loc (v, C.Type v loc, EffInst v loc)
instantiation =
  Ex.path >>= \case
    C.InInstantiateR ty v : path -> pure $ classify v ty path
    C.InInstantiateL v ty : path -> pure $ classify v ty path
    _ -> mzero
  where
    classify v ty (C.InSubAbilities want have : _) =
      (v, ty, Eff want have)
    classify v ty _ = (v, ty, Normal)

badEffectInstantiation ::
  (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
badEffectInstantiation = do
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  (v, _ty, isEff) <- instantiation
  Eff want _ <- pure isEff
  n <- Ex.errorNote
  site <- Ex.innermostTerm
  pure $
    AbilityInstantiationFailure v (Type.cleanups $ sub <$> want) site n

generalMismatch :: (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
generalMismatch = do
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t

      subtypes :: Ex.ErrorExtractor v loc [(C.Type v loc, C.Type v loc)]
      subtypes = do
        path <- Ex.path
        pure [(t1, t2) | C.InSubtype t1 t2 <- path]

      firstLastSubtype ::
        Ex.ErrorExtractor
          v
          loc
          ( (C.Type v loc, C.Type v loc),
            (C.Type v loc, C.Type v loc)
          )
      firstLastSubtype =
        subtypes >>= \case
          [] -> empty
          l -> pure (head l, last l)
  n <- Ex.errorNote
  mismatchSite <- Ex.innermostTerm
  ((foundLeaf, expectedLeaf), (foundType, expectedType)) <- firstLastSubtype
  let mayNeedArgs = findUnderApplication foundLeaf expectedLeaf
  -- If the found type is a function, and the result of that function matches the expected type,
  -- it's likely we're missing some arguments from a function.

  case Type.cleanups [sub foundType, sub expectedType, sub foundLeaf, sub expectedLeaf] of
    [ft, et, fl, el] -> do
      let delayMismatch = Typechecker.isMismatchMissingDelay foundType expectedType
      case (mayNeedArgs, delayMismatch) of
        (_, Just (Left {})) -> pure $ Mismatch ft et fl el mismatchSite n (Just MissingDelay)
        (_, Just (Right {})) -> pure $ Mismatch ft et fl el mismatchSite n (Just SuperfluousDelay)
        (Just needArgs, _)
          | not (null needArgs) -> pure $ FunctionUnderApplied ft et fl el mismatchSite n (lowerType <$> needArgs)
        _ -> do
          pure $ Mismatch ft et fl el mismatchSite n Nothing
    _ -> error "generalMismatch: Mismatched type binding"
  where
    findUnderApplication found expected
      | Right True <- C.isSubtype found expected = pure []
      | otherwise =
          case found of
            Type.Arrow' i o -> (i :) <$> findUnderApplication o expected
            Type.ForallNamed' _ body -> findUnderApplication body expected
            Type.Effect' _ inner -> findUnderApplication inner expected
            _ -> Nothing

and,
  or,
  cond,
  matchGuard ::
    (Var v, Ord loc) =>
    Ex.ErrorExtractor v loc (TypeError v loc)
and = booleanMismatch0 AndMismatch (Ex.inSynthesizeApp >> Ex.inAndApp)
or = booleanMismatch0 OrMismatch (Ex.inSynthesizeApp >> Ex.inOrApp)
cond = booleanMismatch0 CondMismatch Ex.inIfCond
matchGuard = booleanMismatch0 GuardMismatch Ex.inMatchGuard

unguardedCycle :: Ex.ErrorExtractor v loc (TypeError v loc)
unguardedCycle = do
  n <- Ex.errorNote
  C.UnguardedLetRecCycle vs es <- Ex.cause
  let loc = ABT.annotation . snd <$> es
  pure $ UnguardedLetRecCycle vs loc n

-- | helper function to support `and` / `or` / `cond`
booleanMismatch0 ::
  (Var v, Ord loc) =>
  BooleanMismatch ->
  Ex.SubseqExtractor v loc () ->
  Ex.ErrorExtractor v loc (TypeError v loc)
booleanMismatch0 b ex = do
  n <- Ex.errorNote
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  mismatchSite <- Ex.innermostTerm
  foundType <- Ex.unique $ do
    Ex.pathStart
    (foundType, _, _) <- inSubtypes
    void $ Ex.some Ex.inCheck
    ex
    pure $ Type.cleanup foundType
  pure (BooleanMismatch b mismatchSite (sub foundType) n)

existentialMismatch0 ::
  (Var v, Ord loc) =>
  ExistentialMismatch ->
  Ex.SubseqExtractor v loc loc ->
  Ex.ErrorExtractor v loc (TypeError v loc)
existentialMismatch0 em getExpectedLoc = do
  n <- Ex.errorNote
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  mismatchSite <- Ex.innermostTerm
  ([foundType, expectedType], expectedLoc) <- Ex.unique $ do
    Ex.pathStart
    subtypes@(_ : _) <- Ex.some Ex.inSubtype
    let (foundType, expectedType) = last subtypes
    void $ Ex.some Ex.inCheck
    expectedLoc <- getExpectedLoc
    pure (Type.cleanups [foundType, expectedType], expectedLoc)
  pure $
    ExistentialMismatch
      em
      (sub expectedType)
      expectedLoc
      (sub foundType)
      mismatchSite
      -- todo : save type leaves too
      n

actionRestriction ::
  (Var v, Ord loc) =>
  Ex.ErrorExtractor v loc (TypeError v loc)
actionRestriction = do
  Ex.unique Ex.inActionRestriction
  note <- Ex.errorNote
  mismatchSite <- Ex.innermostTerm
  path <- Ex.path
  let subtypes = [t1 | C.InSubtype t1 _ <- path]
  guard . not $ null subtypes
  let foundType = Type.cleanup $ last subtypes
  pure $ ActionRestrictionFailure foundType mismatchSite note

ifBody,
  listBody,
  matchBody ::
    (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
ifBody = existentialMismatch0 IfBody (Ex.inSynthesizeApp >> Ex.inIfBody)
listBody = existentialMismatch0 ListBody (Ex.inSynthesizeApp >> Ex.inVector)
matchBody = existentialMismatch0 CaseBody (Ex.inMatchBody >> Ex.inMatch)

applyingNonFunction :: (Var v) => Ex.ErrorExtractor v loc (TypeError v loc)
applyingNonFunction = do
  _ <- Ex.typeMismatch
  n <- Ex.errorNote
  (f, ft, args) <- Ex.unique $ do
    Ex.pathStart
    _synthApp <- Ex.inSynthesizeApp
    (_, f, ft, args) <- Ex.inFunctionCall
    let expectedArgCount = Type.arityIgnoringEffects ft
        foundArgCount = length args
    -- unexpectedArgLoc = ABT.annotation arg
    whenM (expectedArgCount < foundArgCount) $ pure (f, ft, args)
  pure $ NotFunctionApplication f (Type.cleanup ft) n args

-- | Want to collect this info:
-- The `n`th argument to `f` is `foundType`, but I was expecting `expectedType`.
--
--    30 |   asdf asdf asdf
--
-- If you're curious
-- `f` has type `blah`, where
--    `a` was chosen as `A`
--    `b` was chosen as `B`
--    `c` was chosen as `C`
-- (many colors / groups)
applyingFunction :: forall v loc. (Var v) => Ex.ErrorExtractor v loc (TypeError v loc)
applyingFunction = do
  n <- Ex.errorNote
  ctx <- Ex.typeMismatch
  Ex.unique $ do
    Ex.pathStart
    -- todo: make a new extrator for (some inSubtype) that pulls out the head and tail and nothing in between?
    (found, expected, leafs) <- inSubtypes
    arg <- fst . head <$> Ex.some Ex.inCheck
    (_, _, argIndex) <- Ex.inSynthesizeApp
    (typeVars, f, ft, _args) <- Ex.inFunctionCall
    let go :: v -> Maybe (v, C.Type v loc)
        go v = (v,) . Type.getPolytype <$> C.lookupSolved ctx v
        solvedVars = catMaybes (go <$> typeVars)
    let vm =
          Type.cleanupVarsMap $
            [ft, found, expected]
              <> (fst <$> toList leafs)
              <> (snd <$> toList leafs)
              <> (snd <$> solvedVars)
        cleanup = Type.cleanupVars1' vm . Type.cleanupAbilityLists
    pure $
      FunctionApplication
        f
        (cleanup ft)
        arg
        argIndex
        (cleanup found)
        (cleanup expected)
        ((\(a, b) -> (cleanup a, cleanup b)) <$> leafs)
        (second cleanup <$> solvedVars)
        n
        Nothing

inSubtypes ::
  Ex.SubseqExtractor
    v
    loc
    ( C.Type v loc,
      C.Type v loc,
      Maybe (C.Type v loc, C.Type v loc)
    )
inSubtypes = do
  subtypes <- Ex.some Ex.inSubtype
  let ((found, expected), leaves) = case subtypes of
        [] -> error "unpossible: Ex.some should only succeed on nonnull output"
        [(found, expected)] -> ((found, expected), Nothing)
        _ -> (last subtypes, Just $ head subtypes)
  pure (found, expected, leaves)

-- | Like 'applyingFunction', but for type mismatches in pattern constructor
-- arguments. The error path contains 'InPatternApply' between 'InSubtype' and
-- 'InCheck', which breaks the adjacency chain of 'applyingFunction'.
--
-- Path: InSubtype → InPatternApply → InCheck → InSynthesizeApp → InFunctionCall
applyingPatternConstructor :: forall v loc. (Var v) => Ex.ErrorExtractor v loc (TypeError v loc)
applyingPatternConstructor = do
  n <- Ex.errorNote
  ctx <- Ex.typeMismatch
  Ex.unique $ do
    Ex.pathStart
    (found, expected, leafs) <- inSubtypes
    ref <- Ex.inPatternApply
    arg <- fst . head <$> Ex.some Ex.inCheck
    (_, _, argIndex) <- Ex.inSynthesizeApp
    (typeVars, f, ft, _args) <- Ex.inFunctionCall
    let go :: v -> Maybe (v, C.Type v loc)
        go v = (v,) . Type.getPolytype <$> C.lookupSolved ctx v
        solvedVars = catMaybes (go <$> typeVars)
    let vm =
          Type.cleanupVarsMap $
            [ft, found, expected]
              <> (fst <$> toList leafs)
              <> (snd <$> toList leafs)
              <> (snd <$> solvedVars)
        cleanup = Type.cleanupVars1' vm . Type.cleanupAbilityLists
    pure $
      FunctionApplication
        f
        (cleanup ft)
        arg
        argIndex
        (cleanup found)
        (cleanup expected)
        ((\(a, b) -> (cleanup a, cleanup b)) <$> leafs)
        (second cleanup <$> solvedVars)
        n
        (Just ref)
