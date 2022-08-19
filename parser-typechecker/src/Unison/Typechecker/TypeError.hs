{-# LANGUAGE BangPatterns #-}

module Unison.Typechecker.TypeError where

import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty)
import qualified Unison.ABT as ABT
import Unison.Prelude hiding (whenM)
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker.Context as C
import qualified Unison.Typechecker.Extractor as Ex
import Unison.Util.Monoid (whenM)
import Unison.Var (Var)
import Prelude hiding (all, and, or)

data BooleanMismatch = CondMismatch | AndMismatch | OrMismatch | GuardMismatch
  deriving (Show)

data ExistentialMismatch = IfBody | VectorBody | CaseBody
  deriving (Show)

data TypeError v loc
  = Mismatch
      { foundType :: C.Type v loc, -- overallType1
        expectedType :: C.Type v loc, -- overallType2
        foundLeaf :: C.Type v loc, -- leaf1
        expectedLeaf :: C.Type v loc, -- leaf2
        mismatchSite :: C.Term v loc,
        note :: C.ErrorNote v loc
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
  | FunctionApplication
      { f :: C.Term v loc,
        ft :: C.Type v loc,
        arg :: C.Term v loc,
        argNum :: Int,
        foundType :: C.Type v loc,
        expectedType :: C.Type v loc,
        leafs :: Maybe (C.Type v loc, C.Type v loc), -- found, expected
        solvedVars :: [(v, C.Type v loc)],
        note :: C.ErrorNote v loc
      }
  | NotFunctionApplication
      { f :: C.Term v loc,
        ft :: C.Type v loc,
        note :: C.ErrorNote v loc
      }
  | AbilityCheckFailure
      { ambient :: [C.Type v loc],
        requested :: [C.Type v loc],
        abilityCheckFailureSite :: loc,
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
      matchGuard,
      ifBody,
      vectorBody,
      matchBody,
      applyingFunction,
      applyingNonFunction,
      generalMismatch,
      abilityCheckFailure,
      unguardedCycle,
      unknownType,
      unknownTerm,
      duplicateDefinitions
    ]

topLevelComponent :: Ex.InfoExtractor v a (TypeInfo v a)
topLevelComponent = do
  defs <- Ex.topLevelComponent
  pure $ TopLevelComponent defs

abilityCheckFailure :: Ex.ErrorExtractor v a (TypeError v a)
abilityCheckFailure = do
  (ambient, requested, _ctx) <- Ex.abilityCheckFailure
  e <- Ex.innermostTerm
  n <- Ex.errorNote
  pure $ AbilityCheckFailure ambient requested (ABT.annotation e) n

duplicateDefinitions :: Ex.ErrorExtractor v a (TypeError v a)
duplicateDefinitions = do
  vs <- Ex.duplicateDefinitions
  n <- Ex.errorNote
  pure $ DuplicateDefinitions vs n

unknownType :: Ex.ErrorExtractor v loc (TypeError v loc)
unknownType = do
  (loc, v) <- Ex.unknownSymbol
  n <- Ex.errorNote
  pure $ UnknownType v loc n

unknownTerm :: Var v => Ex.ErrorExtractor v loc (TypeError v loc)
unknownTerm = do
  (loc, v, suggs, typ) <- Ex.unknownTerm
  n <- Ex.errorNote
  pure $ UnknownTerm v loc suggs (Type.cleanup typ) n

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
  case Type.cleanups [sub foundType, sub expectedType, sub foundLeaf, sub expectedLeaf] of
    [ft, et, fl, el] -> pure $ Mismatch ft et fl el mismatchSite n
    _ -> error "generalMismatch: Mismatched type binding"

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

ifBody,
  vectorBody,
  matchBody ::
    (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
ifBody = existentialMismatch0 IfBody (Ex.inSynthesizeApp >> Ex.inIfBody)
vectorBody = existentialMismatch0 VectorBody (Ex.inSynthesizeApp >> Ex.inVector)
matchBody = existentialMismatch0 CaseBody (Ex.inMatchBody >> Ex.inMatch)

applyingNonFunction :: Var v => Ex.ErrorExtractor v loc (TypeError v loc)
applyingNonFunction = do
  _ <- Ex.typeMismatch
  n <- Ex.errorNote
  (f, ft) <- Ex.unique $ do
    Ex.pathStart
    (arity0Type, _arg, _argNum) <- Ex.inSynthesizeApp
    (_, f, ft, args) <- Ex.inFunctionCall
    let expectedArgCount = Type.arity ft
        foundArgCount = length args
    -- unexpectedArgLoc = ABT.annotation arg
    whenM (expectedArgCount < foundArgCount) $ pure (f, arity0Type)
  pure $ NotFunctionApplication f (Type.cleanup ft) n

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
