{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Unison.Typechecker.TypeError where

import           Control.Applicative           (empty)
import           Data.Foldable                 (asum)
import           Data.Functor                  (void)
import           Data.Maybe                    (catMaybes)
import           Prelude                       hiding (all, and, or)
import qualified Unison.ABT                    as ABT
import qualified Unison.Type                   as Type
import qualified Unison.Typechecker.Context    as C
import qualified Unison.Typechecker.Extractor  as Ex
import           Unison.Util.Monoid            (whenM)
import           Unison.Var                    (Var)

data BooleanMismatch = CondMismatch | AndMismatch | OrMismatch | GuardMismatch
  deriving Show

data ExistentialMismatch = IfBody | VectorBody | CaseBody
  deriving Show

data TypeError v loc
  = Mismatch { foundType    :: C.Type v loc -- overallType1
             , expectedType :: C.Type v loc -- overallType2
             , foundLeaf    :: C.Type v loc -- leaf1
             , expectedLeaf :: C.Type v loc -- leaf2
             , mismatchSite :: loc
             , note         :: C.ErrorNote v loc
             }
  | BooleanMismatch { getBooleanMismatch :: BooleanMismatch
                    , mismatchSite       :: loc
                    , foundType          :: C.Type v loc
                    , note               :: C.ErrorNote v loc
                    }
  | ExistentialMismatch { getExistentialMismatch :: ExistentialMismatch
                        , expectedType           :: C.Type v loc
                        , expectedLoc            :: loc
                        , foundType              :: C.Type v loc
                        , mismatchSite           :: loc
                        , note                   :: C.ErrorNote v loc
                        }
  | FunctionApplication { f            :: C.Term v loc
                        , ft           :: C.Type v loc
                        , arg          :: C.Term v loc
                        , argNum       :: Int
                        , foundType    :: C.Type v loc
                        , expectedType :: C.Type v loc
                        , leafs        :: Maybe (C.Type v loc, C.Type v loc) -- found, expected
                        , solvedVars   :: [(v, C.Type v loc)]
                        , note         :: C.ErrorNote v loc
                        }
  | NotFunctionApplication { f    :: C.Term v loc
                           , ft   :: C.Type v loc
                           , note :: C.ErrorNote v loc
                           }
  | AbilityCheckFailure { ambient                 :: [C.Type v loc]
                        , requested               :: [C.Type v loc]
                        , abilityCheckFailureSite :: loc
                        , note                    :: C.ErrorNote v loc
                        }
  | UnknownType { unknownTypeV :: v
                , typeSite     :: loc
                , note         :: C.ErrorNote v loc
                }
  | UnknownTerm { unknownTermV :: v
                , termSite     :: loc
                , suggestions  :: [C.Suggestion v loc]
                , expectedType :: C.Type v loc
                , note         :: C.ErrorNote v loc
                }
  | Other (C.ErrorNote v loc)
  deriving (Show)

data TypeInfo v loc =
  TopLevelComponent { definitions :: [(v, C.Term v loc, C.Type v loc)]
                    , infoNote :: C.InfoNote v loc
                    } deriving (Show)

type TypeNote v loc = Either (TypeError v loc) (TypeInfo v loc)

typeErrorFromNote
  :: (Ord loc, Show loc, Var v) => C.ErrorNote v loc -> TypeError v loc
typeErrorFromNote n = case Ex.extract allErrors n of
  Just msg -> msg
  Nothing  -> Other n

allErrors :: (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
allErrors = asum
  [ and
  , or
  , cond
  , matchGuard
  , ifBody
  , vectorBody
  , matchBody
  , applyingFunction
  , applyingNonFunction
  , generalMismatch
  , abilityCheckFailure
  , unknownType
  , unknownTerm
  ]

topLevelComponent :: Ex.InfoExtractor v a (TypeInfo v a)
topLevelComponent = do
  defs <- Ex.topLevelComponent
  n <- Ex.infoNote
  pure $ TopLevelComponent defs n

abilityCheckFailure :: Ex.ErrorExtractor v a (TypeError v a)
abilityCheckFailure = do
  (ambient, requested, _ctx) <- Ex.abilityCheckFailure
  e <- Ex.innermostTerm
  n <- Ex.errorNote
  pure $ AbilityCheckFailure ambient requested (ABT.annotation e) n

unknownType :: Ex.ErrorExtractor v loc (TypeError v loc)
unknownType = do
  (loc, v) <- Ex.unknownSymbol
  n <- Ex.errorNote
  pure $ UnknownType v loc n

unknownTerm :: Ex.ErrorExtractor v loc (TypeError v loc)
unknownTerm = do
  (loc, v, suggs, typ) <- Ex.unknownTerm
  n <- Ex.errorNote
  pure $ UnknownTerm v loc suggs typ n

generalMismatch :: (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
generalMismatch = do
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t

      subtypes :: Ex.ErrorExtractor v loc [(C.Type v loc, C.Type v loc)]
      subtypes = do
        path <- Ex.path
        pure [ (t1, t2) | C.InSubtype t1 t2 <- path ]

      firstLastSubtype :: Ex.ErrorExtractor v loc ( (C.Type v loc, C.Type v loc)
                                                 , (C.Type v loc, C.Type v loc) )
      firstLastSubtype = subtypes >>= \case
        [] -> empty
        l -> pure (head l, last l)
  n <- Ex.errorNote
  mismatchSite <- Ex.innermostTerm
  ((foundLeaf, expectedLeaf), (foundType, expectedType)) <- firstLastSubtype
  pure $ Mismatch (sub foundType) (sub expectedType)
                  (sub foundLeaf) (sub expectedLeaf)
                  (ABT.annotation mismatchSite)
                  n


and,or,cond,matchGuard
  :: (Var v, Ord loc)
  => Ex.ErrorExtractor v loc (TypeError v loc)
and = booleanMismatch0 AndMismatch (Ex.inSynthesizeApp >> Ex.inAndApp)
or = booleanMismatch0 OrMismatch (Ex.inSynthesizeApp >> Ex.inOrApp)
cond = booleanMismatch0 CondMismatch Ex.inIfCond
matchGuard = booleanMismatch0 GuardMismatch Ex.inMatchGuard

-- | helper function to support `and` / `or` / `cond`
booleanMismatch0 :: (Var v, Ord loc)
                     => BooleanMismatch
                     -> Ex.SubseqExtractor v loc ()
                     -> Ex.ErrorExtractor v loc (TypeError v loc)
booleanMismatch0 b ex = do
  n <- Ex.errorNote
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  mismatchSite <- Ex.innermostTerm
  let mismatchLoc = ABT.annotation mismatchSite
  foundType <- Ex.unique $ do
    Ex.pathStart
    (foundType, _, _) <- inSubtypes
    void $ Ex.some Ex.inCheck
    ex
    pure foundType
  pure (BooleanMismatch b mismatchLoc (sub foundType) n)

existentialMismatch0
  :: (Var v, Ord loc)
  => ExistentialMismatch
  -> Ex.SubseqExtractor v loc loc
  -> Ex.ErrorExtractor v loc (TypeError v loc)
existentialMismatch0 em getExpectedLoc = do
  n <- Ex.errorNote
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  mismatchSite <- Ex.innermostTerm
  let mismatchLoc = ABT.annotation mismatchSite
  (foundType, expectedType, expectedLoc) <- Ex.unique $ do
    Ex.pathStart
    subtypes <- Ex.some Ex.inSubtype
    let (foundType, expectedType) = last subtypes
    void $ Ex.some Ex.inCheck
    expectedLoc <- getExpectedLoc
    pure (foundType, expectedType, expectedLoc)
  pure $ ExistentialMismatch em (sub expectedType) expectedLoc
                                (sub foundType) mismatchLoc
                                -- todo : save type leaves too
                                n

ifBody, vectorBody, matchBody
  :: (Var v, Ord loc) => Ex.ErrorExtractor v loc (TypeError v loc)
ifBody = existentialMismatch0 IfBody (Ex.inSynthesizeApp >> Ex.inIfBody)
vectorBody = existentialMismatch0 VectorBody (Ex.inSynthesizeApp >> Ex.inVector)
matchBody = existentialMismatch0 CaseBody (Ex.inMatchBody >> Ex.inMatch)

applyingNonFunction :: Ex.ErrorExtractor v loc (TypeError v loc)
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
  pure $ NotFunctionApplication f ft n

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
applyingFunction :: forall v loc. Eq v => Ex.ErrorExtractor v loc (TypeError v loc)
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
    pure $ FunctionApplication f ft arg argIndex found expected leafs solvedVars n

inSubtypes :: Ex.SubseqExtractor v loc (C.Type v loc,
                                        C.Type v loc,
                                        Maybe (C.Type v loc, C.Type v loc))
inSubtypes = do
  subtypes <- Ex.some Ex.inSubtype
  let ((found, expected), leaves) = case subtypes of
        [] -> error "unpossible: Ex.some should only succeed on nonnull output"
        [(found, expected)] -> ((found, expected), Nothing)
        _ -> (last subtypes, Just $ head subtypes)
  pure (found, expected, leaves)
