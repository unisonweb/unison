{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Typechecker.TypeError where

import           Control.Applicative           ((<|>))
import           Control.Monad                 (mzero)
import           Prelude                       hiding (all, and, or)
import qualified Unison.ABT                    as ABT
import qualified Unison.Typechecker.Context    as C
import qualified Unison.Typechecker.Extractor2 as Ex
import           Unison.Var                    (Var)

data BooleanMismatch = CondMismatch | AndMismatch | OrMismatch | GuardMismatch
data ExistentialMismatch = IfBody | VectorBody | CaseBody

data TypeError v loc
  = Mismatch { foundType    :: C.Type v loc -- overallType1
             , expectedType :: C.Type v loc -- overallType2
             , foundLeaf    :: C.Type v loc -- leaf1
             , expectedLeaf :: C.Type v loc -- leaf2
             , mismatchSite :: loc
             , note         :: C.Note v loc
             }
  | BooleanMismatch { getBooleanMismatch :: BooleanMismatch
                    , mismatchSite       :: loc
                    , foundType          :: C.Type v loc
                    , note               :: C.Note v loc
                    }
  | ExistentialMismatch { getExistentialMismatch :: ExistentialMismatch
                        , expectedType           :: C.Type v loc
                        , expectedLoc            :: loc
                        , foundType              :: C.Type v loc
                        , mismatchSite           :: loc
                        , note                   :: C.Note v loc
                        }
  | FunctionApplication { f            :: C.Term v loc
                        , argNum       :: Int
                        , expectedType :: C.Type v loc
                        , foundType    :: C.Type v loc
                        , arg          :: C.Term v loc
                        , fVarInfo     :: Maybe (C.Type v loc, [(v, C.Type v loc)])
                        , note         :: C.Note v loc
                        }
  | NotFunctionApplication { f    :: C.Term v loc
                           , ft   :: C.Type v loc
                           , note :: C.Note v loc
                           }
  | AbilityCheckFailure { ambient                 :: [C.Type v loc]
                        , requested               :: [C.Type v loc]
                        , abilityCheckFailureSite :: loc
                        , note                    :: C.Note v loc
                        }
  | UnknownType { unknownTypeV :: v
                , typeSite     :: loc
                , note         :: C.Note v loc
                }
  | UnknownTerm { unknownTermV :: v
                , termSite     :: loc
                , suggestions  :: [C.Suggestion v loc]
                , expectedType :: C.Type v loc
                , note         :: C.Note v loc
                }
  | Other (C.Note v loc)

typeErrorFromNote :: forall loc v. (Ord loc, Show loc, Var v) => C.Note v loc -> TypeError v loc
typeErrorFromNote n = case Ex.runNote all n of
  Just msg -> msg
  Nothing  -> Other n

all :: (Var v, Ord loc) => Ex.NoteExtractor v loc (TypeError v loc)
all = and <|> or <|> cond <|> generalMismatch <|> applyingNonFunction <|>
      abilityCheckFailure <|> unknownType <|> unknownTerm

abilityCheckFailure :: Ex.NoteExtractor v a (TypeError v a)
abilityCheckFailure = do
  (ambient, requested, _ctx) <- Ex.abilityCheckFailure
  e <- Ex.innermostTerm
  n <- Ex.note
  pure $ AbilityCheckFailure ambient requested (ABT.annotation e) n

unknownType :: Ex.NoteExtractor v loc (TypeError v loc)
unknownType = do
  (loc, v) <- Ex.unknownSymbol
  n <- Ex.note
  pure $ UnknownType v loc n

unknownTerm :: Ex.NoteExtractor v loc (TypeError v loc)
unknownTerm = do
  (loc, v, suggs, typ) <- Ex.unknownTerm
  n <- Ex.note
  pure $ UnknownTerm v loc suggs typ n

generalMismatch :: (Var v, Ord loc) => Ex.NoteExtractor v loc (TypeError v loc)
generalMismatch = do
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  n <- Ex.note
  mismatchSite <- Ex.innermostTerm
  ((foundLeaf, expectedLeaf), (foundType, expectedType)) <- firstLastSubtype
  pure $ Mismatch (sub foundType) (sub expectedType)
                  (sub foundLeaf) (sub expectedLeaf)
                  (ABT.annotation mismatchSite)
                  n

subtypes :: Ex.NoteExtractor v loc [(C.Type v loc, C.Type v loc)]
subtypes = do
  path <- Ex.path
  pure [ (t1, t2) | C.InSubtype t1 t2 <- path ]

firstLastSubtype :: Ex.NoteExtractor v loc ( (C.Type v loc, C.Type v loc)
                                           , (C.Type v loc, C.Type v loc) )
firstLastSubtype = subtypes >>= \case
  [] -> mzero
  l -> pure (head l, last l)

and,or,cond :: (Var v, Ord loc) => Ex.NoteExtractor v loc (TypeError v loc)
and = booleanMismatchApply Ex.inAndApp AndMismatch
or = booleanMismatchApply Ex.inOrApp OrMismatch
cond = booleanMismatch0 >>=
  \(mismatchLoc, foundType, n) -> Ex.unique $ do
    Ex.no Ex.inSynthesizeApp
    Ex.inIfCond
    pure $ BooleanMismatch CondMismatch mismatchLoc foundType n

-- | helper function to support `and` / `or`
booleanMismatchApply :: (Var v, Ord loc)
                     => Ex.SubseqExtractor v loc ()
                     -> BooleanMismatch
                     -> Ex.NoteExtractor v loc (TypeError v loc)
booleanMismatchApply ex b =
  booleanMismatch0 >>=
    \(mismatchLoc, foundType, n) -> do
      Ex.unique $ do
        Ex.pathStart
        Ex.no Ex.inSynthesizeApp
        _ <- Ex.inSynthesizeApp
        ex
      pure (BooleanMismatch b mismatchLoc foundType n)

-- | Extracts a type mismatch location, the found type, and the full note;
-- | helper for `and` / `or` / `cond`.
booleanMismatch0 :: (Var v, Ord loc)
                 => Ex.NoteExtractor v loc (loc, C.Type v loc, C.Note v loc)
booleanMismatch0 = do
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  mismatchSite <- Ex.innermostTerm
  n <- Ex.note
  (_, (foundType, _)) <- firstLastSubtype
  pure (ABT.annotation mismatchSite, sub foundType, n)


applyingNonFunction :: Ex.NoteExtractor v loc (TypeError v loc)
applyingNonFunction = do
  _ <- Ex.typeMismatch
  n <- Ex.note
  (f, ft) <- Ex.unique  Ex.applyingNonFunction
  pure $ NotFunctionApplication f ft n

existentialMismatch
  :: (Var v, Ord loc)
  => Ex.NoteExtractor v loc loc
  -> ExistentialMismatch
  -> Ex.NoteExtractor v loc (TypeError v loc)
existentialMismatch getLoc em = do
  n <- Ex.note
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  mismatchSite <- Ex.innermostTerm
  let mismatchLoc = ABT.annotation mismatchSite
  (_, (foundType, expectedType)) <- firstLastSubtype
  expectedLoc <- getLoc
  pure $ ExistentialMismatch em (sub expectedType) expectedLoc
                                (sub foundType) mismatchLoc
                                n
-- existentialMismatch x y = x >>= \expectedLoc -> pure $
--   ExistentialMismatch y expectedType expectedLoc foundType mismatchLoc n
-- | gets expectedType, expectedLoc, foundType, mismatchLoc, n
existentialMismatch0 :: (Var v, Ord loc)
                     => Ex.NoteExtractor v loc (C.Type v loc, loc,
                                                C.Type v loc, loc,
                                                C.Note v loc)
existentialMismatch0 = do
  ctx <- Ex.typeMismatch
  let sub t = C.apply ctx t
  mismatchSite <- Ex.innermostTerm
  n <- Ex.note
  (_, (foundType, expectedType)) <- firstLastSubtype
  pure (sub expectedType, ABT.annotation expectedType,
        sub foundType,
        ABT.annotation mismatchSite,
        n)

-- existentialMismatchApply

-- -- and,or,cond,guard,ifBody,vectorBody,caseBody,all,functionArg
-- --   :: Ex.NoteExtractor v loc (TypeError v loc)

-- -- guard = booleanMismatch Ex.inMatchCaseGuard GuardMismatch
-- -- ifBody = existentialMismatch Ex.inIfBody IfBody
-- -- vectorBody = existentialMismatch Ex.inVectorApp VectorBody
-- -- caseBody = existentialMismatch Ex.inMatchCaseBody CaseBody
-- -- functionArg = do
-- --   (f, index, expectedType, foundType, arg, fPoly) <- Ex.inApp
-- --   pure $ FunctionApplication f index expectedType foundType arg fPoly n
--
-- -- all = and <|> or <|> cond <|> guard <|>
-- --       ifBody <|> vectorBody <|> caseBody <|> functionArg
--
-- all :: (Var v, Ord loc) => Ex.NoteExtractor v loc (TypeError v loc)
-- all = asum [generalMismatch, applyingNonFunction]
