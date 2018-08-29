{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Unison.Typechecker.TypeError where

import           Prelude hiding (all, and, or)
import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
import qualified Unison.ABT as ABT
import qualified Unison.Typechecker.Context as C
import qualified Unison.Typechecker.Extractor2 as Ex
import           Unison.Var (Var)

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
  | BooleanMismatch { booleanMismatch' :: BooleanMismatch
                    , mismatchSite     :: loc
                    , foundType        :: C.Type v loc
                    , note             :: C.Note v loc
                    }
  | ExistentialMismatch { existentialMismatch' :: ExistentialMismatch
                        , expectedType         :: C.Type v loc
                        , expectedLoc          :: loc
                        , foundType            :: C.Type v loc
                        , mismatchSite         :: loc
                        , note                 :: C.Note v loc
                        }
  | FunctionApplication { f            :: C.Term v loc
                        , argNum       :: Int
                        , expectedType :: C.Type v loc
                        , foundType    :: C.Type v loc
                        , arg          :: C.Term v loc
                        , fVarInfo     :: Maybe (C.Type v loc, [(v, C.Type v loc)])
                        , note         :: C.Note v loc
                        }
  | NotFunctionApplication { f :: C.Term v loc
                           , ft :: C.Type v loc
                           , note :: C.Note v loc
                           }
  | AbilityCheckFailure { ambient                 :: [C.Type v loc]
                        , requested               :: [C.Type v loc]
                        , abilityCheckFailureSite :: loc
                        , note :: C.Note v loc
                        }
  | UnknownType { unknownType :: v
                , typeSite    :: loc
                , note        :: C.Note v loc
                }
  | UnknownTerm { unknownTerm :: v
                , termSite :: loc
                , suggestions :: [C.Suggestion v loc]
                , expectedType :: C.Type v loc
                , note :: C.Note v loc
                }
  | Other (C.Note v loc)

typeErrorFromNote :: forall loc v. (Ord loc, Show loc, Var v) => C.Note v loc -> TypeError v loc
typeErrorFromNote n = case Ex.runNote all n of
  Just msg -> msg
  Nothing -> Other n

all :: (Var v, Ord loc) => Ex.NoteExtractor v loc (TypeError v loc)
all = and <|> or <|> cond <|> generalMismatch <|> applyingNonFunction
--
-- {-
-- typeErrorFromNote n@(C.Note (C.TypeMismatch ctx) path) = do
--   (firstSubtype, lastSubtype, innermostTerm) <- mismatch
--   let
--     pathl = toList path
--     subtypes = [ (t1, t2) | C.InSubtype t1 t2 <- pathl ]
--     firstSubtype = listToMaybe subtypes
--     lastSubtype = if null subtypes then Nothing else Just (last subtypes)
--     innermostTerm = C.innermostErrorTerm n
--     -- replace any type vars with their solutions before returning
--     sub t = C.apply ctx t
--   in case (firstSubtype, lastSubtype, innermostTerm) of
--     (Just (foundLeaf, expectedLeaf),
--      Just (foundType, expectedType),
--      Just mismatchSite) ->
--       let mismatchLoc = ABT.annotation mismatchSite
--       in case Ex.run all n of
--         Just msg -> msg
--         Nothing ->
--           Mismatch (sub foundType) (sub expectedType)
--                    (sub foundLeaf) (sub expectedLeaf)
--                    (ABT.annotation mismatchSite)
--                    n
--     (Nothing, Nothing, Just mismatchSite) ->
--       let --mismatchLoc = ABT.annotation mismatchSite
--           appyingNonFunction = do
--             (f, ft) <- Ex.applyingNonFunction
--             pure $ NotFunctionApplication f ft n
--       in case Ex.run appyingNonFunction n of
--         Just msg -> msg
--         Nothing -> trace ("other debug:\n"
--                           ++ "  mismatchSite: " ++ show mismatchSite ++ "\n") $
--                    Other n
--     _ ->
--       trace ("other debug:\n"
--               ++ "   firstSubtype: " ++ show firstSubtype ++ "\n"
--               ++ "    lastSubtype: " ++ show lastSubtype ++ "\n"
--               ++ "  innermostTerm: " ++ show innermostTerm ++ "\n") $
--       Other n
-- typeErrorFromNote n@(C.Note (C.AbilityCheckFailure amb req _) _) =
--   let go :: C.Term v loc -> TypeError v loc
--       go e = AbilityCheckFailure amb req (ABT.annotation e) n
--   in fromMaybe (Other n) $ go <$> C.innermostErrorTerm n
-- typeErrorFromNote n@(C.Note (C.UnknownSymbol loc v) _) =
--   UnknownType v loc n
-- typeErrorFromNote n@(C.Note (C.UnknownTerm loc v suggs typ) _) =
--   UnknownTerm v loc suggs typ n
-- typeErrorFromNote n@(C.Note _ _) = Other n
--
-- -}
--

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
booleanMismatchApply :: (Var v, Ord loc) => Ex.SubseqExtractor v loc () -> BooleanMismatch -> Ex.NoteExtractor v loc (TypeError v loc)
booleanMismatchApply ex b =
  booleanMismatch0 >>=
    \(mismatchLoc, foundType, n) -> do
      (Ex.unique $ Ex.no Ex.inSynthesizeApp >> Ex.inSynthesizeApp >> ex) >>
        pure (BooleanMismatch b mismatchLoc foundType n)

-- | Extracts a type mismatch location, the found type, and the full note;
-- | helper for `and` / `or` / `cond`.
booleanMismatch0 :: (Var v, Ord loc) => Ex.NoteExtractor v loc (loc, C.Type v loc, C.Note v loc)
booleanMismatch0 = do
  ctx <- Ex.typeMismatch
  mismatchSite <- Ex.innermostTerm
  n <- Ex.note
  (_, (foundType, _)) <- firstLastSubtype
  pure $ (ABT.annotation mismatchSite, C.apply ctx foundType, n)


applyingNonFunction :: Ex.NoteExtractor v loc (TypeError v loc)
applyingNonFunction = do
  _ <- Ex.typeMismatch
  n <- Ex.note
  (f, ft) <- Ex.unique  Ex.applyingNonFunction
  pure $ NotFunctionApplication f ft n

-- -- existentialMismatch
-- --   :: Monad m => m loc -> ExistentialMismatch -> m (TypeError v loc)
-- -- existentialMismatch x y = x >>= \expectedLoc -> pure $
-- --   ExistentialMismatch y expectedType expectedLoc foundType mismatchLoc n

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
