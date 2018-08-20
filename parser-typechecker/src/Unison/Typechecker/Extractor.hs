{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unison.Typechecker.Extractor where

import Control.Monad
import Control.Applicative
import Data.Foldable (toList)
-- import Data.Maybe (catMaybes)
import Data.Maybe (isJust)
-- import qualified Unison.ABT as ABT
import qualified Unison.Typechecker.Context as C
import qualified Unison.Term as Term
-- import qualified Unison.TypeVar as TypeVar
import           Unison.Var (Var)
import Unison.Util.Monoid (whenM)
-- import Debug.Trace

newtype NoteExtractor v loc a =
  NoteExtractor { run :: C.Note v loc -> Maybe a }

newtype PathExtractor v loc a =
  PathExtractor { runPath :: C.PathElement v loc -> Maybe a}

_cause :: NoteExtractor v loc (C.Cause v loc)
_cause = NoteExtractor $ pure . C.cause

_path :: NoteExtractor v loc [C.PathElement v loc]
_path = NoteExtractor $ pure . toList . C.path

_mismatchedTerm :: NoteExtractor v loc (Maybe (C.Term v loc))
_mismatchedTerm = NoteExtractor $ pure . C.innermostErrorTerm

adjacent :: PathExtractor v loc a -> PathExtractor v loc b -> NoteExtractor v loc (a, b)
adjacent (PathExtractor a) (PathExtractor b) =
  NoteExtractor $ go Nothing . toList . C.path where
  go _ [] = Nothing
  go Nothing (h:t) = go (a h) t
  go (Just a) (h:t) = case b h of Nothing -> go Nothing t; Just b -> Just (a,b)

adjacent3 :: (Show a, Show b, Show c, Show loc, Var v)
          => PathExtractor v loc a
          -> PathExtractor v loc b
          -> PathExtractor v loc c
          -> NoteExtractor v loc (a, b, c)
adjacent3 (PathExtractor a) (PathExtractor b) (PathExtractor c)=
  NoteExtractor $ go Nothing Nothing . toList . C.path where
  go _ _ [] = Nothing
  go Nothing Nothing (h:t) = go (a h) Nothing t
  go (Just a) Nothing (h:t) = case b h of
    Nothing -> go Nothing Nothing (h:t)
    Just b -> go (Just a) (Just (h,b)) t
  go (Just a) (Just (bh,b)) (h:t) = case c h of
    Nothing -> go Nothing Nothing (bh:h:t)
    Just c -> Just (a,b,c)
  go a b c =
    error ("invalid state in Extractor.adjacent3:\n" ++
           show a ++ "\n" ++
           show b ++ "\n" ++
           show c ++ "\n")

adjacent4 :: (Show a, Show b, Show c, Show d, Show loc, Var v)
          => PathExtractor v loc a
          -> PathExtractor v loc b
          -> PathExtractor v loc c
          -> PathExtractor v loc d
          -> NoteExtractor v loc (a, b, c, d)
adjacent4 (PathExtractor a) (PathExtractor b) (PathExtractor c) (PathExtractor d)=
  NoteExtractor $ go Nothing Nothing Nothing . toList . C.path where
  go _ _ _ [] = Nothing
  go Nothing Nothing Nothing (h:t) = go (a h) Nothing Nothing t
  go (Just a) Nothing Nothing (h:t) = case b h of
    Nothing -> go Nothing Nothing Nothing (h:t)
    Just b -> go (Just a) (Just (h,b)) Nothing t
  go (Just a) (Just (bh,b)) Nothing (h:t) = case c h of
    Nothing -> go Nothing Nothing Nothing (bh:h:t)
    Just c -> go (Just a) (Just (bh, b)) (Just (h,c)) t
  go (Just a) (Just (bh,b)) (Just (ch,c)) (h:t) = case d h of
    Nothing -> go Nothing Nothing Nothing (bh:ch:h:t)
    Just d -> Just (a,b,c,d)
  go a b c d =
    error ("invalid state in Extractor.adjacent4:\n" ++
           show a ++ "\n" ++
           show b ++ "\n" ++
           show c ++ "\n" ++
           show d ++ "\n")


type PathPredicate v loc = C.PathElement v loc -> Bool

_fromPredicate :: (PathPredicate v loc) -> PathExtractor v loc ()
_fromPredicate e = PathExtractor (\p -> whenM (e p) (pure ()))

exactly1AppBefore :: PathExtractor v loc a -> NoteExtractor v loc a
exactly1AppBefore p = do
  (prefix, a) <- elementsUntil p
  case length . filter (isJust . runPath inSynthesizeApp) $ prefix of
    1 -> pure a
    _ -> mzero

applyingNonFunction :: NoteExtractor v loc (C.Term v loc, C.Type v loc)
applyingNonFunction = do
  (prefix, e) <- elementsUntil inSynthesize
  case elementsUntil' inSynthesizeApp prefix of
    Just ([], (ft, _argTerm, _argNum)) ->
      case e of
        Term.Apps' f _args -> pure (f, ft)
        _ -> mzero
    _ -> mzero

elementsUntil' :: PathExtractor v loc a
               -> [C.PathElement v loc]
               -> Maybe ([C.PathElement v loc], a)
elementsUntil' p l = go [] l where
  go _ [] = Nothing
  go acc (h:t) = case runPath p h of
    Just a -> Just (reverse acc, a)
    Nothing -> go (h:acc) t

elementsUntil :: PathExtractor v loc a
              -> NoteExtractor v loc ([C.PathElement v loc], a)
elementsUntil p = NoteExtractor $ elementsUntil' p . toList . C.path

inAndApp :: NoteExtractor v loc ()
inAndApp = exactly1AppBefore . PathExtractor $ \case
  C.InAndApp -> Just ()
  _ -> Nothing

inOrApp :: NoteExtractor v loc ()
inOrApp = exactly1AppBefore . PathExtractor $ \case
  C.InOrApp -> Just ()
  _ -> Nothing

inIfCond :: NoteExtractor v loc ()
inIfCond = exactly1AppBefore . PathExtractor $ \case
  C.InIfCond -> Just ()
  _ -> Nothing

inIfBody :: NoteExtractor v loc loc
inIfBody = exactly1AppBefore . PathExtractor $ \case
  C.InIfBody loc -> Just loc
  _ -> Nothing

inVectorApp :: NoteExtractor v loc loc
inVectorApp = exactly1AppBefore . PathExtractor $ \case
  C.InVectorApp loc -> Just loc
  _ -> Nothing

inMatchCaseGuard :: NoteExtractor v loc ()
inMatchCaseGuard = do
  (prefix, _) <- elementsUntil . PathExtractor $ \case
    C.InMatch _ -> Just ()
    _ -> Nothing
  -- so brittle, whee!
  if length prefix == 5 then pure () else mzero

inMatchCaseBody :: NoteExtractor v loc loc
inMatchCaseBody = do
  (prefix, loc) <- elementsUntil . PathExtractor $ \case
    C.InMatch loc -> Just loc
    _ -> Nothing
  -- so brittle, but I guess it's okay!
  if length prefix == 3 then pure loc else mzero

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

inApp :: forall v loc. (Show loc, Var v)
      => NoteExtractor v loc (C.Term v loc, -- f
                              Int, -- n
                              C.Type v loc, -- expectedType
                              C.Type v loc, -- foundType
                              C.Term v loc, -- arg
                              Maybe (C.Type v loc, [(v, C.Type v loc)]))
inApp = do
  _ctx <- typeMismatch
  ((foundType, expectedType),
   (arg, _expectedType'),
   (_solvedFnType, _arg', argNum),
   (f, _ft, _args)) <- adjacent4 inSubtype inCheck inSynthesizeApp inSynthesizeApps
  let polymorphicTypeInfo :: Maybe (C.Type v loc, [(v, C.Type v loc)])
      polymorphicTypeInfo = Nothing
      -- polymorphicTypeInfo = case f of
      --   Term.Var' v -> do
      --     rawType <- C.lookupAnn ctx v
      --     let go :: C.TypeVar v loc -> Maybe (v, C.Type v loc)
      --         go v0 = let v = TypeVar.underlying v0 in
      --                 (v,) <$> C.lookupAnn ctx v
      --         typeVars :: [C.TypeVar v loc]
      --         typeVars = (toList . ABT.freeVars $ rawType)
      --         solvedVars = catMaybes (go <$> typeVars)
      --     pure (rawType, solvedVars)
      --
      --   -- Term.Ref' r -> lookup the type
      --   -- Term.Builtin' r -> lookup the type
      --
      --   _ -> Nothing
  pure (f, argNum, expectedType, foundType, arg, polymorphicTypeInfo)




-- Causes --
typeMismatch :: NoteExtractor v loc (C.Context v loc)
typeMismatch = NoteExtractor $ \n -> case C.cause n of
  C.TypeMismatch c -> Just c
  _ -> Nothing

illFormedType :: NoteExtractor v loc (C.Context v loc)
illFormedType = NoteExtractor $ \n -> case C.cause n of
  C.IllFormedType c -> Just c
  _ -> Nothing

inSynthesizeApps :: PathExtractor v loc (C.Term v loc, C.Type v loc, [C.Term v loc])
inSynthesizeApps = PathExtractor $ \case
  C.InSynthesizeApps f ft e -> Just (f, ft,e)
  _ -> Nothing

inSynthesizeApp :: PathExtractor v loc (C.Type v loc, C.Term v loc, Int)
inSynthesizeApp = PathExtractor $ \case
  C.InSynthesizeApp t e n -> Just (t,e,n)
  _ -> Nothing

inSynthesize :: PathExtractor v loc (C.Term v loc)
inSynthesize = PathExtractor $ \case
  C.InSynthesize t -> Just t
  _ -> Nothing

inCheck :: PathExtractor v loc (C.Term v loc, C.Type v loc)
inCheck = PathExtractor $ \case
  C.InCheck e t -> Just (e,t)
  _ -> Nothing

inSubtype :: PathExtractor v loc (C.Type v loc, C.Type v loc)
inSubtype = PathExtractor $ \case
  C.InSubtype found expected -> Just (found, expected)
  _ -> Nothing

-- | Handle v

instance Functor (PathExtractor v loc) where
  fmap = liftM

instance Applicative (PathExtractor v loc) where
  (<*>) = ap
  pure = return

instance Monad (PathExtractor v loc) where
  fail _s = empty
  return a = PathExtractor (\_ -> Just a)
  PathExtractor r >>= f = PathExtractor go
    where
      go path = case r path of
        Nothing -> Nothing
        Just a -> runPath (f a) path

instance Alternative (PathExtractor v loc) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (PathExtractor v loc) where
  mzero = PathExtractor (\_ -> Nothing)
  mplus (PathExtractor f1) (PathExtractor f2) =
    PathExtractor (\note -> f1 note `mplus` f2 note)

instance Functor (NoteExtractor v loc) where
  fmap = liftM

instance Applicative (NoteExtractor v loc) where
  (<*>) = ap
  pure = return

instance Monad (NoteExtractor v loc) where
  fail _s = empty
  return a = NoteExtractor (\_ -> Just a)
  NoteExtractor r >>= f = NoteExtractor go
    where
      go note = case r note of
        Nothing -> Nothing
        Just a -> run (f a) note


instance Alternative (NoteExtractor v loc) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (NoteExtractor v loc) where
  mzero = NoteExtractor (\_ -> Nothing)
  mplus (NoteExtractor f1) (NoteExtractor f2) =
    NoteExtractor (\note -> f1 note `mplus` f2 note)
