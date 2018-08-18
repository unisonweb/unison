{-# LANGUAGE LambdaCase #-}
module Unison.Typechecker.Extractor where

import Control.Monad
import Control.Applicative
import Data.Foldable (toList)
import Data.Maybe (isJust)
import qualified Unison.Typechecker.Context as C
import Unison.Util.Monoid (whenM)

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

_adjacent :: PathExtractor v loc a -> PathExtractor v loc b -> NoteExtractor v loc (a, b)
_adjacent (PathExtractor a) (PathExtractor b) =
  NoteExtractor $ go Nothing . toList . C.path where
  go _ [] = Nothing
  go Nothing (h:t) = go (a h) t
  go (Just a) (h:t) = case b h of Nothing -> go Nothing t; Just b -> Just (a,b)

type PathPredicate v loc = C.PathElement v loc -> Bool

_fromPredicate :: (PathPredicate v loc) -> PathExtractor v loc ()
_fromPredicate e = PathExtractor (\p -> whenM (e p) (pure ()))

exactly1AppBefore :: PathExtractor v loc a -> NoteExtractor v loc a
exactly1AppBefore p = do
  (prefix, a) <- elementsUntil p
  case length . filter (isJust . runPath inSynthesizeApp) $ prefix of
    1 -> pure a
    _ -> mzero

elementsUntil :: PathExtractor v loc a
              -> NoteExtractor v loc ([C.PathElement v loc], a)
elementsUntil p = NoteExtractor $ go [] . toList . C.path where
  go _ [] = Nothing
  go acc (h:t) = case runPath p h of
    Just a -> Just (reverse acc, a)
    Nothing -> go (h:acc) t

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


inSynthesizeApp :: PathExtractor v loc (C.Type v loc, C.Term v loc)
inSynthesizeApp = PathExtractor $ \case
  C.InSynthesizeApp t e -> Just (t,e)
  _ -> Nothing

-- App
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
