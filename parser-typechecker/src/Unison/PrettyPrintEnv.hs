{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv
  ( PrettyPrintEnv (..),
    patterns,
    patternName,
    terms,
    types,
    allTermNames,
    allTypeNames,
    termName,
    typeName,
    termNameOrHashOnly,
    typeNameOrHashOnly,
    biasTo,
    labeledRefName,
    -- | Exported only for cases where the codebase's configured hash length is unavailable.
    todoHashLength,
    addFallback,
    union,
    empty,
    PrettyPrint (..),
    usePPE,
  )
where

import Control.Monad.Reader
import Data.Ord (Down (Down))
import Data.Semigroup (Max (Max))
import Unison.ConstructorReference (ConstructorReference)
import qualified Unison.ConstructorType as CT
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Prelude hiding (empty)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent

class Monad m => PrettyPrint m where
  termNames :: Referent -> m [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
  typeNames :: Reference -> m [(HQ'.HashQualified Name, HQ'.HashQualified Name)]

instance Monad m => PrettyPrint (ReaderT (PrettyPrintEnv m) m) where
  termNames r = do
    ppe <- ask
    lift $ termNames' ppe r
  typeNames r = do
    ppe <- ask
    lift $ typeNames' ppe r

usePPE :: PrettyPrintEnv m -> ReaderT (PrettyPrintEnv m) m a -> m a
usePPE ppe m = runReaderT m ppe

data PrettyPrintEnv m = PrettyPrintEnv
  { -- names for terms, constructors, and requests; e.g. [(original name, relativized and/or suffixified pretty name)]
    termNames' :: Referent -> m [(HQ'.HashQualified Name, HQ'.HashQualified Name)],
    -- names for types; e.g. [(original name, possibly suffixified name)]
    typeNames' :: Reference -> m [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
  }

allTermNames :: PrettyPrint m => Referent -> m [HQ'.HashQualified Name]
allTermNames ref = fmap snd <$> termNames ref

allTypeNames :: PrettyPrint m => Reference -> m [HQ'.HashQualified Name]
allTypeNames ref = fmap snd <$> typeNames ref

terms :: PrettyPrint m => Referent -> m (Maybe (HQ'.HashQualified Name))
terms ref = fmap snd . listToMaybe <$> termNames ref

types :: PrettyPrint m => Reference -> m (Maybe (HQ'.HashQualified Name))
types ref = fmap snd . listToMaybe <$> typeNames ref

termNameOrHashOnly :: PrettyPrint m => Referent -> m (HQ.HashQualified Name)
termNameOrHashOnly r = maybe (HQ.fromReferent r) HQ'.toHQ <$> terms r

typeNameOrHashOnly :: PrettyPrint m => Reference -> m (HQ.HashQualified Name)
typeNameOrHashOnly r = maybe (HQ.fromReference r) HQ'.toHQ <$> types r

patterns :: PrettyPrint m => ConstructorReference -> m (Maybe (HQ'.HashQualified Name))
patterns r =
  liftA2
    (<|>)
    (terms (Referent.Con r CT.Data))
    (terms (Referent.Con r CT.Effect))

instance Show (PrettyPrintEnv m) where
  show _ = "PrettyPrintEnv m"

-- | Attempts to find a name in primary ppe, falls back to backup ppe only if no names are
-- found. Typically one can use this to shadow global or absolute names with names that are
-- within the current path.
addFallback :: (Monad m) => PrettyPrintEnv m -> PrettyPrintEnv m -> PrettyPrintEnv m
addFallback primary fallback =
  PrettyPrintEnv
    ( \r -> do
        primaryNames <- termNames' primary r
        if null primaryNames
          then termNames' fallback r
          else pure primaryNames
    )
    ( \r -> do
        primaryNames <- typeNames' primary r
        if null primaryNames
          then typeNames' fallback r
          else pure primaryNames
    )

-- | Finds names from both PPEs, if left unbiased the name from the left ppe is preferred.
--
-- This is distinct from `addFallback` with respect to biasing;
-- A bias applied to a union might select a name in the right half of the union.
-- Whereas, a bias applied to the result of `addFallback` will bias within the available names
-- inside the left PPE and will only search in the fallback if there aren't ANY names in the
-- primary ppe.
--
-- If you don't know the difference, it's likely you want 'addFallback' where you add global
-- names as a fallback for local names.
union :: Applicative m => PrettyPrintEnv m -> PrettyPrintEnv m -> PrettyPrintEnv m
union e1 e2 =
  PrettyPrintEnv
    (\r -> liftA2 (++) (termNames' e1 r) (termNames' e2 r))
    (\r -> liftA2 (++) (typeNames' e1 r) (typeNames' e2 r))

-- todo: these need to be a dynamic length, but we need additional info
todoHashLength :: Int
todoHashLength = 10

termName :: PrettyPrint m => Referent -> m (HashQualified Name)
termName r =
  terms r <&> \case
    Nothing -> HQ.take todoHashLength (HQ.fromReferent r)
    Just name -> HQ'.toHQ name

typeName :: PrettyPrint m => Reference -> m (HashQualified Name)
typeName r =
  types r <&> \case
    Nothing -> HQ.take todoHashLength (HQ.fromReference r)
    Just name -> HQ'.toHQ name

-- | Get a name for a LabeledDependency from the PPE.
labeledRefName :: PrettyPrint m => LabeledDependency -> m (HashQualified Name)
labeledRefName = \case
  LD.TermReferent ref -> termName ref
  LD.TypeReference ref -> typeName ref

patternName :: PrettyPrint m => ConstructorReference -> m (HashQualified Name)
patternName r =
  patterns r <&> \case
    Just name -> HQ'.toHQ name
    Nothing -> HQ.take todoHashLength $ HQ.fromPattern r

empty :: Applicative m => PrettyPrintEnv m
empty = PrettyPrintEnv (const (pure [])) (const (pure []))

-- | Prefer names which share a common prefix with any provided target.
--
-- Results are sorted according to the longest common prefix found against ANY target.
biasTo :: Functor m => [Name] -> PrettyPrintEnv m -> PrettyPrintEnv m
biasTo targets PrettyPrintEnv {termNames', typeNames'} =
  PrettyPrintEnv
    { termNames' = \r ->
        r
          & termNames'
          & fmap (prioritizeBias targets),
      typeNames' = \r ->
        r
          & typeNames'
          & fmap (prioritizeBias targets)
    }

-- | Prefer names which share a common prefix with any provided target.
--
-- Results are sorted according to the longest common prefix found against ANY target.
--
-- >>> prioritizeBias ["a.b", "x"] [(HQ'.unsafeFromText "q", ()), (HQ'.unsafeFromText "x.y", ()), (HQ'.unsafeFromText "a.b.c", ())]
-- [(a.b.c,()),(x.y,()),(q,())]
--
-- Sort is stable if there are no common prefixes
-- >>> prioritizeBias ["not-applicable"] [(HQ'.unsafeFromText "q", ()), (HQ'.unsafeFromText "a.b.c", ()), (HQ'.unsafeFromText "x", ())]
-- [(q,()),(a.b.c,()),(x,())]
prioritizeBias :: [Name] -> [(HQ'.HashQualified Name, a)] -> [(HQ'.HashQualified Name, a)]
prioritizeBias targets =
  sortOn \(fqn, _) ->
    targets
      & foldMap
        ( \target ->
            Just (Max (length $ Name.commonPrefix target (HQ'.toName fqn)))
        )
      & fromMaybe 0
      & Down -- Sort large common prefixes highest
