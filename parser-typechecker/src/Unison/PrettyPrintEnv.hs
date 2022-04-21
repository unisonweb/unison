{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv
  ( PrettyPrintEnv (..),
    patterns,
    patternName,
    termName,
    typeName,
    labeledRefName,
    -- | Exported only for cases where the codebase's configured hash length is unavailable.
    todoHashLength,
  )
where

import Unison.ConstructorReference (ConstructorReference)
import qualified Unison.ConstructorType as CT
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent

data PrettyPrintEnv m = PrettyPrintEnv
  { -- names for terms, constructors, and requests
    terms :: Referent -> m (Maybe (HQ'.HashQualified Name)),
    -- names for types
    types :: Reference -> m (Maybe (HQ'.HashQualified Name))
  }

patterns :: Applicative m => PrettyPrintEnv m -> ConstructorReference -> m (Maybe (HQ'.HashQualified Name))
patterns ppe r =
  liftA2
    (<|>)
    (terms ppe (Referent.Con r CT.Data))
    (terms ppe (Referent.Con r CT.Effect))

instance Show (PrettyPrintEnv m) where
  show _ = "PrettyPrintEnv"

-- Left-biased union of environments
unionLeft :: Applicative m => PrettyPrintEnv m -> PrettyPrintEnv m -> PrettyPrintEnv m
unionLeft e1 e2 =
  PrettyPrintEnv
    (\r -> liftA2 (<|>) (terms e1 r) (terms e2 r))
    (\r -> liftA2 (<|>) (types e1 r) (types e2 r))

-- todo: these need to be a dynamic length, but we need additional info
todoHashLength :: Int
todoHashLength = 10

termName :: Functor m => PrettyPrintEnv m -> Referent -> m (HashQualified Name)
termName env r =
  terms env r <&> \case
    Nothing -> HQ.take todoHashLength (HQ.fromReferent r)
    Just name -> HQ'.toHQ name

typeName :: Functor m => PrettyPrintEnv m -> Reference -> m (HashQualified Name)
typeName env r =
  types env r <&> \case
    Nothing -> HQ.take todoHashLength (HQ.fromReference r)
    Just name -> HQ'.toHQ name

-- | Get a name for a LabeledDependency from the PPE.
labeledRefName :: Functor m => PrettyPrintEnv m -> LabeledDependency -> m (HashQualified Name)
labeledRefName ppe = \case
  LD.TermReferent ref -> termName ppe ref
  LD.TypeReference ref -> typeName ppe ref

patternName :: (Functor m, Applicative m) => PrettyPrintEnv m -> ConstructorReference -> m (HashQualified Name)
patternName env r =
  patterns env r <&> \case
    Just name -> HQ'.toHQ name
    Nothing -> HQ.take todoHashLength $ HQ.fromPattern r

instance Applicative m => Monoid (PrettyPrintEnv m) where
  mempty = PrettyPrintEnv (const $ pure Nothing) (const $ pure Nothing)

instance Applicative m => Semigroup (PrettyPrintEnv m) where
  (<>) = unionLeft
