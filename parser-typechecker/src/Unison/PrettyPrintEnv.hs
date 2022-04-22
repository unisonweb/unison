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

data PrettyPrintEnv = PrettyPrintEnv
  { -- names for terms, constructors, and requests
    terms :: Referent -> Maybe (HQ'.HashQualified Name),
    -- names for types
    types :: Reference -> Maybe (HQ'.HashQualified Name)
  }

patterns :: PrettyPrintEnv -> ConstructorReference -> Maybe (HQ'.HashQualified Name)
patterns ppe r =
  terms ppe (Referent.Con r CT.Data)
    <|> terms ppe (Referent.Con r CT.Effect)

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

-- Left-biased union of environments
unionLeft :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
unionLeft e1 e2 =
  PrettyPrintEnv
    (\r -> terms e1 r <|> terms e2 r)
    (\r -> types e1 r <|> types e2 r)

-- todo: these need to be a dynamic length, but we need additional info
todoHashLength :: Int
todoHashLength = 10

termName :: PrettyPrintEnv -> Referent -> HashQualified Name
termName env r =
  case terms env r of
    Nothing -> HQ.take todoHashLength (HQ.fromReferent r)
    Just name -> HQ'.toHQ name

typeName :: PrettyPrintEnv -> Reference -> HashQualified Name
typeName env r =
  case types env r of
    Nothing -> HQ.take todoHashLength (HQ.fromReference r)
    Just name -> HQ'.toHQ name

-- | Get a name for a LabeledDependency from the PPE.
labeledRefName :: PrettyPrintEnv -> LabeledDependency -> HashQualified Name
labeledRefName ppe = \case
  LD.TermReferent ref -> termName ppe ref
  LD.TypeReference ref -> typeName ppe ref

patternName :: PrettyPrintEnv -> ConstructorReference -> HashQualified Name
patternName env r =
  case patterns env r of
    Just name -> HQ'.toHQ name
    Nothing -> HQ.take todoHashLength $ HQ.fromPattern r

instance Monoid PrettyPrintEnv where
  mempty = PrettyPrintEnv (const Nothing) (const Nothing)
  mappend = unionLeft

instance Semigroup PrettyPrintEnv where
  (<>) = mappend
