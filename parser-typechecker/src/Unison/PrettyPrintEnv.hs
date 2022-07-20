{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv
  ( PrettyPrintEnv (..),
    patterns,
    patternName,
    terms,
    types,
    termName,
    typeName,
    biasTo,
    labeledRefName,
    -- | Exported only for cases where the codebase's configured hash length is unavailable.
    todoHashLength,
  )
where

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
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent

data PrettyPrintEnv = PrettyPrintEnv
  { -- names for terms, constructors, and requests; e.g. [(original name, relativized and/or suffixified pretty name)]
    termNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)],
    -- names for types; e.g. [(original name, relativized and/or suffixified pretty name)]
    typeNames :: Reference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
  }

terms :: PrettyPrintEnv -> Referent -> Maybe (HQ'.HashQualified Name)
terms ppe = fmap snd . listToMaybe . termNames ppe

types :: PrettyPrintEnv -> Reference -> Maybe (HQ'.HashQualified Name)
types ppe = fmap snd . listToMaybe . typeNames ppe

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
    (\r -> termNames e1 r <|> termNames e2 r)
    (\r -> typeNames e1 r <|> typeNames e2 r)

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
  mempty = PrettyPrintEnv (const []) (const [])
  mappend = unionLeft

instance Semigroup PrettyPrintEnv where
  (<>) = mappend

biasTo :: [Name] -> PrettyPrintEnv -> PrettyPrintEnv
biasTo targets PrettyPrintEnv {termNames, typeNames} =
  PrettyPrintEnv
    { termNames = prioritizeBias targets . termNames,
      typeNames = prioritizeBias targets . typeNames
    }

prioritizeBias :: [Name] -> [(HQ'.HashQualified Name, a)] -> [(HQ'.HashQualified Name, a)]
prioritizeBias targets =
  sortOn \(fqn, _) ->
    targets
      & foldMap
        ( \target ->
            Just (Max (Name.commonPrefix target (HQ'.toName fqn)))
        )
      & Down
