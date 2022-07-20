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
    fallback,
  )
where

import Data.Ord (Down (Down))
import Data.Semigroup (Max (Max))
import Unison.ConstructorReference (ConstructorReference)
import qualified Unison.ConstructorType as CT
import qualified Unison.Debug as Debug
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
fallback :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
fallback e1 e2 =
  PrettyPrintEnv
    ( \r ->
        let primary = termNames e1 r
         in if null primary
              then termNames e2 r
              else primary
    )
    ( \r ->
        let primary = typeNames e1 r
         in if null primary
              then typeNames e2 r
              else primary
    )

union :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
union e1 e2 =
  PrettyPrintEnv
    (termNames e1 <> termNames e2)
    (typeNames e1 <> typeNames e2)

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

instance Semigroup PrettyPrintEnv where
  (<>) = union

biasTo :: [Name] -> PrettyPrintEnv -> PrettyPrintEnv
biasTo targets PrettyPrintEnv {termNames, typeNames} =
  PrettyPrintEnv
    { termNames = \r ->
        r
          & termNames
          & prioritizeBias targets
          & Debug.debugLog Debug.Names (show ("Biased to:" :: String, targets)),
      typeNames = \r ->
        r
          & typeNames
          & prioritizeBias targets
          & Debug.debugLog Debug.Names (show ("Biased to:" :: String, targets))
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
