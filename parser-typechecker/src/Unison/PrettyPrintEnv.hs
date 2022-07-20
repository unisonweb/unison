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
    addFallback,
    union,
    empty,
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
import Unison.Prelude hiding (empty)
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

-- | Attempts to find a name in primary ppe, falls back to backup ppe only if no names are
-- found.
addFallback :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
addFallback primary fallback =
  PrettyPrintEnv
    ( \r ->
        let primaryNames = termNames primary r
         in if null primaryNames
              then termNames fallback r
              else primaryNames
    )
    ( \r ->
        let primaryNames = typeNames primary r
         in if null primaryNames
              then typeNames fallback r
              else primaryNames
    )

-- | Finds names from both PPEs.
--
-- This is distinct from `addFallback` with respect to biasing;
-- A bias applied to a union might select a name in the right half of the union.
-- Whereas, a bias applied to the result of `addFallback` will bias within the available names
-- inside the left PPE and will only search in the fallback if there aren't ANY names in the
-- primary ppe.
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

empty :: PrettyPrintEnv
empty = PrettyPrintEnv mempty mempty

-- | Prefer names which share a common prefix with any provided target.
--
-- Results are sorted according to the longest common prefix found against ANY target.
biasTo :: [Name] -> PrettyPrintEnv -> PrettyPrintEnv
biasTo targets PrettyPrintEnv {termNames, typeNames} =
  PrettyPrintEnv
    { termNames = \r ->
        r
          & termNames
          & prioritizeBias targets,
      typeNames = \r ->
        r
          & typeNames
          & prioritizeBias targets
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
