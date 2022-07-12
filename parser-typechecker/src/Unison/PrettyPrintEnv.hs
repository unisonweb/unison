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
    suffixifiedPPE,
    unsuffixifiedPPE,
    Suffixify (..),
    Perspective (..),
  )
where

import Data.Set.NonEmpty (NESet)
import Unison.Codebase.Path (Path)
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

data Suffixify
  = Suffixify
  | NoSuffixify
  deriving (Show, Eq, Ord)

data Perspective
  = -- Make names in Path relative, and prefer those names, but fall back to external names
    RelativeTo Path
  | -- View everything from the root perspective with fully qualified names.
    Root
  deriving (Show, Eq, Ord)

data PrettyPrintEnv = PrettyPrintEnv
  { -- names for terms, constructors, and requests
    termNames :: Maybe (NESet Path) -> Perspective -> Maybe Name -> Suffixify -> Referent -> [HQ'.HashQualified Name],
    -- names for types
    typeNames :: Maybe (NESet Path) -> Perspective -> Maybe Name -> Suffixify -> Reference -> [HQ'.HashQualified Name],
    -- allows adjusting a pretty-printer to a specific perspective
    perspective :: Perspective,
    -- Optionally restrict all names to names contained within any of these paths
    restrictions :: Maybe (NESet Path),
    -- allows biasing returned names towards a specific location
    bias :: Maybe Name,
    suffixify :: Suffixify
  }

-- -- | Freeze a PPE so it ignores future settings. This is helpful when combining PPEs.
-- freeze :: PrettyPrintEnv -> PrettyPrintEnv
-- freeze ppe@PrettyPrintEnv {termNames, typeNames, perspective, restrictions, bias, suffixify} =
--   ppe
--     { termNames = \_ _ _ _ ref -> termNames restrictions perspective bias suffixify ref,
--       typeNames = \_ _ _ _ ref -> typeNames restrictions perspective bias suffixify ref
--     }

---- | Try to find names in the first PPE, if any are found, return them.
---- Otherwise, return the result of the second PPE.
----
---- Note that unlike the semigroup instance, this does NOT include names from the second PPE
---- if the first PPE has some names.
--withFallback :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
--withFallback primary fallback =

suffixifiedPPE :: PrettyPrintEnv -> PrettyPrintEnv
suffixifiedPPE ppe = ppe {suffixify = Suffixify}

unsuffixifiedPPE :: PrettyPrintEnv -> PrettyPrintEnv
unsuffixifiedPPE ppe = ppe {suffixify = NoSuffixify}

terms :: PrettyPrintEnv -> Referent -> Maybe (HQ'.HashQualified Name)
terms PrettyPrintEnv {termNames, perspective, bias, suffixify, restrictions} ref =
  listToMaybe $ termNames restrictions perspective bias suffixify ref

types :: PrettyPrintEnv -> Reference -> Maybe (HQ'.HashQualified Name)
types PrettyPrintEnv {typeNames, perspective, bias, suffixify, restrictions} ref =
  listToMaybe $ typeNames restrictions perspective bias suffixify ref

patterns :: PrettyPrintEnv -> ConstructorReference -> Maybe (HQ'.HashQualified Name)
patterns ppe r =
  terms ppe (Referent.Con r CT.Data)
    <|> terms ppe (Referent.Con r CT.Effect)

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

-- Left-biased union of environments
unionLeft :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
unionLeft (PrettyPrintEnv {bias, perspective, termNames, typeNames, suffixify, restrictions}) (PrettyPrintEnv {termNames = fallbackTerms, typeNames = fallbackTypes}) =
  PrettyPrintEnv
    { bias,
      perspective,
      suffixify,
      restrictions,
      termNames = \restr p b suff r -> termNames restr p b suff r <|> fallbackTerms restr p b suff r,
      typeNames = \restr p b suff r -> typeNames restr p b suff r <|> fallbackTypes restr p b suff r
    }

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
  mempty = PrettyPrintEnv {termNames = mempty, typeNames = mempty, bias = Nothing, perspective = Root, suffixify = NoSuffixify, restrictions = Nothing}
  mappend = unionLeft

instance Semigroup PrettyPrintEnv where
  (<>) = mappend
