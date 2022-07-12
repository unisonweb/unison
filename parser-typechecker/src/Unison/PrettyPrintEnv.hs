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
    Suffixify (..),
  )
where

import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
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

data PrettyPrintEnv = PrettyPrintEnv
  { -- names for terms, constructors, and requests
    termNames :: Path -> Maybe Name -> Suffixify -> Referent -> [HQ'.HashQualified Name],
    -- names for types
    typeNames :: Path -> Maybe Name -> Suffixify -> Reference -> [HQ'.HashQualified Name],
    -- allows adjusting a pretty-printer to a specific perspective
    perspective :: Path,
    -- allows biasing returned names towards a specific location
    bias :: Maybe Name,
    suffixify :: Suffixify
  }

terms :: PrettyPrintEnv -> Referent -> Maybe (HQ'.HashQualified Name)
terms PrettyPrintEnv {termNames, perspective, bias, suffixify} ref =
  listToMaybe $ termNames perspective bias suffixify ref

types :: PrettyPrintEnv -> Reference -> Maybe (HQ'.HashQualified Name)
types PrettyPrintEnv {typeNames, perspective, bias, suffixify} ref =
  listToMaybe $ typeNames perspective bias suffixify ref

patterns :: PrettyPrintEnv -> ConstructorReference -> Maybe (HQ'.HashQualified Name)
patterns ppe r =
  terms ppe (Referent.Con r CT.Data)
    <|> terms ppe (Referent.Con r CT.Effect)

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

-- Left-biased union of environments
unionLeft :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
unionLeft (PrettyPrintEnv {bias, perspective, termNames, typeNames, suffixify}) (PrettyPrintEnv {termNames = fallbackTerms, typeNames = fallbackTypes}) =
  PrettyPrintEnv
    { bias,
      perspective,
      suffixify,
      termNames = \b p suff r -> termNames b p suff r <|> fallbackTerms b p suff r,
      typeNames = \b p suff r -> typeNames b p suff r <|> fallbackTypes b p suff r
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
  mempty = PrettyPrintEnv {termNames = mempty, typeNames = mempty, bias = Nothing, perspective = Path.empty, suffixify = NoSuffixify}
  mappend = unionLeft

instance Semigroup PrettyPrintEnv where
  (<>) = mappend
