{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv (PrettyPrintEnv(..), patterns, patternName, termName, typeName) where

import Unison.Prelude

import           Unison.HashQualified           ( HashQualified )
import           Unison.Name                    ( Name )
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent )
import qualified Unison.HashQualified          as HQ
import qualified Unison.Referent               as Referent
import qualified Unison.ConstructorType as CT

data PrettyPrintEnv = PrettyPrintEnv {
  -- names for terms, constructors, and requests
  terms :: Referent -> Maybe (HashQualified Name),
  -- names for types
  types :: Reference -> Maybe (HashQualified Name) }

patterns :: PrettyPrintEnv -> Reference -> Int -> Maybe (HashQualified Name)
patterns ppe r cid = terms ppe (Referent.Con r cid CT.Data)
                  <|>terms ppe (Referent.Con r cid CT.Effect)

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

-- Left-biased union of environments
unionLeft :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
unionLeft e1 e2 = PrettyPrintEnv
  (\r -> terms e1 r <|> terms e2 r)
  (\r -> types e1 r <|> types e2 r)

-- todo: these need to be a dynamic length, but we need additional info
todoHashLength :: Int
todoHashLength = 10

termName :: PrettyPrintEnv -> Referent -> HashQualified Name
termName env r =
  fromMaybe (HQ.take todoHashLength $ HQ.fromReferent r) (terms env r)

typeName :: PrettyPrintEnv -> Reference -> HashQualified Name
typeName env r =
  fromMaybe (HQ.take todoHashLength $ HQ.fromReference r) (types env r)

patternName :: PrettyPrintEnv -> Reference -> Int -> HashQualified Name
patternName env r cid =
  case patterns env r cid of
    Just name -> name
    Nothing -> HQ.take todoHashLength $ HQ.fromPattern r cid

instance Monoid PrettyPrintEnv where
  mempty = PrettyPrintEnv (const Nothing) (const Nothing)
  mappend = unionLeft
instance Semigroup PrettyPrintEnv where
  (<>) = mappend
