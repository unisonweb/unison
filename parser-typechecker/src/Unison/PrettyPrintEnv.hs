{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Unison.Reference (Reference)
import qualified Data.Map as Map
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import Unison.Names (Names)
import qualified Unison.Names as Names
import qualified Unison.Names2 as Names2
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent

data PrettyPrintEnv = PrettyPrintEnv {
  -- names for terms, constructors, and requests
  terms :: Referent -> Maybe HashQualified,
  -- names for types
  types :: Reference -> Maybe HashQualified }

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

fromNames :: Names -> PrettyPrintEnv
fromNames ns =
  let terms =
        Map.fromList [ (r, HQ.fromName n) | (n, r) <- Map.toList (Names.termNames ns) ]
      types =
        Map.fromList [ (r, HQ.fromName n) | (n, r) <- Map.toList (Names.typeNames ns) ]
  in PrettyPrintEnv (`Map.lookup` terms) (`Map.lookup` types)

fromNames2 :: Names2.Names -> PrettyPrintEnv
fromNames2 = fromNames . Names.fromNames2

-- Left-biased union of environments
unionLeft :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
unionLeft e1 e2 = PrettyPrintEnv
  (\r -> terms e1 r <|> terms e2 r)
  (\r -> types e1 r <|> types e2 r)

assignTermName :: Referent -> HashQualified -> PrettyPrintEnv -> PrettyPrintEnv
assignTermName r name = (fromTermNames [(r,name)] `unionLeft`)

fromTypeNames :: [(Reference,HashQualified)] -> PrettyPrintEnv
fromTypeNames types = let
  m = Map.fromList types
  in PrettyPrintEnv (const Nothing) (`Map.lookup` m)

fromTermNames :: [(Referent,HashQualified)] -> PrettyPrintEnv
fromTermNames tms = let
  m = Map.fromList tms
  in PrettyPrintEnv (`Map.lookup` m) (const Nothing)

-- todo: these need to be a dynamic length, but we need additional info
todoHashLength :: Int
todoHashLength = 10
termName :: PrettyPrintEnv -> Referent -> HashQualified
termName env r =
  fromMaybe (HQ.take todoHashLength $ HQ.fromReferent r) (terms env r)

typeName :: PrettyPrintEnv -> Reference -> HashQualified
typeName env r =
  fromMaybe (HQ.take todoHashLength $ HQ.fromReference r) (types env r)

patternName :: PrettyPrintEnv -> Reference -> Int -> HashQualified
patternName env r cid =
  case terms env (Referent.Con r cid) of
    Just name -> name
    Nothing -> HQ.take todoHashLength $ HQ.fromReferent (Referent.Con r cid)

instance Monoid PrettyPrintEnv where
  mempty = PrettyPrintEnv (const Nothing) (const Nothing)
  mappend = unionLeft
instance Semigroup PrettyPrintEnv where
  (<>) = mappend
