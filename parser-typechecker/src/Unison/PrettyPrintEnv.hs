{-# Language OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.PrettyPrintEnv where

import           Control.Applicative            ( (<|>) )
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Debug.Trace                    ( trace )
import           Unison.HashQualified           ( HashQualified )
import           Unison.Name                    ( Name )
import           Unison.Names3                  ( Names )
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent )
import           Unison.Util.List               (safeHead)
import qualified Data.Map                      as Map
import qualified Unison.HashQualified          as HQ
import qualified Unison.Name                   as Name
import qualified Unison.Names3                 as Names
import qualified Unison.Referent               as Referent

data PrettyPrintEnv = PrettyPrintEnv {
  -- names for terms, constructors, and requests
  terms :: Referent -> Maybe HashQualified,
  -- names for types
  types :: Reference -> Maybe HashQualified }

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

fromNames :: Int -> Names -> PrettyPrintEnv
fromNames length names = PrettyPrintEnv terms' types' where
  terms' r = safeHead (Names.termName length r names)
  types' r = safeHead (Names.typeName length r names)

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

-- Type aliases relating to Fully-Qualified Names, e.g. 'Acme.API.foo'
-- Used primarily by the FQN elision code - see TermPrinter.PrintAnnotation.

-- Note that a Suffix can include dots.
type Suffix = Text
-- Each member of a Prefix list is dot-free.
type Prefix = [Text]
-- Keys are FQNs, values are shorter names which are equivalent, thanks to use
-- statements that are in scope.
type Imports = Map Name Suffix

-- Give the shortened version of an FQN, if there's been a `use` statement for that FQN.
elideFQN :: Imports -> HQ.HashQualified -> HQ.HashQualified
elideFQN imports hq =
  let hash = HQ.toHash hq
      name' = do name <- HQ.toName hq
                 let hit = fmap Name.unsafeFromText (Map.lookup name imports)
                 -- Cut out the "const id $" to get tracing of FQN elision attempts.
                 let t = const id $ trace ("hit: " ++ show hit ++ " finding: " ++ show hq ++ " in imports: " ++ show imports)
                 t (pure $ fromMaybe name hit)
  in HQ.fromNameHash name' hash
