{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Unison.Metadata where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics
import Unison.Var (Var)
import qualified Data.Text as Text
import qualified Unison.Var as Var

data Sort = Type | Term deriving (Eq,Ord,Show,Generic)

data Metadata v h =
  Metadata {
    sort :: Sort,
    names :: Names v,
    description :: Maybe h
  } deriving (Eq,Ord,Show,Generic)

combine :: Maybe (Metadata v h) -> Metadata v h -> Metadata v h
combine Nothing md2 = md2
combine (Just (Metadata _ (Names names1) desc1)) (Metadata sort (Names names2) desc2) =
  Metadata sort (Names $ names2 ++ names1) (desc2 <|> desc1)

matches :: Var v => Query -> Metadata v h -> Bool
matches (Query txt) (Metadata _ (Names ns) _) =
  any (Text.isPrefixOf txt) (map Var.name ns)

-- | Nameless metadata, contains only the annotation
synthetic :: Sort -> Metadata v h
synthetic t = Metadata t (Names []) Nothing

-- | Nameless term metadata, containing only the type annotation
syntheticTerm :: Metadata v h
syntheticTerm = synthetic Term

newtype Names v = Names [v] deriving (Eq,Ord,Show,Generic)

firstName :: Names v -> Maybe v
firstName (Names (h:_)) = Just h
firstName _ = Nothing

allNames :: Names v -> [v]
allNames (Names ns) = ns

newtype Query = Query Text

instance Show Query where
  show (Query q) = show q

queryPositions :: Query -> [Int]
queryPositions (Query q) = [0 .. (Text.length q - 1)]

instance ToJSON Query where
  toJSON (Query q) = toJSON q

instance FromJSON Query where
  parseJSON v = Query <$> parseJSON v

deriveJSON defaultOptions ''Metadata
deriveJSON defaultOptions ''Names
deriveJSON defaultOptions ''Sort
