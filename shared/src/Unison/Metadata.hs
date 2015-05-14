{-# LANGUAGE TemplateHaskell #-}
module Unison.Metadata where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Unison.Symbol (Symbol)
import qualified Data.Text as Text
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term

data Sort = Type | Term deriving (Eq,Ord,Show)

data Metadata k =
  Metadata {
    sort :: Sort,
    names :: Names,
    locals :: [(Term.Path, Symbol)],
    description :: Maybe k
  } deriving (Eq,Ord,Show)

matches :: Query -> Metadata k -> Bool
matches (Query txt) (Metadata _ (Names ns) _ _) =
  any (Text.isPrefixOf txt) (map Symbol.name ns)

-- | Nameless metadata, contains only the annotation
synthetic :: Sort -> Metadata k
synthetic t = Metadata t (Names []) [] Nothing

-- | Nameless term metadata, containing only the type annotation
syntheticTerm :: Metadata k
syntheticTerm = synthetic Term

data Names = Names [Symbol] deriving (Eq,Ord,Show)

data Query = Query Text

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
