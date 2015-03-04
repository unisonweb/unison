{-# LANGUAGE TemplateHaskell #-}
module Unison.Node.Metadata where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
-- import Data.Map as M
import Data.Aeson.TH
import Data.Aeson
import qualified Unison.Edit.Term.Path as P

data Sort = Type | Term deriving (Eq,Ord,Show)
data Fixity = InfixL | InfixR | Infix | Prefix deriving (Eq,Ord,Show)
data Symbol = Symbol { name :: Text, fixity :: Fixity, precedence :: Int } deriving (Eq,Ord,Show)

data Metadata k =
  Metadata {
    sort :: Sort,
    names :: Names,
    locals :: [(P.Path, Symbol)],
    description :: Maybe k
  } deriving (Eq,Ord,Show)

matches :: Query -> Metadata k -> Bool
matches (Query txt) (Metadata _ (Names ns) _ _) = txt `elem` map name ns

{-
localMatches :: V.Var -> Query -> Metadata k -> Bool
localMatches v (Query txt) (Metadata _ _ m _ _) =
  txt `elem` [ name sym | (var, ns) <- m,
                          var == v,
                          (_, Names syms) <- ns,
                          sym <- syms
             ]
-}

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

-- data Examples k = Examples [(k, k)]

deriveJSON defaultOptions ''Fixity
deriveJSON defaultOptions ''Symbol
deriveJSON defaultOptions ''Metadata
deriveJSON defaultOptions ''Names
deriveJSON defaultOptions ''Sort
