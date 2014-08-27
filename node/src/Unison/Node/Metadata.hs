{-# LANGUAGE TemplateHaskell #-}
module Unison.Node.Metadata where

import Data.Text (Text)
-- import Data.Map as M
import Data.Aeson.TH
import qualified Unison.Syntax.Var as V
import qualified Unison.Edit.Term.Path as P

data Sort = Type | Term deriving (Eq,Ord,Show)
data Fixity = InfixL | InfixR | Infix | Prefix deriving (Eq,Ord,Show)
data Symbol = Symbol { name :: Text, fixity :: Fixity, precedence :: Int } deriving (Eq,Ord,Show)

data Metadata k =
  Metadata {
    sort :: Sort,
    names :: Names,
    locals :: [((P.Path,V.Var), Names)],
    description :: Maybe k,
    annotation :: k
  } deriving (Eq,Ord,Show)

matches :: Query -> Metadata k -> Bool
matches (Query txt) (Metadata _ (Names ns) _ _ _) = txt `elem` map name ns

localMatches :: V.Var -> Query -> Metadata k -> Bool
localMatches v (Query txt) (Metadata _ _ m _ _) =
  let f ((_,var), Names names) | var == v = map name names
      f _ = []
  in txt `elem` (m >>= f)

-- | Nameless metadata, contains only the annotation
synthetic :: Sort -> k -> Metadata k
synthetic t ann = Metadata t (Names []) [] Nothing ann

-- | Nameless term metadata, containing only the type annotation
syntheticTerm :: k -> Metadata k
syntheticTerm = synthetic Term

data Names = Names [Symbol] deriving (Eq,Ord,Show)

data Query = Query Text

-- data Examples k = Examples [(k, k)]

deriveJSON defaultOptions ''Fixity
deriveJSON defaultOptions ''Symbol
deriveJSON defaultOptions ''Metadata
deriveJSON defaultOptions ''Names
deriveJSON defaultOptions ''Query
deriveJSON defaultOptions ''Sort
