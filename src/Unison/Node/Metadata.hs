module Unison.Node.Metadata where

import Data.Text
import Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Unison.Syntax.Var as V

data Sort = Type | Term deriving (Eq,Ord,Show)

data Metadata k =
  Metadata {
    sort :: Sort,
    names :: Names,
    locals :: Map V.Var Names,
    description :: k,
    annotation :: k
  } deriving (Eq,Ord,Show)

matches :: Query -> Metadata k -> Bool
matches (Query txt) (Metadata _ (Names ns) _ _ _) = txt `elem` ns

localMatches :: V.Var -> Query -> Metadata k -> Bool
localMatches v (Query txt) (Metadata _ _ m _ _) =
  txt `elem` (let Names ns = fromMaybe (Names []) (M.lookup v m) in ns)

data Names = Names [Text] deriving (Eq,Ord,Show,Read)

data Query = Query Text

-- data Examples k = Examples [(k, k)]
