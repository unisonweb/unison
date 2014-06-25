module Unison.Node.Metadata where

import Data.Text

data Metadata k
  = Term Names [Names] k k -- ^ @Term names paramNames description typ
  | Type Names [Names] k k -- ^ @Type names paramNames description kind
  deriving (Eq,Ord,Show,Read)

matches :: Query -> Metadata k -> Bool
matches (Query txt) (Term (Names ns) _ _ _) = txt `elem` ns
matches (Query txt) (Type (Names ns) _ _ _) = txt `elem` ns

data Names = Names [Text] deriving (Eq,Ord,Show,Read)

data Query = Query Text

-- data Examples k = Examples [(k, k)]
