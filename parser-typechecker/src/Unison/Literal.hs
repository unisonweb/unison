{-# Language DeriveGeneric #-}

module Unison.Literal where

import Data.Text (Text)
import GHC.Generics
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as Hashable

-- | Literals in the Unison language
data Literal
  = Number Double
  | Text Text
  | If
  deriving (Eq,Ord,Generic)

instance Hashable Literal where
  tokens (Number d) = [Hashable.Tag 0, Hashable.Double d]
  tokens (Text txt) = [Hashable.Tag 1, Hashable.Text txt]
  tokens If = [Hashable.Tag 2]

instance Show Literal where
  show (Text t) = show t
  show If = "if"
  show (Number n) = case floor n of
    m | fromIntegral m == n -> show (m :: Int)
    _ -> show n
