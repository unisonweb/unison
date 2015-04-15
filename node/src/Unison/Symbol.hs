{-# LANGUAGE TemplateHaskell #-}
module Unison.Symbol where

import Data.Aeson.TH
import Data.Text (Text)

data Fixity = InfixL | InfixR | Infix | Prefix deriving (Eq,Ord,Show)
data Symbol = Symbol { name :: Text, freshId :: !Int, fixity :: !Fixity, precedence :: !Int } deriving (Eq,Ord,Show)

symbol :: Text -> Fixity -> Int -> Symbol
symbol n f p = Symbol n 0 f p

freshen :: Symbol -> Symbol
freshen (Symbol n i f p) = Symbol n (i+1) f p

prefix :: Text -> Symbol
prefix name = symbol name Prefix 9

deriveJSON defaultOptions ''Fixity
deriveJSON defaultOptions ''Symbol
