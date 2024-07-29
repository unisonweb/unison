module Unison.Util.Defn
  ( Defn (..),
  )
where

-- | A "definition" is either a term or a type.
data Defn term typ
  = TermDefn term
  | TypeDefn typ
