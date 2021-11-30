{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Util.Convert where

class Convert a b where
  convert :: a -> b

class Parse a b where
  parse :: a -> Maybe b

instance (Parse a a2, Parse b b2) => Parse (a,b) (a2,b2) where
  parse (a,b) = (,) <$> parse a <*> parse b
