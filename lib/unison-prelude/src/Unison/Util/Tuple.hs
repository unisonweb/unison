{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tuple utils.
module Unison.Util.Tuple
  ( drop4th,
  )
where

class Drop4th a b | a -> b where
  -- | Drop the 4th element of a tuple.
  drop4th :: a -> b

instance (x ~ (a, b, c)) => Drop4th (a, b, c, d) x where
  drop4th (a, b, c, _) = (a, b, c)
