module Unison.DataDeclaration.ConstructorId
  ( ConstructorId,
    fromNumConstructors,
  )
where

import Data.Word (Word64)

type ConstructorId = Word64

-- | Return a list of constructor ids from the number of constructors a decl has.
fromNumConstructors :: Int -> [ConstructorId]
fromNumConstructors n
  | n <= 0 = []
  | otherwise = [0 .. fromIntegral (n - 1)]
