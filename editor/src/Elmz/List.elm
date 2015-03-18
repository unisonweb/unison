module Elmz.List where

import List

prefixes : List a -> List (List a)
prefixes xs =
  -- rather horrible quadratic implementation
  List.map (\n -> List.take n xs) [0 .. List.length xs]
