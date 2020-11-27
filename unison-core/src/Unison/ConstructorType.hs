module Unison.ConstructorType where

import Unison.Hashable (Hashable, Token (Tag), tokens)

data ConstructorType = Data | Effect deriving (Eq, Ord, Show, Enum)

instance Hashable ConstructorType where
  tokens b = [Tag . fromIntegral $ fromEnum b]
