module Unison.Codebase.V1.ConstructorType where

import U.Util.Hashable (Hashable, Token(Tag), tokens)

data ConstructorType = Data | Effect deriving (Eq, Ord, Show, Enum)

instance Hashable ConstructorType where
  tokens b = [Tag . fromIntegral $ fromEnum b]
