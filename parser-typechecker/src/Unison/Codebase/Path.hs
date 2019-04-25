module Unison.Codebase.Path
  -- ( Name(..)
  -- , unsafeFromText
  -- , toString
  -- , fromString
  -- , toVar
  -- , unsafeFromVar
  -- , isPrefixOf
  -- , stripPrefix
  -- )
where

import           Data.List                (intercalate)
-- import           Data.String                    ( IsString
--                                                 , fromString
--                                                 )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)

-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
newtype Path = Path { toList :: [NameSegment] } deriving (Eq, Ord)

instance Show Path where
  show (Path nss) = intercalate "/" $ fmap escape1 nss
    where escape1 ns = escape =<< (Text.unpack . toText $ ns)
          escape = \case '/' -> "\\/"; c -> [c]

-- unsafeFromString :: NameSegment ->
-- toString :: NameSegment -> String
-- toString = Text.unpack . toText
--
-- isPrefixOf :: Name -> Name -> Bool
-- a `isPrefixOf` b = toText a `Text.isPrefixOf` toText b
--
-- stripPrefix :: Name -> Name -> Maybe Name
-- stripPrefix prefix name =
--   Name <$> Text.stripPrefix (toText prefix) (toText name)
--
-- instance Show Name where
--   show = toString
--
-- instance IsString Name where
--   fromString = unsafeFromText . Text.pack
--
instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]
