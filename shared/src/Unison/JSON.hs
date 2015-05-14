-- | Various JSON-related classes and helper functions not provided by Aeson

module Unison.JSON where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Data.Vector ((!?))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as Aeson
import qualified Data.Vector as Vector

class ToJSON1 f where
  toJSON1 :: ToJSON a => f a -> Aeson.Value

class FromJSON1 f where
  parseJSON1 :: FromJSON a => Aeson.Value -> Aeson.Parser (f a)

text :: Text -> Aeson.Value
text t = toJSON t

array :: [Aeson.Value] -> Aeson.Value
array = Aeson.Array . Vector.fromList

-- | Run the parser on the nth (0-based) subtree, assuming the input is an array
at :: Int -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
at ind parse j = J.withArray "at" k j where
  k vs = maybe z parse (vs !? ind) where z = fail ("invalid index: " ++ show ind)

-- | Run the parser on the 0th subtree, assuming the input is an array
at0 :: (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser a
at0 = at 0
