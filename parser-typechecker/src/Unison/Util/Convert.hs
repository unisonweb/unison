module Unison.Util.Convert where
import qualified Control.Lens as Lens

-- | Meant to be used with Type Applications.
--
-- E.g.
--
-- @@
-- myText :: Text
-- myText = from @String "testing"
-- @@
from :: forall a b. Convert a b => a -> b
from = convert

-- | Meant to be used with Type Applications.
--
-- E.g.
--
-- @@
-- myText = into @Text myString
-- @@
into :: forall b a. Convert a b => a -> b
into = convert

between :: (Convert b a, Convert a b) => Lens.Iso' a b
between = Lens.iso from into

class Convert a b where
  convert :: a -> b

class Parse a b where
  parse :: a -> Maybe b

instance (Parse a a2, Parse b b2) => Parse (a,b) (a2,b2) where
  parse (a,b) = (,) <$> parse a <*> parse b
