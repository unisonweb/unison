module Unison.Util.Convert where

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

-- | Meant to be used with Type Applications.
--
-- E.g.
--
-- @@
-- myName :: Name
-- myName = parseFrom @String "testing"
-- @@
parseFrom :: forall a b. Parse a b => a -> Maybe b
parseFrom = parse

-- | Meant to be used with Type Applications.
--
-- E.g.
--
-- @@
-- myName = parseInto @Name myString
-- @@
parseInto :: forall b a. Parse a b => a -> Maybe b
parseInto = parse

class Convert a b where
  convert :: a -> b

class Parse a b where
  parse :: a -> Maybe b

instance (Parse a a2, Parse b b2) => Parse (a,b) (a2,b2) where
  parse (a,b) = (,) <$> parse a <*> parse b
