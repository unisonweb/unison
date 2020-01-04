{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Wwarn #-} -- temporary

module Unison.Name
  ( Name
    -- * Conversion functions
    -- ** To name
  , fromNameSegment
  , fromVar
  , unsafeFromText
    -- ** From name
  , toString
  , toText
  , toVar
    -- * Name API
  , asRelative
  , countDots
  , isAbsolute
  , isLower
  , isPrefixOf
  , joinDot
  , makeAbsolute
  , oldSplits
  , parent
  , segments
  , sortNamed
  , sortNamed'
  , splits
  , stripNamePrefix
  , suffixes
  , unqualified
  )
where

import Unison.Prelude

import           Control.Arrow                  ( (***) )
import           Control.Lens                   ( unsnoc )
import qualified Data.Char                     as Char
import           Data.List                      ( inits, intersperse )
import qualified Data.List                     as List
import           Data.List.Extra                ( dropEnd )
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Text                     as Text
import           Unison.Codebase.NameSegment    ( NameSegment )
import qualified Unison.Codebase.NameSegment   as NameSegment
import qualified Unison.Hashable               as H
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Data.RFC5051                  as RFC5051
import           Data.List                      ( sortBy, tails )

data Name
  = Name Text
  | Name' Placement (NonEmpty NameSegment)
  deriving stock (Eq, Ord, Show)

data Placement
  = Absolute
  | Relative
  deriving stock (Eq, Ord, Show)

toText :: Name -> Text
toText (Name name) = name
toText (Name' placement names) =
  f (Text.intercalate "." (map NameSegment.toText (toList names)))
  where
    f =
      case placement of
        Absolute -> Text.cons '.'
        Relative -> id

fromNameSegment :: NameSegment -> Name
fromNameSegment = unsafeFromText . NameSegment.toText

sortNamed :: (a -> Name) -> [a] -> [a]
sortNamed by as = let
  as' = [ (a, Text.unpack (toText (by a))) | a <- as ]
  comp (_,s) (_,s2) = RFC5051.compareUnicode s s2
  in fst <$> sortBy comp as'

-- | Like sortNamed, but takes an additional backup comparison function if two
-- names are equal.
sortNamed' :: (a -> Name) -> (a -> a -> Ordering) -> [a] -> [a]
sortNamed' by by2 as = let
  as' = [ (a, Text.unpack (toText (by a))) | a <- as ]
  comp (a,s) (a2,s2) = RFC5051.compareUnicode s s2 <> by2 a a2
  in fst <$> sortBy comp as'

unsafeFromText :: Text -> Name
unsafeFromText t =
  if Text.any (== '#') t then error $ "not a name: " <> show t else Name t

toVar :: Var v => Name -> v
toVar (Name t) = Var.named t

fromVar :: Var v => v -> Name
fromVar = unsafeFromText . Var.name

toString :: Name -> String
toString = Text.unpack . toText

isPrefixOf :: HasCallStack => Name -> Name -> Bool
Name a `isPrefixOf` Name b = a `Text.isPrefixOf` b
n1@(Name' pa a) `isPrefixOf` n2@(Name' pb b) =
  if pa == pb then
    toList a `NonEmpty.isPrefixOf` b
  else
    error ("isPrefixOf (" ++ show n1 ++ ") (" ++ show n2 ++ ")")

-- stripNamePrefix a.b  a.b.c = Just c
-- stripNamePrefix a.b. a.b.c = undefined, "a.b." isn't a valid name IMO
-- stripNamePrefix x.y  a.b.c = Nothing, x.y isn't a prefix of a.b.c
-- stripNamePrefix "" a.b.c = undefined, "" isn't a valid name IMO
-- stripNamePrefix . .Nat = Just Nat
stripNamePrefix :: Name -> Name -> Maybe Name
stripNamePrefix prefix@Name{} name@Name{} =
  Name <$> Text.stripPrefix (toText prefix <> mid) (toText name)
  where
  mid = if toText prefix == "." then "" else "."
stripNamePrefix n1@(Name' p1 prefix) n2@(Name' p2 name) =
  if p1 == p2 then
    fmap (Name' Relative) do
      suffix <- List.stripPrefix (toList prefix) (toList name)
      NonEmpty.nonEmpty suffix
  else
    error ("stripNamePrefix (" ++ show n1 ++ ") (" ++ show n2 ++ ")")

joinDot :: Name -> Name -> Name
joinDot prefix@Name{} suffix@Name{} =
  if toText prefix == "." then Name (toText prefix <> toText suffix)
  else Name (toText prefix <> "." <> toText suffix)
joinDot n1@(Name' p1 prefix) n2@(Name' p2 suffix) =
  case p2 of
    Absolute -> error ("joinDot (" ++ show n1 ++ ") (" ++ show n2 ++ ")")
    Relative -> Name' p1 (prefix <> suffix)

unqualified :: Name -> Name
unqualified (Name name) = unsafeFromText (last (Text.splitOn "." name))
unqualified (Name' _ names) = Name' Relative (NonEmpty.last names :| [])

-- parent . -> Nothing
-- parent + -> Nothing
-- parent foo -> Nothing
-- parent foo.bar -> foo
-- parent foo.bar.+ -> foo.bar
parent :: Name -> Maybe Name
parent (Name txt) = case unsnoc (Text.splitOn "." txt) of
  Nothing -> Nothing
  Just ([],_) -> Nothing
  Just (init,_) -> Just $ Name (Text.intercalate "." init)
parent (Name' p names) =
  Name' p <$> NonEmpty.nonEmpty (NonEmpty.init names)

-- | Return the relative suffixes of a name.
--
-- @
-- suffixes .foo.bar = [foo.bar, bar]
-- suffixes  foo.bar = [foo.bar, bar]
-- @
suffixes :: Name -> [Name]
suffixes (Name n) =
  fmap up . tails . dropWhile (== "") $ Text.splitOn "." n
  where
  up ns = Name (Text.intercalate "." ns)
suffixes (Name' _ names) =
  map (Name' Relative . NonEmpty.fromList) (NonEmpty.init (NonEmpty.tails names))

makeAbsolute :: Name -> Name
makeAbsolute n | toText n == "." = Name ".."
               | isAbsolute n    = n
               | otherwise       = Name ("." <> toText n)

countDots :: Name -> Int
countDots = Text.count "." . Text.dropEnd 1 . toText

segments :: Name -> [NameSegment]
segments (Name name) = fmap NameSegment.unsafeFromText (Text.splitOn "." name)

isLower :: Name -> Bool
isLower = Text.all Char.isLower . Text.take 1 . toText

isAbsolute :: Name -> Bool
isAbsolute (Name name) = Text.isPrefixOf "." name && name /= "."

splits :: Name -> [([NameSegment], Name)]
splits =
  map (map NameSegment.unsafeFromText *** unsafeFromText) . oldSplits

-- > oldSplits "x" == [([], "x")]
-- > oldSplits "A.x" == [(["A"], "x")]
-- > oldSplits "A.B.x" == [(["A"], "B.x"), (["A.B"], "x")]
oldSplits :: Name -> [([Text], Text)]
oldSplits (Name n) = let ns = Text.splitOn "." n
                     in dropEnd 1 (inits ns `zip` (map dotConcat $ tails ns))
  where dotConcat = Text.concat . intersperse "."

-- | Drop the leading '.' from a name if it's an absolute name.
asRelative :: Name -> Name
asRelative name =
  if isAbsolute name then
    Name (Text.drop 1 (toText name))
  else
    name

instance H.Hashable Name where
  tokens s = [H.Text (toText s)]
