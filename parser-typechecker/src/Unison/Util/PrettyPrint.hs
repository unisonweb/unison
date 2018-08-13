{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Util.PrettyPrint where


import qualified Data.ListLike      as LL
import           Data.String        (IsString, fromString)
import           Unison.Util.Monoid (intercalateMap)

data PrettyPrint a
  = Empty
  | Literal a
  | Append (PrettyPrint a) (PrettyPrint a)
  | Nest a (PrettyPrint a)
  | Breakable a
  | Group (PrettyPrint a)

unbrokenWidth :: LL.ListLike a b => PrettyPrint a -> Int
unbrokenWidth = \case
  Empty -> 0
  Literal a -> LL.length a
  Append a b -> unbrokenWidth a + unbrokenWidth b
  Nest _prefix a -> unbrokenWidth a
  Breakable a -> LL.length a
  Group a -> unbrokenWidth a

renderUnbroken :: Monoid a => PrettyPrint a -> a
renderUnbroken = \case
  Empty -> mempty
  Literal a -> a
  Append a b -> renderUnbroken a <> renderUnbroken b
  Nest _prefix a -> renderUnbroken a
  Breakable delim -> delim
  Group a -> renderUnbroken a

renderBroken :: forall a b. (LL.ListLike a b, Eq b)
             => Int -> Bool -> b -> PrettyPrint a -> a
renderBroken width beginLine lineSeparator = \case
  Empty -> LL.empty
  Literal a -> a
  Append a b ->
    let ra = renderBroken width beginLine lineSeparator a
        trailing = lengthOfLastLine lineSeparator ra
    in ra <> renderBroken (width - trailing) (trailing == 0) lineSeparator b
  Nest prefix a ->
    if beginLine
    then
      let ra = renderBroken (width - LL.length prefix) False lineSeparator a
      in prefix <> replaceOneWithMany lineSeparator (LL.cons lineSeparator prefix) ra
    else renderBroken width False lineSeparator a
  Breakable _delim -> LL.singleton lineSeparator
  Group a -> render' width lineSeparator a

  where
    replaceOneWithMany :: (LL.FoldableLL a b, Eq b) => b -> a -> a -> a
    replaceOneWithMany target replacement list =
      LL.foldl (go target replacement) LL.empty list
        where go :: (LL.FoldableLL a b, Eq b) => b -> a -> a -> b -> a
              go target replacement a b =
                if b == target then LL.append a replacement else a

    lengthOfLastLine :: (LL.ListLike a b, Eq b) => b -> a -> Int
    lengthOfLastLine lineSeparator ra =
      LL.length ra + 1 - case LL.reverse $ LL.findIndices (==lineSeparator) ra of
        []    -> -1
        h : _ -> h

render :: LL.ListLike a Char => Int -> PrettyPrint a -> a
render width doc = render' width '\n' doc

render' :: (LL.ListLike a b, Eq b) => Int -> b -> PrettyPrint a -> a
render' width lineSeparator doc =
  if unbrokenWidth doc <= width
  then renderUnbroken doc
  else renderBroken width False lineSeparator doc


softbreak :: IsString a => PrettyPrint a
softbreak = Breakable " "

semicolon :: IsString a => PrettyPrint a
semicolon = Breakable "; "

comma :: IsString a => PrettyPrint a
comma = Breakable ", "

softbreaks :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
softbreaks = intercalateMap softbreak id

semicolons :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
semicolons = intercalateMap semicolon id

commas :: (Foldable f, IsString a) => f (PrettyPrint a) -> PrettyPrint a
commas = intercalateMap comma id

instance Semigroup (PrettyPrint a) where
  (<>) = mappend

instance Monoid (PrettyPrint a) where
  mempty = Empty
  mappend a b = Append a b

instance IsString a => IsString (PrettyPrint a) where
  fromString = Literal . fromString
