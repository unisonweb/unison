{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}
module JSON.Diff.Printer where
import Data.Aeson.Extra
import Data.String (fromString)
import Data.Text
import qualified Data.Functor.Foldable as FF
import Data.Aeson
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.HashMap.Lazy as HM
import qualified Unison.Util.Pretty as P
import JSON.Diff
import Text.Pretty.Simple
import qualified Control.Monad.Free as Free
import Control.Monad.Free (Free (Free))
import qualified Data.Semialign as Zip
import Data.These
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified System.Random as Random

intercalateMap :: (Monoid m, Foldable f) => m -> (a -> m) -> f a -> m
intercalateMap m f fa =
  Foldable.fold
    . List.intersperse m
    . fmap f
    . Foldable.toList
    $ fa

added :: P.Pretty P.ColorText -> P.Pretty P.ColorText
added = P.green

deleted :: P.Pretty P.ColorText -> P.Pretty P.ColorText
deleted = P.red

unchanged :: P.Pretty P.ColorText -> P.Pretty P.ColorText
unchanged = P.purple

prettyPrintJsonF :: (P.Pretty P.ColorText -> P.Pretty P.ColorText) -> ValueF (P.Pretty P.ColorText) -> P.Pretty P.ColorText
prettyPrintJsonF mod = \case
  ObjectF hm ->
    let inner = HM.mapWithKey (,) hm
    in mod "{ "<> P.indent "  " (P.commas $ fmap (\(k, v) -> mod ("\"" <> P.text k <> "\"" <> ": ") <> v) inner) <> mod "\n}"
  ArrayF vec ->
    mod "[ " <> P.indent "  " (added (P.commas vec)) <> mod "]"
  StringF txt -> mod ("\"" <> P.text txt <> "\"")
  NumberF sci -> mod $ P.shown sci
  BoolF b -> mod $ fromString $ show b
  NullF -> mod $ "null"

prettyPrintJson :: (P.Pretty P.ColorText -> P.Pretty P.ColorText) -> Value -> P.Pretty P.ColorText
prettyPrintJson mod = FF.cata (prettyPrintJsonF mod)

src :: Value
src =
  Array
    [ "Hello, World",
      Object
        [ ("a" :: Text) .= Array [Number 1, Number 2, Null],
          ("b" :: Text) .= Array [Number 3, Number 4, "Hi!"]
        ]
    ]


dest :: Value
dest =
  Array
    [ "Hello, World",
      Object
        [ ("a" :: Text) .= Array [Number 1, Number 2, Null],
          ("c" :: Text) .= Array [Number 5, Number 4]
        ]
    ]


debug :: IO ()
debug = pPrint $ computePatch src dest


prettyPrintDiff :: Value -> Value -> P.Pretty P.ColorText
prettyPrintDiff src dest = Free.iter (prettyPrintJsonF unchanged) $ fmap (prettyPrintChange bindings) patch
  where
    patch :: Patch ValueF Hash
    patch = computePatch src dest
    -- TODO: replace this with something more efficient
    bindings :: Map Hash Value
    bindings = allBindings src <> allBindings dest

pickColor :: Hash -> P.Pretty P.ColorText -> P.Pretty P.ColorText
pickColor (Hash n) = 
  let (i, _) = Random.randomR (0, Vector.length hashColors - 1) (Random.mkStdGen n)
   in hashColors Vector.! i

hashColors :: Vector.Vector (P.Pretty P.ColorText -> P.Pretty P.ColorText)
hashColors =
  Vector.fromList
    [ P.yellow,
      P.blue,
      P.purple,
      P.cyan,
      P.hiYellow,
      P.hiBlue,
      P.hiPurple,
      P.hiCyan
    ]


prettyPrintChange :: Map Hash Value -> Change ValueF Hash -> P.Pretty P.ColorText
prettyPrintChange bindings Change{deletions,insertions} = go unchanged deletions insertions
  where
    renderHash :: Hash -> P.Pretty P.ColorText
    renderHash k = prettyPrintJson (pickColor k) $ bindings Map.! k
    alignSpines :: (P.Pretty P.ColorText -> P.Pretty P.ColorText) -> These (Free ValueF Hash) (Free ValueF Hash) -> P.Pretty P.ColorText
    alignSpines mod (These a b) = go mod a b
    alignSpines _mod (This a) = Free.iter (prettyPrintJsonF deleted) (fmap renderHash a)
    alignSpines _mod (That b) = Free.iter (prettyPrintJsonF deleted) (fmap renderHash b)
    renderFree :: (P.Pretty P.ColorText -> P.Pretty P.ColorText) -> Free ValueF Hash -> P.Pretty P.ColorText
    renderFree mod fv = Free.iter (prettyPrintJsonF mod) (fmap renderHash fv)
    go :: (P.Pretty P.ColorText -> P.Pretty P.ColorText)
       -> Free ValueF Hash
       -> Free ValueF Hash
       -> P.Pretty P.ColorText
    go mod a b = case (a, b) of
      (Free (ObjectF o), Free (ObjectF o')) -> prettyPrintJsonF mod (ObjectF . HM.mapWithKey (\k p -> mod (P.text k <> ": ") <> p) $ Zip.alignWith (alignSpines mod) o o')
      (Free (ArrayF a), Free (ArrayF a'))   ->  prettyPrintJsonF mod (ArrayF $ Zip.alignWith (alignSpines mod) a a')
      (Free (StringF s), Free (StringF s')) | s == s' -> mod $ P.shown s
      (Free (NumberF n), Free (NumberF n')) | n == n' -> mod $ P.shown n
      (Free (BoolF b), Free (BoolF b'))     | b == b' -> mod $ P.shown b
      (Free NullF, Free NullF) -> unchanged "null"
      (l, r) -> renderFree deleted l <> P.newline <> renderFree added r

