module Main where
import EasyTest
import System.Random (Random)
import qualified Unison.Util.Relation as Relation
import Unison.Util.Relation3 (Relation3)
import qualified Unison.Util.Relation3 as Relation3
import Unison.Util.Relation4 (Relation4)
import qualified Unison.Util.Relation4 as Relation4

main :: IO ()
main =
  run do
    scope "Relation3" do
      scope "d12 works" do
        r3 <- randomR3 @Char @Char @Char 1000
        let d12 = Relation.fromList . map (\(a, b, _) -> (a, b)) . Relation3.toList
        expectEqual (Relation3.d12 r3) (d12 r3)

      scope "d13 works" do
        r3 <- randomR3 @Char @Char @Char 1000
        let d13 = Relation.fromList . map (\(a, _, c) -> (a, c)) . Relation3.toList
        expectEqual (Relation3.d13 r3) (d13 r3)

    scope "Relation4" do
      scope "d124 works" do
        r4 <- randomR4 @Char @Char @Char @Char 1000
        let d124 = Relation3.fromList . map (\(a, b, _, d) -> (a, b, d)) . Relation4.toList
        expectEqual (Relation4.d124 r4) (d124 r4)

    scope "relation operations" do
      r1 <- randomR1 @Char @Char 1000
      r2 <- randomR1 @Char @Char 500
      scope "a `union` (a `intersection` b) == a" do
        expectEqual
          (r1 `Relation.union` (r1 `Relation.intersection` r2))
          r1
      scope "union a a == a" do
        expectEqual (Relation.union r1 r1) r1

      scope "intersection a a == a" do
        expectEqual (Relation.union r1 r1) r1

randomR1 ::  (Ord a, Ord b, Random a, Random b) =>  Int -> Test (Relation.Relation a b)
randomR1 n = Relation.fromList <$> listOf n tuple2

randomR3 :: (Ord a, Random a, Ord b, Random b, Ord c, Random c) => Int -> Test (Relation3 a b c)
randomR3 n =
  Relation3.fromList <$> listOf n tuple3

randomR4 :: (Ord a, Random a, Ord b, Random b, Ord c, Random c, Ord d, Random d) => Int -> Test (Relation4 a b c d)
randomR4 n =
  Relation4.fromList <$> listOf n tuple4
