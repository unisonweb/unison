{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Term where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Hash (Hash)
import Unison.Node.MemNode ()
import Unison.Parsers (unsafeParseTerm)
import Unison.Reference as R
import Unison.Symbol (Symbol)
import Unison.Term
import Unison.View (DFO)
import Unison.Dimensions (Width(..),Height(..),Region,X(..),Y(..))
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Unison.ABT as ABT
import qualified Unison.Doc as Doc
import qualified Unison.Paths as Paths
import qualified Unison.Parser as Parser
import qualified Unison.TermParser as TermParser
import qualified Unison.Test.Common as Common
import qualified Unison.Var as Var
import qualified Unison.Views as Views
import Debug.Trace

-- term for testing
type TTerm = Term (Symbol DFO)

hash :: TTerm -> Hash
hash = ABT.hash

atPts :: Bool -> Common.TNode -> [(Int,Int)] -> TTerm -> [(Paths.Path, Region)]
atPts print (_,symbol) pts t = map go pts where
  go (x,y) = let p = path x y in (p, Doc.region bounds p)
  doc = Views.term symbol t
  layout = Doc.layout Doc.textWidth (Width 80) doc
  bounds = debug $ Doc.bounds (\t -> (Doc.textWidth t, Height 1)) (Doc.box layout)
  path x y = Doc.at bounds (X (fromIntegral x), Y (fromIntegral y))
  debug b = if print then trace ("\n" ++ Doc.debugDoc doc ++ "\n\n" ++ Doc.debugBox b ++ "\n\n" ++ Doc.debugBoxp b) b else b

tests :: TestTree
tests = withResource Common.node (\_ -> pure ()) $ \node -> testGroup "Term"
    [ testCase "alpha equivalence (term)" $ assertEqual "identity"
       (unsafeParseTerm "a -> a")
       (unsafeParseTerm "x -> x")
    , testCase "hash cycles" $ assertEqual "pingpong"
       (hash pingpong1)
       (hash pingpong2)
    , testCase "infix-rendering (1)" $ node >>= \(_,symbol) ->
        let t = unsafeParseTerm "Number.plus 1 1"
        in assertEqual "+"
          "1 + 1"
          (Doc.formatText (Width 80) (Views.term symbol t))
    , testCase "infix-rendering (unsaturated)" $ node >>= \(_,symbol) ->
        let t = unsafeParseTerm "Number.plus _"
        in assertEqual "+"
          "(+) _"
          (Doc.formatText (Width 80) (Views.term symbol t))
    , testCase "infix-rendering (totally unsaturated)" $ node >>= \(_,symbol) ->
        let t = unsafeParseTerm "Number.plus"
        in assertEqual "+" "(+)" (Doc.formatText (Width 80) (Views.term symbol t))
    , testCase "infix-rendering (2)" $ node >>= \(_,symbol) ->
        do
          t <- pure $ unsafeParseTerm "Number.plus 1 1"
          let d = Views.term symbol t
          assertEqual "path sanity check"
             [Paths.Fn,Paths.Arg]
             (head $ Doc.leafPaths d)
    , testCase "let-rendering (1)" $ node >>= \node ->
        do
          -- let xy = 4223 in 42
          t <- pure $ unsafeParseTerm "let xy = 4223 in 42"
          [(p1,r1), (p2,_), (p3,r3), (p4,_), (p5,r5), (p6,r6)] <- pure $
            atPts False node [(0,0), (1,0), (10,0), (11,0), (5,0), (8,0)] t
          assertEqual "p1" [] p1
          assertEqual "p2" [] p2
          assertEqual "r1" (rect 0 0 19 1) r1
          assertEqual "p3" [Paths.Binding 0, Paths.Body] p3
          assertEqual "r3" (rect 9 0 4 1) r3
          assertEqual "p3 == p4" p3 p4
          assertEqual "p5" [Paths.Binding 0, Paths.Bound] p5
          assertEqual "r5" (rect 4 0 2 1) r5
          assertEqual "p6" [Paths.Binding 0] p6
          assertEqual "r6" (rect 4 0 9 1) r6
    , testCase "map lambda rendering" $ node >>= \node ->
        do
          -- map (x -> _) [1,2,3]
          t <- pure $ builtin "Vector.map" `app` lam' ["x"] blank `app` vector (map num [1,2,3])
          [(p1,r1)] <- pure $ atPts False node [(5,0)] t
          assertEqual "p1" [Paths.Fn, Paths.Arg] p1
          assertEqual "r1" (rect 4 0 8 1) r1
    , testCase "operator chain rendering" $ node >>= \node ->
        do
          t <- pure $ unsafeParseTerm "1 `Number.plus` 2 `Number.plus` 3"
          [(p1,r1),(p2,_)] <- pure $ atPts False node [(1,0), (2,0)] t
          assertEqual "p1" [Paths.Fn, Paths.Arg, Paths.Fn, Paths.Arg] p1
          assertEqual "r1" (rect 0 0 1 1) r1
          assertEqual "p2" [] p2
    ]

rect :: Int -> Int -> Int -> Int -> (X,Y,Width,Height)
rect x y w h =
  (X (fromIntegral x), Y (fromIntegral y), Width (fromIntegral w), Height (fromIntegral h))

-- various unison terms, useful for testing
pingpong1 :: TTerm
pingpong1 =
  unsafeParseTerm $
    unlines [ "let rec ping = x -> pong (x + 1)"
            , "      ; pong = y -> ping (y - 1)"
            , "  in ping 1"
            ]

pingpong2 :: TTerm
pingpong2 =
  unsafeParseTerm $
    unlines [ "let rec pong1 = p -> ping1 (p - 1)"
            , "      ; ping1 = q -> pong1 (q + 1)"
            , "  in ping1 1"
            ]
