{-# LANGUAGE ScopedTypeVariables #-}
module Unison.Test.Util.Pretty where

import EasyTest
import qualified Unison.Util.Pretty as P

test :: Test ()
test =
  scope "util.pretty" . tests $
  [ scope "commas" $
    let
      actual = P.render 80 $ P.commas input
      expected = "1, 2, 3"
    in expectEqual actual expected
  , scope "commas.backticked" $
      let
        list = P.backticked . P.string . show <$> [1, 2, 3]
        actual = P.render 80 $ P.commas list
        expected = "`1`, `2`, `3`"
      in expectEqual actual expected
  , scope "group.commas.backticked" $
      let
        actual = P.render 80 $ P.group commas
        expected = "`1`, `2`, `3`"
      in expectEqual actual expected
  ]
  where 
    input = P.string . show <$> [1, 2, 3]
    commas = P.commas (P.backticked . P.string . show <$> [1, 2, 3])