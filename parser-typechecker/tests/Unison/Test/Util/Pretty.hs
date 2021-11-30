{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Test.Util.Pretty
  ( test
  ) where

import Control.Monad
import Data.String (fromString)
import EasyTest
import qualified Unison.Util.Pretty as Pretty

test :: Test ()
test =
  scope "util.pretty" . tests $ [
    scope "Delta.Semigroup.<>.associative" $ do
      replicateM_ 100 $ do
        d1 <- randomDelta
        d2 <- randomDelta
        d3 <- randomDelta
        expect' $ (d1 <> d2) <> d3 == d1 <> (d2 <> d3)
      ok
  ]

randomDelta :: Test Pretty.Delta
randomDelta =
  Pretty.delta <$> randomPretty

  where
    randomPretty :: Test (Pretty.Pretty String)
    randomPretty =
      fromString <$> randomString

    randomString :: Test String
    randomString =
      replicateM 3 (pick ['x', 'y', 'z', '\n'])
