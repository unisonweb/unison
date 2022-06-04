module Unison.Prelude.Text
  ( fromInt,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyText.Builder
import qualified Data.Text.Lazy.Builder.Int as LazyText.Builder

-- | Render an Int as Text.
fromInt :: Int -> Text
fromInt =
  LazyText.toStrict . LazyText.Builder.toLazyText . LazyText.Builder.decimal
