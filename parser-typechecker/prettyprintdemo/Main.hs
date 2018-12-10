{-# Language OverloadedStrings #-}

module Main where

import Data.String (fromString)
import Unison.Util.Pretty as PP
import qualified Data.Text as Text

main :: IO ()
main = do
  -- putStrLn . PP.toANSI 60 $ ex1
  putStrLn . PP.toANSI 60 $ examples
  where
  -- ex1 = PP.linesSpaced [PP.red "hi", PP.blue "blue"]
  examples = PP.linesSpaced . take 7 $ [
    -- Use `OverloadedStrings`, `lit`, and `text` to get values into `Pretty`
    "Hello, world!",
    PP.lit "Use `lit` to embed any `IsString`", -- works for any `IsString`
    PP.text ("I never want to call Text.unpack again" :: Text.Text),

    -- Use the `Monoid` and/or `Semigroup` to combine strings
    "Hello, " <> PP.red "world!",
    -- `wrapWords` does automatic line wrapping`
    PP.wrapWords loremIpsum,

    -- Indentation: got it covered  🎸
    -- by a # of spaces
    PP.indentN 2 (PP.wrapWords loremIpsum),
    -- or by any other `Pretty`
    PP.indent (PP.red ">> ") (PP.wrapWords loremIpsum),

    -- Some handy functions:
    PP.sep ", " (replicate 10 "a"),
    PP.lines ["Alice", "Bob", "Carol"],
    PP.bulleted ["Alice", "Bob", "Carol"],
    PP.dashed ["Alice", "Bob", "Carol"],
    PP.column2 [
      (PP.bold "Name", PP.bold "Favorite color"),
      ("Alice"       , PP.red "Red"),
      ("Bob"         , PP.blue "Blue"),
      ("Carolina"    , PP.green "Green"),
      ("Dave"        , PP.black "Black")
    ],
    PP.numbered (fromString . show) ["uno", "dos", "tres"],
    -- Feel free to start the numbering wherever you like
    PP.numbered (fromString . show . (10 +)) ["uno", "dos", "tres"]
    ]
  loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas sem nisi, venenatis viverra ex eu, tristique dapibus justo. Ut lobortis mattis rutrum. Vivamus mattis eros diam, a egestas mi venenatis vel. Nunc felis dui, consectetur ac volutpat vitae, molestie in augue. Cras nec aliquet ex. In et sem vel sapien auctor euismod. Pellentesque eu aliquam dolor. Cras porttitor mi velit, dapibus vulputate odio pharetra non. Etiam iaculis nulla eu nisl euismod ultricies."
