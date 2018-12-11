{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}

module Main where

import Data.String (fromString)
import Unison.Util.Pretty as PP
import Data.Text (Text)

main :: IO ()
main = do
  -- putStrLn . PP.toANSI 60 $ ex1
  -- print $ examples
  putStrLn . PP.toANSI 25 $ examples
  where
  -- ex1 = PP.linesSpaced [PP.red "hi", PP.blue "blue"]
  examples = PP.linesSpaced [
    PP.bold "Creating `Pretty`s",

    "Use `OverloadedStrings`, `lit`, and `text` to get values into `Pretty`",
    "Here's an overloaded string",
    PP.lit "Here's a call to `lit`", -- works for any `IsString`
    PP.text ("No need to Text.unpack, just `PP.text` directly" :: Text),

    PP.bold "Use the `Monoid` and/or `Semigroup` to combine strings",
    "Hello, " <> PP.red "world!",

    PP.yellow "`wrapWords` does automatic line wrapping",
    PP.wrapWords loremIpsum,

    PP.bold "Indentation: can indent by n spaces, or by another `Pretty`",
    PP.indentN 2 (PP.wrapWords loremIpsum),
    PP.indent (PP.red (fromString ">> ")) (PP.wrapWords loremIpsum),

    PP.bold "Other handy functions",

    PP.bulleted [
      PP.sep ", " (replicate 10 "a"),
      PP.lines ["Alice", "Bob", "Carol"],
      PP.blue "foo bar baz"
    ],

    PP.indentN 4 $ PP.bulleted ["Alice", "Bob", "Carol"],
    PP.dashed ["Alice", "Bob", "Carol"],
    PP.column2 [
      (PP.bold "Name", PP.bold "Favorite color"),
      ("Alice"       , PP.red "Red"),
      ("Bob"         , PP.blue "Blue"),
      ("Carolina"    , PP.green "Green"),
      ("Dave"        , PP.black "Black")
    ],
    PP.numbered (fromString . show) [
      "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"],
    -- Feel free to start the numbering wherever you like
    PP.numbered (fromString . show . (10 +)) ["uno", "dos", "tres"]

    ]
  loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
  -- loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas sem nisi, venenatis viverra ex eu, tristique dapibus justo. Ut lobortis mattis rutrum. Vivamus mattis eros diam, a egestas mi venenatis vel. Nunc felis dui, consectetur ac volutpat vitae, molestie in augue. Cras nec aliquet ex. In et sem vel sapien auctor euismod. Pellentesque eu aliquam dolor. Cras porttitor mi velit, dapibus vulputate odio pharetra non. Etiam iaculis nulla eu nisl euismod ultricies."
